package ast

import (
	"fmt"
)

// Contains is like Walk(), except its return value is the reverse: If the
// walk visitor returns true, walking is aborted, and the function returns true.
func Contains(expr Expression, visitor func(Expression) bool) bool {
	if expr == nil {
		return false
	}
	return !Walk(expr, func(e Expression) bool {
		return !visitor(e)
	})
}

// Walk walks an expression top-down, i.e. the outer struct first. If
// the visitor returns false, the walking is halted.
func Walk(expr Expression, visitor func(Expression) bool) bool {
	if expr == nil {
		return true
	}

	if !visitor(expr) {
		return false
	}

	switch e := expr.(type) {
	case *Everything, *This, *Ellipsis, *Parent, *StringLiteral,
		*FloatLiteral, *BooleanLiteral, *IntegerLiteral, *NullLiteral, *Attribute,
		*ArrayTraversal, *Param, *FunctionParam:
		return true
	case *Constraint:
		return Walk(e.Expression, visitor)
	case *Subscript:
		return Walk(e.Value, visitor)
	case *Range:
		return walkExprs(visitor, e.Start, e.End)
	case *BinaryOperator:
		return walkExprs(visitor, e.LHS, e.RHS)
	case *DotOperator:
		return walkExprs(visitor, e.LHS, e.RHS)
	case *Group:
		return Walk(e.Expression, visitor)
	case *PipeOperator:
		return walkExprs(visitor, e.LHS, e.RHS)
	case *PrefixOperator:
		return Walk(e.RHS, visitor)
	case *PostfixOperator:
		return Walk(e.LHS, visitor)
	case *Object:
		return walkExprs(visitor, e.Expressions...)
	case *Array:
		return walkExprs(visitor, e.Expressions...)
	case *FunctionCall:
		return walkExprs(visitor, e.Arguments...)
	case *FunctionPipe:
		return walkExprs(visitor, e.LHS, e.Func)
	case *Filter:
		return walkExprs(visitor, e.LHS, e.Constraint)
	case *Slice:
		return walkExprs(visitor, e.LHS, e.Range)
	case *Element:
		return walkExprs(visitor, e.LHS, e.Idx)
	case *Projection:
		return walkExprs(visitor, e.LHS, e.Object)
	case *Tuple:
		return walkExprs(visitor, e.Members...)
	}

	panic(fmt.Sprintf("Unexpected AST expression of type %T: %[1]v", expr))
}

// WalkAndFindParentUsageInvalid is like Walk() but it returns an error if an invalid
// parent usage is found.
// This function is exclusively used to walk the body of a custom defined function.
// The usage of a parent operator is *not* allowed in the body of a custom defined function
// if it reaches out of that function's scope. It's valid within a filter or a projection, because
// we create NewNestedScope for those.
func WalkAndFindParentUsageInvalid(count int, expr Expression) (int, error) {
	if expr == nil {
		return count, nil
	}
	switch e := expr.(type) {
	case *Parent:
		count--
		if count <= 0 {
			return count, fmt.Errorf("Parent usage is invalid")
		}
		return count, nil
	case *Everything, *This, *Ellipsis, *StringLiteral,
		*FloatLiteral, *BooleanLiteral, *IntegerLiteral, *NullLiteral, *Attribute,
		*ArrayTraversal, *Param, *FunctionParam:
		return count, nil
	case *Constraint:
		return WalkAndFindParentUsageInvalid(count, e.Expression)
	case *Subscript:
		return WalkAndFindParentUsageInvalid(count, e.Value)
	case *Range:
		if c, err := WalkAndFindParentUsageInvalid(count, e.Start); err != nil {
			return c, err
		}
		return WalkAndFindParentUsageInvalid(count, e.End)
	case *BinaryOperator:
		if c, err := WalkAndFindParentUsageInvalid(count, e.LHS); err != nil {
			return c, err
		}
		return WalkAndFindParentUsageInvalid(count, e.RHS)
	case *DotOperator:
		// Chain the count between LHS and RHS sides of the dot operator.
		c, err := WalkAndFindParentUsageInvalid(count, e.LHS)
		if err != nil {
			return c, err
		}
		return WalkAndFindParentUsageInvalid(c, e.RHS)
	case *Group:
		return WalkAndFindParentUsageInvalid(count, e.Expression)
	case *PipeOperator:
		if _, err := WalkAndFindParentUsageInvalid(count, e.LHS); err != nil {
			return count, err
		}
		return WalkAndFindParentUsageInvalid(count, e.RHS)
	case *PrefixOperator:
		return WalkAndFindParentUsageInvalid(count, e.RHS)
	case *PostfixOperator:
		return WalkAndFindParentUsageInvalid(count, e.LHS)
	case *Array:
	case *Object:
		for _, expr := range e.Expressions {
			if c, err := WalkAndFindParentUsageInvalid(count, expr); err != nil {
				return c, err
			}
		}
		return count, nil
	case *FunctionCall:
		for _, arg := range e.Arguments {
			if c, err := WalkAndFindParentUsageInvalid(count, arg); err != nil {
				return c, err
			}
		}
		return count, nil
	case *FunctionPipe:
		if c, err := WalkAndFindParentUsageInvalid(count, e.LHS); err != nil {
			return c, err
		}
		return WalkAndFindParentUsageInvalid(count, e.Func)
	case *Filter:
		if c, err := WalkAndFindParentUsageInvalid(count, e.LHS); err != nil {
			return c, err
		}

		return WalkAndFindParentUsageInvalid(count+1, e.Constraint)
	case *Slice:
		if c, err := WalkAndFindParentUsageInvalid(count, e.LHS); err != nil {
			return c, err
		}
		return WalkAndFindParentUsageInvalid(count, e.Range)
	case *Element:
		if c, err := WalkAndFindParentUsageInvalid(count, e.LHS); err != nil {
			return c, err
		}
		return WalkAndFindParentUsageInvalid(count, e.Idx)
	case *Projection:
		if c, err := WalkAndFindParentUsageInvalid(count, e.LHS); err != nil {
			return c, err
		}
		return WalkAndFindParentUsageInvalid(count+1, e.Object)
	case *Tuple:
		for _, member := range e.Members {
			if c, err := WalkAndFindParentUsageInvalid(count, member); err != nil {
				return c, err
			}
		}
		return count, nil
	}

	panic(fmt.Sprintf("Unexpected AST expression of type %T: %[1]v", expr))
}

func walkExprs(visitor func(Expression) bool, exprs ...Expression) bool {
	for _, expr := range exprs {
		if !Walk(expr, visitor) {
			return false
		}
	}
	return true
}
