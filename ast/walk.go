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

func ValidateParentAccess(expr Expression) error {
	_, err := walkAndFindParentUsageInvalid(0, expr)
	return err
}

// walkAndFindParentUsageInvalid is like Walk() but it returns an error if an invalid
// parent usage is found.
// This function is exclusively used to walk the body of a custom defined function.
// The usage of a parent operator is *not* allowed in the body of a custom defined function
// if it reaches out of that function's scope. It's valid within a filter or a projection, because
// we create NewNestedScope for those.
func walkAndFindParentUsageInvalid(count int, expr Expression) (int, error) {
	switch e := expr.(type) {
	case *Parent:
		if count <= 0 {
			return count, NewParseError("parent usage is invalid", e.Pos)
		}
		return count - 1, nil
	case *Everything, *This, *Ellipsis, *StringLiteral,
		*FloatLiteral, *BooleanLiteral, *IntegerLiteral, *NullLiteral, *Attribute,
		*ArrayTraversal, *Param, *FunctionParam:
		return count, nil
	case *Constraint:
		return walkAndFindParentUsageInvalid(count, e.Expression)
	case *Subscript:
		return walkAndFindParentUsageInvalid(count, e.Value)
	case *Range:
		if _, err := walkAndFindParentUsageInvalid(count, e.Start); err != nil {
			return 0, err
		}
		return walkAndFindParentUsageInvalid(count, e.End)
	case *BinaryOperator:
		if _, err := walkAndFindParentUsageInvalid(count, e.LHS); err != nil {
			return 0, err
		}
		return walkAndFindParentUsageInvalid(count, e.RHS)
	case *DotOperator:
		// Chain the count between LHS and RHS sides of the dot operator.
		c, err := walkAndFindParentUsageInvalid(count, e.LHS)
		if err != nil {
			return 0, err
		}
		return walkAndFindParentUsageInvalid(c, e.RHS)
	case *Group:
		return walkAndFindParentUsageInvalid(count, e.Expression)
	case *PipeOperator:
		if _, err := walkAndFindParentUsageInvalid(count, e.LHS); err != nil {
			return count, err
		}
		return walkAndFindParentUsageInvalid(count, e.RHS)
	case *PrefixOperator:
		return walkAndFindParentUsageInvalid(count, e.RHS)
	case *PostfixOperator:
		return walkAndFindParentUsageInvalid(count, e.LHS)
	case *Array:
		for _, expr := range e.Expressions {
			if _, err := walkAndFindParentUsageInvalid(count, expr); err != nil {
				return 0, err
			}
		}
		return count, nil
	case *Object:
		for _, expr := range e.Expressions {
			if _, err := walkAndFindParentUsageInvalid(count, expr); err != nil {
				return 0, err
			}
		}
		return count, nil
	case *FunctionCall:
		for _, arg := range e.Arguments {
			if _, err := walkAndFindParentUsageInvalid(count, arg); err != nil {
				return 0, err
			}
		}
		return count, nil
	case *FunctionPipe:
		if _, err := walkAndFindParentUsageInvalid(count, e.LHS); err != nil {
			return 0, err
		}
		if e.Func.Name == "order" {
			count = count + 1
		}
		return walkAndFindParentUsageInvalid(count, e.Func)
	case *Filter:
		if _, err := walkAndFindParentUsageInvalid(count, e.LHS); err != nil {
			return 0, err
		}

		return walkAndFindParentUsageInvalid(count+1, e.Constraint)
	case *Slice:
		if _, err := walkAndFindParentUsageInvalid(count, e.LHS); err != nil {
			return 0, err
		}
		return walkAndFindParentUsageInvalid(count, e.Range)
	case *Element:
		if _, err := walkAndFindParentUsageInvalid(count, e.LHS); err != nil {
			return 0, err
		}
		return walkAndFindParentUsageInvalid(count, e.Idx)
	case *Projection:
		if _, err := walkAndFindParentUsageInvalid(count, e.LHS); err != nil {
			return 0, err
		}
		return walkAndFindParentUsageInvalid(count+1, e.Object)
	case *Tuple:
		for _, member := range e.Members {
			if _, err := walkAndFindParentUsageInvalid(count, member); err != nil {
				return 0, err
			}
		}
		return count, nil
	}

	panic(fmt.Errorf("unexpected AST expression of type %T: %[1]v", expr))
}

func walkExprs(visitor func(Expression) bool, exprs ...Expression) bool {
	for _, expr := range exprs {
		if !Walk(expr, visitor) {
			return false
		}
	}
	return true
}
