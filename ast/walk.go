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
		*ArrayTraversal, *Param:
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

func walkExprs(visitor func(Expression) bool, exprs ...Expression) bool {
	for _, expr := range exprs {
		if !Walk(expr, visitor) {
			return false
		}
	}
	return true
}
