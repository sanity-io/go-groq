package ast

import (
	"fmt"
	"reflect"
	"sort"
)

// AttributeAliasFromExpr will return "bar" for "bar" or "foo.bar". Returns
// false if the expression includes anything else, such as parent
// references or array subscripting.
func AttributeAliasFromExpr(expr Expression) (string, bool) {
	switch e := expr.(type) {
	case *DotOperator:
		if _, ok := AttributeAliasFromExpr(e.LHS); ok {
			return AttributeAliasFromExpr(e.RHS)
		}
	case *Attribute:
		return e.Name, true
	default:
		// OK
	}
	return "", false
}

func AsAttribute(expr Expression) (*Attribute, bool) {
	if a, ok := expr.(*Attribute); ok {
		return a, true
	}
	return nil, false
}

func AsStringLiteral(expr Expression) (string, bool) {
	if stringLiteral, ok := expr.(*StringLiteral); ok {
		return stringLiteral.Value, true
	}
	return "", false
}

func AsIntegerLiteral(expr Expression) (int, bool) {
	if intLiteral, ok := expr.(*IntegerLiteral); ok {
		return intLiteral.Value, true
	}
	// Params and certain other valuas can't be expressed as real int literals, and will appear
	// here as floats without fractions.
	if floatLiteral, ok := expr.(*FloatLiteral); ok {
		return int(floatLiteral.Value), true
	}
	return 0, false
}

func CombineAnd(lhs, rhs Expression) Expression {
	if lhs == nil {
		return rhs
	} else if rhs == nil {
		return lhs
	}
	return &BinaryOperator{
		Operator: And,
		LHS:      lhs,
		RHS:      rhs,
	}
}

func CombineOr(lhs, rhs Expression) Expression {
	if lhs == nil {
		return rhs
	} else if rhs == nil {
		return lhs
	}
	return &BinaryOperator{
		Operator: Or,
		LHS:      lhs,
		RHS:      rhs,
	}
}

// AreAllExpressions returns true if all expressions satisfy a function.
func AreAllExpressions(e []Expression, fn func(Expression) bool) bool {
	for _, elem := range e {
		if !fn(elem) {
			return false
		}
	}
	return true
}

func IsLiteral(e Expression) bool {
	_, ok := e.(Literal)
	return ok
}

// IsArithmeticOperator checks whether an operator is an arithmetic operator.
func IsArithmeticOperator(token Token) bool {
	return token == Plus ||
		token == Minus ||
		token == Asterisk ||
		token == Slash ||
		token == Exponentiation ||
		token == Percent
}

// IsSubscriptExpression checks whether an expression is a valid subscript expression.
// Since subscripts and filters share the same syntax, we limit subscripts to only
// contain numeric literals and the arithmetic operators.
func IsSubscriptExpression(e Expression) bool {
	if e == nil {
		return false
	}
	if r, ok := e.(*Range); ok {
		return IsSubscriptExpression(r.Start) && IsSubscriptExpression(r.End)
	}
	return Walk(e, func(e Expression) bool {
		switch t := e.(type) {
		case *IntegerLiteral:
			return true
		case *FloatLiteral:
			return true
		case *BinaryOperator:
			return IsArithmeticOperator(t.Operator)
		case *Group:
			return IsSubscriptExpression(t.Expression)
		case *PrefixOperator:
			switch t.Operator {
			case Plus, Minus:
				return true
			default:
				return false
			}
		default:
			return false
		}
	})
}

func IsAttribute(e Expression) bool {
	_, ok := e.(*Attribute)
	return ok
}

func InvertOperator(operator Token) Token {
	switch operator {
	case LT:
		return GT
	case LTE:
		return GTE
	case GT:
		return LT
	case GTE:
		return LTE
	}
	panic(fmt.Sprintf("Cannot switch operator %q", operator))
}

// LiteralFromInterface takes a value and returns a value wrapped in
// a Literal interface.
func LiteralFromInterface(value interface{}, pos Position) (Expression, error) {
	switch v := value.(type) {
	case string:
		return &StringLiteral{
			Value: v,
			Pos:   pos,
		}, nil
	case int:
		return &IntegerLiteral{
			Value: v,
			Pos:   pos,
		}, nil
	case int32:
		return &IntegerLiteral{
			Value: int(v),
			Pos:   pos,
		}, nil
	case int64:
		return &IntegerLiteral{
			Value: int(v),
			Pos:   pos,
		}, nil
	case float32:
		return &FloatLiteral{
			Value: float64(v),
			Pos:   pos,
		}, nil
	case float64:
		return &FloatLiteral{
			Value: v,
			Pos:   pos,
		}, nil
	case bool:
		return &BooleanLiteral{
			Value: v,
			Pos:   pos,
		}, nil
	case nil:
		return &NullLiteral{
			Pos: pos,
		}, nil
	}

	if slice, ok := intoInterfaceSlice(value); ok {
		exprs := make([]Expression, len(slice))
		for i, v := range slice {
			var err error
			exprs[i], err = LiteralFromInterface(v, pos)
			if err != nil {
				return nil, err
			}
		}
		return &Array{
			Pos:         pos,
			Expressions: exprs,
		}, nil
	}

	if stringMap, ok := intoInterfaceMap(value); ok {
		// Sort keys to make expansion consistent in tests
		keys := make([]string, 0, len(stringMap))
		for k := range stringMap {
			keys = append(keys, k)
		}
		sort.Strings(keys)

		exprs := make([]Expression, 0, len(keys))
		for _, k := range keys {
			v := stringMap[k]

			var err error
			literal, err := LiteralFromInterface(v, pos)
			if err != nil {
				return nil, err
			}
			exprs = append(exprs, &BinaryOperator{
				Operator: Colon,
				LHS: &StringLiteral{
					Pos:   pos,
					Value: k,
				},
				RHS: literal,
			})
		}
		return &Object{
			Pos:         pos,
			Expressions: exprs,
		}, nil
	}

	return nil, fmt.Errorf("cannot construct literal from interface value %#v", value)
}

// InterfaceFromLiteral returns a value from a Literal interface.
// Returns nil if the expression is not a literal.
func InterfaceFromLiteral(expr interface{}) interface{} {
	literal, ok := expr.(Literal)
	if !ok {
		return nil
	}
	return literal.LiteralValue()
}

func IsTrue(expr Expression) bool {
	b, ok := expr.(*BooleanLiteral)
	return ok && b.Value
}

func IsFalse(expr Expression) bool {
	b, ok := expr.(*BooleanLiteral)
	return ok && !b.Value
}

func HasArrayTraversal(expr Expression) bool {
	switch t := expr.(type) {
	case *Everything, *ArrayTraversal, *Slice, *Filter:
		return true
	case *Element:
		return HasArrayTraversal(t.LHS)
	case *Projection:
		return HasArrayTraversal(t.LHS)
	case *FunctionPipe:
		return HasArrayTraversal(t.LHS)
	case *PipeOperator:
		if HasArrayTraversal(t.LHS) {
			return true
		}
		if _, ok := t.RHS.(*ArrayTraversal); ok {
			return true
		}
	case *DotOperator:
		return HasArrayTraversal(t.LHS)
	case *PostfixOperator:
		return HasArrayTraversal(t.LHS)
	default:
		// For go-sumtype linter
	}
	return false
}

// intoInterfaceMap takes a string map and returns a neutral string map of interfaces.
// If the input value is not a string map, it returns false as the second return
// value. If the input value is already map[string]interface{}, the value is returned
// as-is.
func intoInterfaceMap(value interface{}) (map[string]interface{}, bool) {
	v := reflect.ValueOf(value)
	if v.Kind() != reflect.Map {
		return nil, false
	}

	if v.Type() == intfStringMapType {
		return v.Interface().(map[string]interface{}), true
	}

	length := v.Len()
	stringMap := make(map[string]interface{}, length)
	for _, key := range v.MapKeys() {
		if key.Kind() != reflect.String {
			return nil, false
		}
		stringMap[key.String()] = v.MapIndex(key).Interface()
	}
	return stringMap, true
}

var intfStringMapType = reflect.TypeOf(map[string]interface{}{})

// intoInterfaceSlice takes a slice and returns a neutral slice of interfaces.
// If the input value is not a slice, it returns false as the second return
// value. If the input value is already []interface{}, the value is returned
// as-is.
func intoInterfaceSlice(value interface{}) ([]interface{}, bool) {
	v := reflect.ValueOf(value)
	if v.Kind() != reflect.Slice {
		return nil, false
	}

	if v.Type() == intfSliceType {
		return v.Interface().([]interface{}), true
	}

	length := v.Len()
	slice := make([]interface{}, length)
	for i := 0; i < length; i++ {
		slice[i] = v.Index(i).Interface()
	}
	return slice, true
}

var intfSliceType = reflect.TypeOf([]interface{}{})
