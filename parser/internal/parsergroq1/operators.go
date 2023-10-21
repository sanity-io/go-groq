package parsergroq1

import (
	"github.com/sanity-io/go-groq/ast"
)

func isPrefixOperator(token ast.Token) bool {
	return token == ast.Not ||
		token == ast.DotDotDot ||
		token == ast.Plus ||
		token == ast.Minus
}

func isInfixOperator(token ast.Token) bool {
	return token == ast.Equals ||
		token == ast.Or ||
		token == ast.And ||
		token == ast.GT ||
		token == ast.LT ||
		token == ast.GTE ||
		token == ast.LTE ||
		token == ast.NEQ ||
		token == ast.Colon ||
		token == ast.Dot ||
		token == ast.DotDot ||
		token == ast.MatchOperator ||
		token == ast.DotDotDot ||
		token == ast.InOperator ||
		token == ast.Pipe ||
		token == ast.Rocket ||
		token == ast.Plus ||
		token == ast.Minus ||
		token == ast.Asterisk ||
		token == ast.Slash ||
		token == ast.Exponentiation ||
		token == ast.Percent ||
		token == ast.DoubleColon
}

func isPostfixOperator(token ast.Token) bool {
	return (token == ast.AscOperator || token == ast.DescOperator || token == ast.Arrow)
}

func isOperator(token ast.Token) bool {
	return isInfixOperator(token) || isPrefixOperator(token) || isPostfixOperator(token)
}

func identIsOperator(ident string) bool {
	_, ok := identToOperator(ident)
	return ok
}

func identToOperator(ident string) (ast.Token, bool) {
	switch ident {
	case ast.AscOperator.Literal():
		return ast.AscOperator, true
	case ast.DescOperator.Literal():
		return ast.DescOperator, true
	case ast.InOperator.Literal():
		return ast.InOperator, true
	case ast.MatchOperator.Literal():
		return ast.MatchOperator, true
	}
	return ast.Illegal, false
}

func asRange(e ast.Expression) *ast.Range {
	switch e := e.(type) {
	case *ast.Range:
		return e
	case *ast.BinaryOperator:
		if e.Operator == ast.DotDot || e.Operator == ast.DotDotDot {
			return &ast.Range{
				Start:     e.LHS,
				End:       e.RHS,
				Pos:       e.Pos,
				Inclusive: e.Operator == ast.DotDot,
			}
		}
	}
	return nil
}

func isRange(e ast.Expression) bool {
	switch e := e.(type) {
	case *ast.Range:
		return true
	case *ast.BinaryOperator:
		return e.Operator == ast.DotDot || e.Operator == ast.DotDotDot
	default:
		return false
	}
}
