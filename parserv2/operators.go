package parserv2

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

const highestPrecedence = 12

func precedenceAndAssociativity(token ast.Token) (precedence int, associativity int) {
	switch token {
	case ast.Dot:
		return highestPrecedence, associatesLeft
	case ast.Pipe:
		return highestPrecedence, associatesLeft

	case ast.Arrow:
		return 11, associatesLeft
	case ast.AscOperator:
		return 11, associatesLeft
	case ast.DescOperator:
		return 11, associatesLeft

	case ast.Exponentiation:
		return 10, associatesRight

	case ast.Asterisk:
		return 9, associatesLeft
	case ast.Slash:
		return 9, associatesLeft
	case ast.Percent:
		return 9, associatesLeft

	// Gap at 8 to accomodate some in-parser precedence for function calls

	case ast.Not:
		return 7, associatesRight
	case ast.Plus:
		return 7, associatesLeft
	case ast.Minus:
		return 7, associatesLeft

	case ast.DotDot:
		return 6, associatesLeft
	case ast.DotDotDot:
		return 6, associatesLeft

	case ast.MatchOperator:
		return 5, associatesLeft
	case ast.Equals:
		return 5, associatesLeft
	case ast.GT:
		return 5, associatesLeft
	case ast.GTE:
		return 5, associatesLeft
	case ast.InOperator:
		return 5, associatesLeft
	case ast.LT:
		return 5, associatesLeft
	case ast.LTE:
		return 5, associatesLeft
	case ast.NEQ:
		return 5, associatesLeft

	case ast.And:
		return 4, associatesLeft

	case ast.Or:
		return 3, associatesLeft

	case ast.Rocket:
		return 2, associatesLeft

	case ast.Colon:
		return 1, associatesLeft
	case ast.Comma:
		return 1, associatesLeft
	case ast.DoubleColon:
		return 1, associatesLeft
	}
	return -1, -1
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
