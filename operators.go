package groq

import "github.com/sanity-io/go-groq/ast"

const HighestPrecedence = 12

type Associativity int

const (
	AssociatesLeft Associativity = iota
	AssociatesRight
)

func PrecedenceAndAssociativity(token ast.Token) (precedence int, associativity Associativity) {
	switch token {
	case ast.Dot:
		return HighestPrecedence, AssociatesLeft
	case ast.Pipe:
		return HighestPrecedence, AssociatesLeft

	case ast.Arrow:
		return 11, AssociatesLeft
	case ast.AscOperator:
		return 11, AssociatesLeft
	case ast.DescOperator:
		return 11, AssociatesLeft

	case ast.Exponentiation:
		return 10, AssociatesRight

	case ast.Asterisk:
		return 9, AssociatesLeft
	case ast.Slash:
		return 9, AssociatesLeft
	case ast.Percent:
		return 9, AssociatesLeft

	// Gap at 8 to accomodate some in-parser precedence for function calls

	case ast.Not:
		return 7, AssociatesRight
	case ast.Plus:
		return 7, AssociatesLeft
	case ast.Minus:
		return 7, AssociatesLeft

	case ast.DotDot:
		return 6, AssociatesLeft
	case ast.DotDotDot:
		return 6, AssociatesLeft

	case ast.MatchOperator:
		return 5, AssociatesLeft
	case ast.Equals:
		return 5, AssociatesLeft
	case ast.EqualSign:
		return 5, AssociatesLeft
	case ast.GT:
		return 5, AssociatesLeft
	case ast.GTE:
		return 5, AssociatesLeft
	case ast.InOperator:
		return 5, AssociatesLeft
	case ast.LT:
		return 5, AssociatesLeft
	case ast.LTE:
		return 5, AssociatesLeft
	case ast.NEQ:
		return 5, AssociatesLeft

	case ast.And:
		return 4, AssociatesLeft

	case ast.Or:
		return 3, AssociatesLeft

	case ast.Rocket:
		return 2, AssociatesLeft

	case ast.Colon:
		return 1, AssociatesLeft
	case ast.Comma:
		return 1, AssociatesLeft
	case ast.DoubleColon:
		return 1, AssociatesLeft
	case ast.Semicolon:
		return 1, AssociatesLeft
	}
	return -1, -1
}
