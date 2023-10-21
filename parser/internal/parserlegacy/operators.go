package parserlegacy

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
		token == ast.Percent
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

// In lieu of an explicit pipe operator, the presence of one of these tokens inserts
// an implied pipe operator. as in `*[is "article"]` is parsed as `*|[is "article"]`
func entailsImplicitPipeOperator(token ast.Token) bool {
	return token == ast.BraceLeft || token == ast.BracketLeft
}

func interpretBinOpAsRange(binOp *ast.BinaryOperator) *ast.Range {
	if binOp.Operator != ast.DotDot && binOp.Operator != ast.DotDotDot {
		return nil
	}
	return &ast.Range{
		Start:     binOp.LHS,
		End:       binOp.RHS,
		Pos:       binOp.Pos,
		Inclusive: binOp.Operator == ast.DotDot,
	}
}
