package parserv1

import (
	"fmt"
	"strconv"

	"github.com/sanity-io/go-groq"
	"github.com/sanity-io/go-groq/ast"
	"github.com/sanity-io/go-groq/tokenizer"
)

type Option func(*parser)

func WithParams(p groq.Params) Option {
	return func(parser *parser) {
		for k, v := range p {
			parser.params[k] = v
		}
	}
}

func WithParamNodes(v bool) Option {
	return func(parser *parser) {
		parser.createParamNodes = v
	}
}

// parser represents a parser (!).
type parser struct {
	tk     *tokenizer.Tokenizer
	src    string
	params groq.Params
	buf    struct {
		tok ast.Token // last read token
		lit string    // last read literal
		pos int       // position of last token
		n   int       // buffer size (max=1)
	}
	createParamNodes bool
}

// Parse parses a string of GROQ.
func Parse(src string, opts ...Option) (ast.Expression, error) {
	return newParser(src, opts...).parse()
}

func newParser(src string, opts ...Option) *parser {
	p := &parser{tk: tokenizer.New(src), src: src, params: groq.Params{}}
	for _, o := range opts {
		o(p)
	}
	return p
}

// scan returns the next token from the underlying scanner.
// If a token has been unscanned then read that instead.
func (p *parser) scan() (tok ast.Token, lit string, pos int) {
	// If we have a token on the buffer, then return it.
	if p.buf.n != 0 {
		p.buf.n = 0
		return p.buf.tok, p.buf.lit, p.buf.pos
	}

	// Otherwise read the next token from the scanner.
	tok, lit, pos = p.tk.Scan()

	// Save it to the buffer in case we unscan later.
	p.buf.tok, p.buf.lit, p.buf.pos = tok, lit, pos

	return
}

// scanIgnoreWhitespace scans the next non-whitespace token.
func (p *parser) scanIgnoreWhitespace() (tok ast.Token, lit string, pos int) {
	tok, lit, pos = p.scan()
	for tok == ast.Whitespace {
		tok, lit, pos = p.scan()
	}
	return
}

// unscan pushes the previously read token back onto the buffer.
func (p *parser) unscan() { p.buf.n = 1 }

func (p *parser) dereferenceParam(name string, pos int) (ast.Expression, error) {
	if p.createParamNodes {
		return &ast.Param{
			Name: name,
			Pos:  p.makeTokenPos(pos, name),
		}, nil
	}

	value, exists := p.params[name]
	if !exists {
		return nil, ast.NewParseError(
			fmt.Sprintf("param $%s referenced, but not provided", name),
			p.makeTokenPos(pos, name))

	}
	expr, err := ast.LiteralFromInterface(value, p.makeTokenPos(pos, name))
	if err != nil {
		return nil, ast.NewParseError(
			fmt.Sprintf("error while accessing value of $%s: %s", name, err.Error()),
			p.makeTokenPos(pos, name))

	}
	return expr, nil
}

// parseAtom parses an atomic expression which is basically an expression that from the outside does not involve any operators.
// It is one of:
// * attribute name
// * param
// * a function call
// * an '*' or '^'
// * a literal integer, float, string or boolean value
// * a general expression or tuple enclosed in ()
// * an array descriptor (if immediateLhs is false)
// * a constraint, range or subscript filter (if immediateLhs is true)
// * an object descriptor
func (p *parser) parseAtom(immediateLHS bool) (ast.Expression, error) {
	tok, lit, pos := p.scanIgnoreWhitespace()
	switch tok {
	case ast.Name:
		// It is an identifier, if it starts with a '$' it's a param
		if lit[0] == groq.ParamPrefixCharacter {
			expr, err := p.dereferenceParam(lit[1:], pos)
			if err != nil {
				return nil, err
			}
			return expr, nil
		}
		// Check if it's a function call
		function, err := p.parseFunctionExpression(lit, pos)
		if err != nil {
			return nil, err
		}
		if function != nil {
			return function, nil
		}

		// Was not a function, so was an attribute
		return &ast.Attribute{
			Pos:  p.makeTokenPos(pos, lit),
			Name: lit,
		}, nil
	case ast.Asterisk:
		return &ast.Everything{
			Pos: p.makeTokenPos(pos, lit),
		}, nil
	case ast.At:
		return &ast.This{
			Pos: p.makeTokenPos(pos, lit),
		}, nil
	case ast.Hat:
		return &ast.Parent{
			Pos: p.makeTokenPos(pos, lit),
		}, nil
	case ast.Integer:
		val, _ := strconv.Atoi(lit)
		// TODO: Handle error
		return &ast.IntegerLiteral{
			Value: val,
			Pos:   p.makeTokenPos(pos, lit),
		}, nil
	case ast.Float:
		val, _ := strconv.ParseFloat(lit, 64)
		// TODO: Handle error
		return &ast.FloatLiteral{
			Value: val,
			Pos:   p.makeTokenPos(pos, lit),
		}, nil
	case ast.String:
		// Strip quote characters
		val := lit[1 : len(lit)-1]
		return &ast.StringLiteral{
			Value: val,
			Pos:   p.makeTokenPos(pos, lit),
		}, nil
	case ast.Bool:
		val := lit == "true"
		return &ast.BooleanLiteral{
			Value: val,
			Pos:   p.makeTokenPos(pos, lit),
		}, nil
	case ast.Null:
		return &ast.NullLiteral{
			Pos: p.makeTokenPos(pos, lit),
		}, nil
	case ast.ParenLeft:
		// Normal paren, just parse the insides
		expr, err := p.parseGeneralExpression(1, false, false)
		if err != nil {
			return nil, err
		}
		if tok, _, tokPos := p.scanIgnoreWhitespace(); tok != ast.ParenRight {
			return nil, ast.NewParseError(
				"expected ')' following parenthesized expression",
				p.makeRangePos(pos, tokPos))

		}
		if ast.HasArrayTraversal(expr) {
			return &ast.Group{
				Expression: expr,
				Pos:        p.makeTokenPos(pos, lit),
			}, nil
		}
		return expr, nil

	case ast.BracketLeft:
		p.unscan()
		var expr ast.Expression
		var err error
		if immediateLHS {
			// This is a square-bracketed thing with an immediate LHS, so is one of subscript
			// dereference, range spec or constraint
			expr, err = p.parseChainedBracketedExpression()
			if err != nil {
				return nil, err
			}
			if expr == nil {
				// This is actually impossible to trigger barring an error in the parser itself.
				// Turn into a panic?
				return nil, ast.NewParseError(
					"expected constraint, subscript dereference or range",
					p.makeTokenPos(pos, lit))

			}
		} else {
			// This is a square-bracketed thing with no immediate LHS, so must be array
			expr, err = p.parseArrayExpression()
			if err != nil {
				return nil, err
			}
			if expr == nil {
				// This is actually impossible to trigger barring an error in the parser itself.
				// Turn into a panic?
				return nil, ast.NewParseError(
					"expected array expression",
					p.makeTokenPos(pos, lit))

			}
		}
		return expr, nil
	case ast.BraceLeft:
		p.unscan()
		expr, err := p.parseObjectExpression()
		if err != nil {
			return nil, err
		}
		if expr == nil {
			// This is actually impossible to trigger barring an error in the parser itself.
			// Turn into a panic?
			return nil, ast.NewParseError(
				"expected object expression",
				p.makeTokenPos(pos, lit))

		}
		return expr, nil
	}
	// There was no atom here. Unscan and die
	p.unscan()
	return nil, nil
}

// A generarExpression is a set of expressions optionally composed and augmented with operators
func (p *parser) parseGeneralExpression(minPrecedence int, immediateLHS bool, mayBeEmpty bool) (ast.Expression, error) {
	var expr ast.Expression

	// Consider prefix operator
	tok, lit, pos := p.scanIgnoreWhitespace()
	if isPrefixOperator(tok) {
		precedence, associativity := groq.PrecedenceAndAssociativity(tok)
		var rhs ast.Expression
		var err error
		if associativity == groq.AssociatesLeft {
			rhs, err = p.parseGeneralExpression(precedence+1, false, true)
		} else {
			rhs, err = p.parseGeneralExpression(precedence, false, true)
		}
		if err != nil {
			return nil, err
		}
		if rhs == nil {
			// Special consideration for the `...` operator that also is an atom
			if tok == ast.DotDotDot {
				expr = &ast.Ellipsis{
					Pos: p.makeTokenPos(pos, lit),
				}
			}
		} else {
			expr = &ast.PrefixOperator{
				Pos:      p.makeTokenPos(pos, lit),
				Operator: tok,
				RHS:      rhs,
			}
		}
	} else {
		// There was no prefix operator
		p.unscan()
		var err error
		expr, err = p.parseAtom(immediateLHS)
		if err != nil {
			return nil, err
		}
	}

	// A list may be empty, but a general expression must start with an atom
	if expr == nil {
		if mayBeEmpty {
			return nil, nil
		}
		aTok, aLit, pos := p.scan()

		var seen string
		if aTok == ast.EOF {
			seen = "end-of-file"
		} else {
			seen = fmt.Sprintf("token %q", aLit)
		}

		return nil, ast.NewParseError(
			fmt.Sprintf("unexpected %s, expected expression", seen),
			p.makeTokenPos(pos, aLit))

	}
	// At this point, expr is the first value in a potential chain of values stringed together with
	// explicit or implicit operators, so now we'll look for these explicit or implicit operators:
	// Keep parsing until our minPrecedence limit is broken, or expression is complete
	for {
		tok, ident, pos := p.scanIgnoreWhitespace()

		if tok == ast.Name {
			if op, ok := identToOperator(ident); ok {
				tok = op
			}
		}

		var operator ast.Token
		operatorImplicit := false

		if !isOperator(tok) {
			p.unscan()
			if entailsImplicitPipeOperator(tok) {
				operator = ast.Pipe
				operatorImplicit = true
			} else if postfixOperator, ok := expr.(*ast.PostfixOperator); ok &&
				postfixOperator.Operator == ast.Arrow &&
				tok == ast.Name {
				// Add implicit Dot to convert a->b to a->.b when the current
				operator = ast.Dot
				operatorImplicit = true
			} else {
				break
			}
		} else {
			operator = tok
		}

		// Uses precedence climbing to resolve precedence according to
		// http://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
		precedence, associativity := groq.PrecedenceAndAssociativity(operator)
		if precedence < minPrecedence {
			p.unscan()
			break
		}

		if isPostfixOperator(tok) {
			expr = &ast.PostfixOperator{
				Pos:      p.makeTokenPos(pos, lit),
				LHS:      expr,
				Operator: tok,
			}
			continue
		}

		// Infix operator
		if !isInfixOperator(operator) {
			// Impossible to trigger barring a bug in the parser. Turn into panic?
			return nil, ast.NewParseError(
				fmt.Sprintf("expected infix operator, but saw %q", operator.Literal()),
				p.makeTokenPos(pos, lit))

		}
		var rhs ast.Expression
		RHSHasImmediateLHS := operator == ast.Pipe || operator == ast.Dot
		if associativity == groq.AssociatesLeft {
			var err error
			rhs, err = p.parseGeneralExpression(precedence+1, RHSHasImmediateLHS, false)
			if err != nil {
				return nil, err
			}
		} else {
			var err error
			rhs, err = p.parseGeneralExpression(precedence, RHSHasImmediateLHS, false)
			if err != nil {
				return nil, err
			}
		}
		if rhs == nil {
			return nil, ast.NewParseError(
				"expected expressions following operator", p.makeTokenPos(pos, lit))
		}

		// Quietly translate pipes to dots if rhs is attribute and the pipe is implicit. This handles the case
		// where string-literal-attributes is used, as in `a["title"]` which should become `a.title`, not `a|title`.
		if _, ok := rhs.(*ast.Attribute); ok && operator == ast.Pipe && operatorImplicit {
			operator = ast.Dot
		}

		switch operator {
		case ast.Dot:
			// attribute.^ is not allowed
			if _, rhsIsParent := rhs.(*ast.Parent); rhsIsParent {
				// but ^.^, ^.^.^ etc. is allowed
				if !isParentDereferencing(expr) {
					return nil, ast.NewParseError("^ is not allowed here", rhs.GetPos())
				}
			}
			// attribute.@ is not allowed
			if _, rhsIsThis := rhs.(*ast.This); rhsIsThis {
				return nil, ast.NewParseError("@ is not allowed here", rhs.GetPos())
			}
			expr = &ast.DotOperator{
				Pos: p.makeTokenPos(pos, lit),
				LHS: expr,
				RHS: rhs,
			}
		case ast.Pipe:
			expr = &ast.PipeOperator{
				Pos: p.makeTokenPos(pos, lit),
				LHS: expr,
				RHS: rhs,
			}
		default:
			binOp := &ast.BinaryOperator{
				Pos:      p.makeTokenPos(pos, lit),
				Operator: operator,
				LHS:      expr,
				RHS:      rhs,
			}
			// Might be a range expression?
			if rangeExpression := interpretBinOpAsRange(binOp); rangeExpression != nil {
				expr = rangeExpression
			} else {
				expr = binOp
			}
		}
	}
	return expr, nil
}

// parseChainedBracketedExpression: Chained Bracketed Expressions are expressions in `[]`
// that have an immediate LHS. This means they are either a constraint block [a == "1" && b < 2]
// or a subscript a[2], limit/offset a[2..12] or string attribute dereference object["attribute"]
func (p *parser) parseChainedBracketedExpression() (ast.Expression, error) {
	tok, _, posStart := p.scanIgnoreWhitespace()
	if tok != ast.BracketLeft {
		return nil, nil
	}
	expr, err := p.parseGeneralExpression(1, false, true)
	if err != nil {
		return nil, err
	}
	tok, litEnd, posEnd := p.scanIgnoreWhitespace()
	rangeEnd := posEnd + len(litEnd)

	if tok != ast.BracketRight {
		return nil, ast.NewParseError(
			"expected ']' following expression",
			p.makeRangePos(posStart, rangeEnd))

	}

	if expr == nil {
		return &ast.ArrayTraversal{
			Pos: p.makeRangePos(posStart, rangeEnd),
		}, nil
	}

	// The bracketed expression is in the can, now to determine whether it is a constraint or a subscript
	switch t := expr.(type) {
	case *ast.StringLiteral:
		return &ast.Attribute{
			Pos:  p.makeRangePos(posStart, rangeEnd),
			Name: t.Value,
		}, nil
	case *ast.Range:
		if !ast.IsSubscriptExpression(t) {
			return nil, ast.NewParseError(
				"subscript ranges must have integer endpoints",
				p.makeRangePos(posStart, rangeEnd))

		}
		return &ast.Subscript{
			Pos:   p.makeRangePos(posStart, rangeEnd),
			Value: expr,
		}, nil
	default:
		// For go-sumtype linter
	}
	if ast.IsSubscriptExpression(expr) {
		return &ast.Subscript{
			Pos:   p.makeRangePos(posStart, rangeEnd),
			Value: expr,
		}, nil
	}
	return &ast.Constraint{
		Pos:        p.makeRangePos(posStart, rangeEnd),
		Expression: expr,
	}, nil
}

// An array is a list of terms bracketed with `[]` that is determined not to be a
// constraint, subscript or range spec. This is resolved by determining whether a bracketed
// expression has an immediate LHS. These are not arrays: `a[0]`, `*[is "article"]`,
// `feh[1,2,3]`, `*|[a]`. These are arrays: `{"a": [1,2,3]}`, `[1,2,3]`, `a|([1,2,3])`
func (p *parser) parseArrayExpression() (ast.Expression, error) {
	tok, _, posStart := p.scanIgnoreWhitespace()
	if tok != ast.BracketLeft {
		return nil, nil
	}
	exprs, err := p.parseList()
	if err != nil {
		return nil, err
	}
	tok, litEnd, posEnd := p.scanIgnoreWhitespace()
	rangeEnd := posEnd + len(litEnd)

	if tok != ast.BracketRight {
		return nil, ast.NewParseError(
			"expected ']' following array body",
			p.makeRangePos(posStart, rangeEnd))

	}
	return &ast.Array{
		Pos:         p.makeRangePos(posStart, rangeEnd),
		Expressions: exprs,
	}, nil
}

// An object is list of terms bracketed with `{}`
func (p *parser) parseObjectExpression() (ast.Expression, error) {
	tok, _, posStart := p.scanIgnoreWhitespace()
	if tok != ast.BraceLeft {
		return nil, nil
	}
	exprs, err := p.parseList()
	if err != nil {
		return nil, err
	}
	tok, litEnd, posEnd := p.scanIgnoreWhitespace()
	rangeEnd := posEnd + len(litEnd)

	if tok != ast.BraceRight {
		return nil, ast.NewParseError(
			"expected '}' following object body",
			p.makeRangePos(posStart, rangeEnd))

	}
	return &ast.Object{
		Pos:         p.makeRangePos(posStart, rangeEnd),
		Expressions: exprs,
	}, nil
}

func (p *parser) parseFunctionExpression(name string, pos int) (ast.Expression, error) {
	// We already have the function name, so we're expecting either a parenthesized
	// or bare argument list.
	tok, _, posStart := p.scan()
	hasParens := false
	switch tok {
	case ast.ParenLeft:
		hasParens = true
	case ast.Whitespace:
		tok, ident, _ := p.scan()
		// Not function call if followed by { or [ or infix operator
		switch {
		case tok == ast.BracketLeft || tok == ast.BraceLeft || isInfixOperator(tok) ||
			(tok == ast.Name && identIsOperator(ident)):
			p.unscan()
			return nil, nil
		case tok == ast.ParenLeft:
			hasParens = true
		default:
			p.unscan()
		}
	default:
		p.unscan()
		return nil, nil
	}
	var exprs []ast.Expression
	var err error
	switch {
	case hasParens:
		exprs, err = p.parseList()
		if err != nil {
			return nil, err
		}
		tok, litEnd, posEnd := p.scanIgnoreWhitespace()
		rangeEnd := posEnd + len(litEnd)
		if tok != ast.ParenRight {
			return nil, ast.NewParseError(
				"expected ')' following function arguments",
				p.makeRangePos(posStart, rangeEnd))

		}
	default:
		// We allow 'funcName arg' for backwards compatibility
		expr, err := p.parseGeneralExpression(7, false, true)
		if err != nil {
			return nil, err
		}
		// Not a function call if no arguments following it
		if expr == nil {
			p.unscan()
			return nil, nil
		}
		exprs = []ast.Expression{expr}
	}
	return &ast.FunctionCall{
		Name:      name,
		Pos:       p.makeTokenPos(pos, name),
		Arguments: exprs,
	}, nil
}

// Parses a list of BasicExpressions, as in the interiors of projections or
// arrays.
func (p *parser) parseList() ([]ast.Expression, error) {
	expr, err := p.parseGeneralExpression(1, false, true)
	if err != nil {
		return nil, err
	}
	if expr == nil {
		// Empty list is totally fine
		return []ast.Expression{}, nil
	}
	result := []ast.Expression{expr}
	for {
		tok, _, _ := p.scanIgnoreWhitespace()
		if tok != ast.Comma {
			// The absence of a comma after an expression is a valid termination
			// of the list
			p.unscan()
			break
		}
		expr, err := p.parseGeneralExpression(1, false, len(result) > 0)
		if err != nil {
			return nil, err
		}
		if expr == nil {
			// There was no valid expression following the comma, but we are a modern
			// language, and thus we support trailing commas
			break
		}
		result = append(result, expr)
	}
	return result, nil
}

func (p *parser) parse() (ast.Expression, error) {
	token, _, _ := p.scanIgnoreWhitespace()
	if token == ast.EOF {
		return nil, ast.NewParseError(
			"no query",
			p.makeSpotPos(0))

	}
	p.unscan()

	var err error
	result, err := p.parseGeneralExpression(1, false, false)
	if err != nil {
		return nil, err
	}

	token, _, pos := p.scanIgnoreWhitespace()
	if token != ast.EOF {
		return nil, ast.NewParseError(
			"unable to parse entire expression",
			p.makeSpotPos(pos))

	}
	return result, nil
}

func (p *parser) makeTokenPos(start int, literal string) ast.Position {
	return ast.Position{
		Start:  start,
		End:    start + len(literal),
		Source: p.src,
	}
}

func (p *parser) makeRangePos(start int, end int) ast.Position {
	return ast.Position{
		Start:  start,
		End:    end,
		Source: p.src,
	}
}

func (p *parser) makeSpotPos(start int) ast.Position {
	return ast.Position{
		Start:  start,
		End:    start,
		Source: p.src,
	}
}

func isParentDereferencing(expr ast.Expression) bool {
	switch t := expr.(type) {
	case *ast.Parent:
		return true
	case *ast.DotOperator:
		return isParentDereferencing(t.LHS) && isParentDereferencing(t.RHS)
	default:
		return false
	}
}
