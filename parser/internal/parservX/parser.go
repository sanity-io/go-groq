package parservX

import (
	"fmt"
	"strconv"

	"github.com/sanity-io/go-groq"
	"github.com/sanity-io/go-groq/ast"
	"github.com/sanity-io/go-groq/tokenizer"
)

// parseError represents an error during parsing
type parseError struct {
	msg string
	pos ast.Position
}

func (e *parseError) Pos() ast.Position { return e.pos }
func (e *parseError) Message() string   { return e.msg }

func (e *parseError) Error() string {
	if e.pos.Start == e.pos.End {
		return fmt.Sprintf("parse error at position %d: %s", e.pos.End, e.msg)
	}
	return fmt.Sprintf("parse error at positions %d..%d: %s", e.pos.Start, e.pos.End, e.msg)
}

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

func WithFunctions(fns map[ast.FunctionID]*ast.FunctionDefinition) Option {
	return func(parser *parser) {
		parser.functions = fns
	}
}

// parser represents a parser (!).
type parser struct {
	tk               *tokenizer.Tokenizer
	src              string
	params           groq.Params
	createParamNodes bool
	buf              struct {
		tok ast.Token // last read token
		lit string    // last read literal
		pos int       // position of last token
		n   int       // buffer size (max=1)
	}
	functions map[ast.FunctionID]*ast.FunctionDefinition
}

// Parse parses a string of GROQ.
func Parse(src string, opts ...Option) (ast.Expression, error) {
	return newParser(src, opts...).parse()
}

func newParser(src string, opts ...Option) *parser {
	p := &parser{tk: tokenizer.New(src), src: src, params: groq.Params{}, functions: make(map[ast.FunctionID]*ast.FunctionDefinition)}
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
		return nil, &parseError{
			msg: fmt.Sprintf("param $%s referenced, but not provided", name),
			pos: p.makeTokenPos(pos, name),
		}
	}
	expr, err := ast.LiteralFromInterface(value, p.makeTokenPos(pos, name))
	if err != nil {
		return nil, &parseError{
			msg: fmt.Sprintf("error while accessing value of $%s: %s", name, err.Error()),
			pos: p.makeTokenPos(pos, name),
		}
	}
	return expr, nil
}

// parseAtom parses an atomic expression which is basically an expression that from the outside does not involve any operators.
// It is one of:
// * attribute name
// * param
// * a function call, with or without a namespace
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
		function, err := p.parseFunctionExpression(ast.Name, lit, pos)
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
		return p.parseParenthesisExpr(pos, lit)
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
				return nil, &parseError{
					msg: "expected constraint, subscript dereference or range",
					pos: p.makeTokenPos(pos, lit),
				}
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
				return nil, &parseError{
					msg: "expected array expression",
					pos: p.makeTokenPos(pos, lit),
				}
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
			return nil, &parseError{
				msg: "expected object expression",
				pos: p.makeTokenPos(pos, lit),
			}
		}
		return expr, nil
	}
	// There was no atom here. Unscan and die
	p.unscan()
	return nil, nil
}

func (p *parser) parseParenthesisExpr(pos int, lit string) (ast.Expression, error) {
	expr, err := p.parseGeneralExpression(1, false, false)
	if err != nil {
		return nil, err
	}

	tok, _, tokPos := p.scanIgnoreWhitespace()

	var tupleMembers []ast.Expression

	if tok == ast.Comma {
		// Parse as tuple
		tupleMembers = make([]ast.Expression, 0, 2)
		tupleMembers = append(tupleMembers, expr)

		for tok == ast.Comma {
			expr, err = p.parseGeneralExpression(1, false, false)
			if err != nil {
				return nil, err
			}
			tupleMembers = append(tupleMembers, expr)

			tok, _, tokPos = p.scanIgnoreWhitespace()
		}
	}

	if tok != ast.ParenRight {
		return nil, &parseError{
			msg: "expected ')' following parenthesized expression",
			pos: p.makeRangePos(pos, tokPos),
		}
	}

	if len(tupleMembers) > 0 {
		return &ast.Tuple{
			Members: tupleMembers,
			Pos:     p.makeTokenPos(pos, lit),
		}, nil
	}

	return &ast.Group{
		Expression: expr,
		Pos:        p.makeTokenPos(pos, lit),
	}, nil
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

		return nil, &parseError{
			msg: fmt.Sprintf("unexpected %s, expected expression", seen),
			pos: p.makeTokenPos(pos, aLit),
		}
	}

	// At this point, expr is the first value in a potential chain of values stringed together with
	// explicit or implicit operators, so now we'll look for these explicit or implicit operators:
	// Keep parsing until our minPrecedence limit is broken, or expression is complete
Loop:
	for {
		tok, ident, pos := p.scanIgnoreWhitespace()

		if tok == ast.Name {
			if op, ok := identToOperator(ident); ok {
				tok = op
			}
		}

		var operator ast.Token
		if !isOperator(tok) {
			p.unscan()
			if tok == ast.BraceLeft {
				operator = ast.Pipe
			} else if tok == ast.BracketLeft {
				if groq.HighestPrecedence < minPrecedence {
					break Loop
				}
				// This is a square-bracketed thing with an immediate LHS, so is one of subscript
				// dereference, range spec or constraint
				rhs, err := p.parseChainedBracketedExpression()
				if err != nil {
					return nil, err
				}
				switch rhs := rhs.(type) {
				case *ast.Attribute:
					// expr["foo"] is same as expr.foo
					expr = &ast.DotOperator{
						Pos: p.makeTokenPos(pos, lit),
						LHS: expr,
						RHS: rhs,
					}
					continue Loop
				case *ast.Subscript:
					// expr[0] or expr[0..1]
					if isRange(rhs.Value) {
						expr = &ast.Slice{
							Pos:   p.makeTokenPos(p.buf.pos, p.buf.lit),
							LHS:   expr,
							Range: rhs,
						}
					} else {
						expr = &ast.Element{
							Pos: p.makeTokenPos(p.buf.pos, p.buf.lit),
							LHS: expr,
							Idx: rhs,
						}
					}
					continue Loop
				case *ast.Constraint:
					// expr[filter]
					expr = &ast.Filter{
						Pos:        p.makeTokenPos(p.buf.pos, p.buf.lit),
						LHS:        expr,
						Constraint: rhs,
					}
					continue Loop
				case *ast.ArrayTraversal:
					// expr[]
					expr = &ast.ArrayTraversal{
						Pos:  p.makeTokenPos(p.buf.pos, p.buf.lit),
						Expr: expr,
					}
					continue Loop
				default:
					p.unscan()
					break Loop
				}
			} else if postfixOperator, ok := expr.(*ast.PostfixOperator); ok &&
				postfixOperator.Operator == ast.Arrow &&
				tok == ast.Name {
				// Add implicit Dot to convert a->b to a->.b when the current
				operator = ast.Dot
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
			return nil, &parseError{
				msg: fmt.Sprintf("expected infix operator, but saw %q", operator.Literal()),
				pos: p.makeTokenPos(pos, lit),
			}
		}

		rhsHasImmediateLHS := operator == ast.Pipe || operator == ast.Dot

		var rhs ast.Expression
		if associativity == groq.AssociatesLeft {
			var err error
			rhs, err = p.parseGeneralExpression(precedence+1, rhsHasImmediateLHS, false)
			if err != nil {
				return nil, err
			}
		} else {
			var err error
			rhs, err = p.parseGeneralExpression(precedence, rhsHasImmediateLHS, false)
			if err != nil {
				return nil, err
			}
		}
		if rhs == nil {
			return nil, &parseError{
				pos: p.makeTokenPos(pos, lit),
				msg: "expected expressions following operator",
			}
		}

		switch operator {
		case ast.Dot:
			switch rhs := rhs.(type) {
			case *ast.Parent:
				// attribute.^ is not allowed but ^.^, ^.^.^ etc. is allowed
				if !isParentDereferencing(expr) {
					return nil, &parseError{
						pos: rhs.GetPos(),
						msg: "^ is not allowed here",
					}
				}
			case *ast.This:
				// attribute.@ is not allowed
				return nil, &parseError{
					pos: rhs.GetPos(),
					msg: "@ is not allowed here",
				}
			}
			expr = &ast.DotOperator{
				Pos: p.makeTokenPos(pos, lit),
				LHS: expr,
				RHS: rhs,
			}
		case ast.Pipe:
			switch rhs := rhs.(type) {
			case *ast.Object:
				expr = &ast.Projection{
					Pos:    p.makeTokenPos(pos, lit),
					LHS:    expr,
					Object: rhs,
				}
			case *ast.FunctionCall:
				expr = &ast.FunctionPipe{
					Pos:  p.makeTokenPos(pos, lit),
					LHS:  expr,
					Func: rhs,
				}
			default:
				return nil, &parseError{
					pos: rhs.GetPos(),
					msg: "object or function expected",
				}
			}
		default:
			binOp := &ast.BinaryOperator{
				Pos:      p.makeTokenPos(pos, lit),
				Operator: operator,
				LHS:      expr,
				RHS:      rhs,
			}
			// Might be a range expression?
			if rangeExpression := asRange(binOp); rangeExpression != nil {
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
		return nil, &parseError{
			msg: "expected ']' following expression",
			pos: p.makeRangePos(posStart, rangeEnd),
		}
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
			return nil, &parseError{
				msg: "subscript ranges must have integer endpoints",
				pos: p.makeRangePos(posStart, rangeEnd),
			}
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
		return nil, &parseError{
			msg: "expected ']' following array body",
			pos: p.makeRangePos(posStart, rangeEnd),
		}
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
		return nil, &parseError{
			msg: "expected '}' following object body",
			pos: p.makeRangePos(posStart, rangeEnd),
		}
	}
	return &ast.Object{
		Pos:         p.makeRangePos(posStart, rangeEnd),
		Expressions: exprs,
	}, nil
}

func (p *parser) parseFunctionExpression(nameToken ast.Token, name string, namePos int) (*ast.FunctionCall, error) {
	// We already have the function name, so we're expecting either a parenthesized
	// or bare argument list.
	tok, lit, pos := p.scan()
	hasParens := false
	switch tok {
	case ast.DoubleColon:
		if nameToken != ast.Name {
			return nil, &parseError{
				msg: fmt.Sprintf("expected valid namespace identifier before '%s'", ast.DoubleColon.String()),
				pos: p.makeTokenPos(pos, lit),
			}
		}

		// It should be a function call after a double colon (namespace).
		fTok, fLit, fPos := p.scanIgnoreWhitespace()
		if fTok != ast.Name {
			return nil, &parseError{
				msg: "expected a function following namespace expression",
				pos: p.makeTokenPos(fPos, fLit),
			}
		}

		function, err := p.parseFunctionExpression(ast.DoubleColon, fLit, fPos)
		if err != nil {
			return nil, err
		}
		if function == nil {
			return nil, &parseError{
				msg: "expected a function following namespace expression",
				pos: p.makeTokenPos(fPos, fLit),
			}
		}
		function.Namespace = name    // Assign namespace to function call.
		function.Pos.Start = namePos // Reset position to start of namespace.
		return function, nil
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
			return nil, &parseError{
				msg: "expected ')' following function arguments",
				pos: p.makeRangePos(pos, rangeEnd),
			}
		}
	default:
		p.unscan()
		return nil, nil
	}
	return &ast.FunctionCall{
		Name:      name,
		Pos:       p.makeTokenPos(namePos, name),
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
		return nil, &parseError{
			msg: "no query",
			pos: p.makeSpotPos(0),
		}
	}
	p.unscan()

	// Parse top level function definitions
	err := p.parseFunctionDefinitions()
	if err != nil {
		return nil, err
	}

	result, err := p.parseGeneralExpression(1, false, false)
	if err != nil {
		return nil, err
	}

	token, _, pos := p.scanIgnoreWhitespace()
	if token != ast.EOF {
		return nil, &parseError{
			msg: "unable to parse entire expression",
			pos: p.makeSpotPos(pos),
		}
	}
	return result, nil
}

func (p *parser) parseFunctionDefinitions() error {

	if p.functions == nil {
		return &parseError{
			msg: "functions are not supported",
			pos: p.makeSpotPos(0),
		}
	}

	for {
		tok, lit, _ := p.scanIgnoreWhitespace()
		if tok == ast.Name && p.isFunctionDefinitionKeyword(lit) {
			function, err := p.parseFunctionDefinition()
			if err != nil {
				return err
			}
			p.functions[function.GetID()] = function
		} else {
			p.unscan()
			break
		}
	}
	return nil
}

func (p *parser) isFunctionDefinitionKeyword(s string) bool {
	switch s {
	case "def", "fn", "func", "function", "let", "const":
		return true
	default:
		return false
	}
}

/*
parseFunctionDefinition parses a function definition of the form:
def foo::imageAsset($asset) = $asset->{url, foo, bar}
*/
func (p *parser) parseFunctionDefinition() (*ast.FunctionDefinition, error) {
	namespace, name, err := p.parseFunctionName()
	if err != nil {
		return nil, err
	}

	tok, lit, pos := p.scanIgnoreWhitespace()
	if tok != ast.ParenLeft {
		return nil, &parseError{
			msg: "expected '(' following function name",
			pos: p.makeSpotPos(pos),
		}
	}

	args, err := p.parseFunctionArguments()
	if err != nil {
		return nil, err
	}

	tok, _, pos = p.scanIgnoreWhitespace()
	if tok != ast.ParenRight {
		return nil, &parseError{
			msg: "expected ')' following function arguments",
			pos: p.makeSpotPos(pos),
		}
	}

	tok, lit, pos = p.scanIgnoreWhitespace()
	if tok != ast.EqualSign {
		return nil, &parseError{
			msg: "expected '=' following ()",
			pos: p.makeSpotPos(pos),
		}
	}

	body, err := p.parseFunctionBody()
	if err != nil {
		return nil, err
	}

	return &ast.FunctionDefinition{
		ID:         ast.FunctionID{Namespace: namespace, Name: name},
		Body:       body,
		Pos:        p.makeTokenPos(pos, lit),
		Parameters: args,
	}, nil

}

func (p *parser) parseFunctionName() (string, string, error) {
	tok, lit, pos := p.scanIgnoreWhitespace()
	if tok != ast.Name {
		return "", "", &parseError{
			msg: "expected function namespace",
			pos: p.makeSpotPos(pos),
		}
	}
	functionNamespace := lit

	tok, lit, pos = p.scanIgnoreWhitespace()
	if tok != ast.DoubleColon {
		return "", "", &parseError{
			msg: "expected '::' followed by a function name",
			pos: p.makeSpotPos(pos),
		}
	}

	tok, lit, pos = p.scanIgnoreWhitespace()
	if tok != ast.Name {
		return "", "", &parseError{
			msg: "expected a function name",
			pos: p.makeSpotPos(pos),
		}
	}
	functionName := lit

	return functionNamespace, functionName, nil
}

func (p *parser) parseFunctionArguments() ([]ast.FunctionParam, error) {
	var args []ast.FunctionParam

	// We only allow 1 argument at the moment
	tok, lit, pos := p.scanIgnoreWhitespace()
	if tok != ast.Name || lit[0] != '$' {
		return nil, &parseError{
			msg: "expected parameter name",
			pos: p.makeSpotPos(pos),
		}
	}
	param := ast.FunctionParam{
		Name: lit,
		Pos:  p.makeTokenPos(pos, lit),
	}
	args = append(args, param)

	return args, nil
}

/*
The function body is one of the forms:
  - $param{…}
  - $param->{…}
  - $param[]{…}
  - $param[]->{…}
*/
func (p *parser) parseFunctionBody() (ast.Expression, error) {
	// Skip parsing $param as we already parsed it in the function arguments
	tok, lit, pos := p.scanIgnoreWhitespace()
	if tok != ast.Name || lit[0] != '$' {
		return nil, &parseError{
			msg: "expected parameter name",
			pos: p.makeSpotPos(pos),
		}
	}

	tok, lit, pos = p.scanIgnoreWhitespace()
	if tok != ast.BraceLeft && tok != ast.BracketLeft && tok != ast.Arrow {
		return nil, &parseError{
			msg: "expected '{', '[' or '->' following parameter name",
			pos: p.makeSpotPos(pos),
		}
	}
	body, err := p.parseGeneralExpression(1, false, false)
	if err != nil {
		return nil, err
	}
	return body, nil
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
