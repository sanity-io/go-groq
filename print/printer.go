package print

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"strconv"
	"strings"

	"github.com/sanity-io/go-groq"
	"github.com/sanity-io/go-groq/ast"
)

// Print prints an AST to GROQ format.
func Print(expr ast.Expression, w io.Writer) error {
	return print(expr, false, false, w)
}

// PrettyPrint prints an AST to GROQ format, with indentation enabled.
func PrettyPrint(expr ast.Expression, w io.Writer) error {
	return print(expr, true, false, w)
}

// Stringify converts an AST to GROQ format.
func Stringify(expr ast.Expression, pretty bool, hideLiterals bool) (string, error) {
	var b bytes.Buffer
	if err := print(expr, pretty, hideLiterals, &b); err != nil {
		return "", err
	}
	return b.String(), nil
}

// MustStringify converts an AST to GROQ format, and panics on failure.
func MustStringify(expr ast.Expression, pretty bool, hideLiterals bool) string {
	if s, err := Stringify(expr, pretty, hideLiterals); err != nil {
		panic(err)
	} else {
		return s
	}
}

func print(expr ast.Expression, pretty bool, hideLiterals bool, w io.Writer) error {
	printer := NewPrinter(pretty, hideLiterals, w)
	printer.Print(expr)
	printer.Flush()
	return printer.stream.err
}

type printStream struct {
	err         error
	writer      *bufio.Writer
	indentLevel int
}

func (s *printStream) flush() {
	s.setError(s.writer.Flush())
}

func (s *printStream) indent() {
	s.indentLevel++
}

func (s *printStream) unindent() {
	s.indentLevel--
}

func (s *printStream) append(v string) {
	if s.err == nil {
		_, err := s.writer.WriteString(v)
		s.setError(err)
	}
}

func (s *printStream) newLine() {
	s.append("\n" + strings.Repeat(indentation, s.indentLevel))
}

func (s *printStream) setError(err error) {
	if s.err == nil {
		s.err = err
	}
}

type unhandledFunc func(*Printer, *printStream, ast.Expression) bool

var unhandlers []unhandledFunc

func AddUnhandledFunc(fn unhandledFunc) {
	unhandlers = append(unhandlers, fn)
}

type Printer struct {
	HideLiterals bool
	Pretty       bool

	stream *printStream
}

func NewPrinter(pretty bool, hideLiterals bool, w io.Writer) *Printer {
	return &Printer{
		Pretty:       pretty,
		HideLiterals: hideLiterals,
		stream: &printStream{
			writer: bufio.NewWriter(w),
		},
	}
}

func (p *Printer) Flush() {
	p.stream.flush()
}

func (p *Printer) BeginBrace() {
	p.stream.append("{")
	if p.Pretty {
		p.stream.indent()
		p.stream.newLine()
	}
}

func (p *Printer) EndBrace() {
	if p.Pretty {
		p.stream.unindent()
		p.stream.newLine()
	}
	p.stream.append("}")
}

func (p *Printer) BeginBracket() {
	p.stream.append("[")
	if p.Pretty {
		p.stream.indent()
		p.stream.newLine()
	}
}

func (p *Printer) EndBracket() {
	if p.Pretty {
		p.stream.unindent()
		p.stream.newLine()
	}
	p.stream.append("]")
}

func (p *Printer) Print(expr ast.Expression) {
	p.print(simplifyGroups(expr))
}

func (p *Printer) print(expr ast.Expression) {
	stream := p.stream
	switch e := expr.(type) {
	case *ast.PipeOperator:
		p.print(e.LHS)
		stream.append(" ")
		p.print(e.RHS)
	case *ast.DotOperator:
		p.print(e.LHS)
		stream.append(".")
		p.print(e.RHS)
	case *ast.ArrayTraversal:
		if e.Expr != nil {
			p.print(e.Expr)
		}
		stream.append("[]")
	case *ast.Group:
		stream.append("(")
		p.print(e.Expression)
		stream.append(")")
	case *ast.Tuple:
		stream.append("(")
		for idx, expr := range e.Members {
			if idx > 0 {
				stream.append(", ")
			}
			p.print(expr)
		}
		stream.append(")")
	case *ast.Constraint:
		stream.append("[")
		p.print(e.Expression)
		stream.append("]")
	case *ast.Object:
		stream.append("{")
		if p.Pretty {
			stream.indent()
			stream.newLine()
		}
		for i, expr := range e.Expressions {
			p.print(expr)
			if i != len(e.Expressions)-1 {
				stream.append(",")
				if !p.Pretty {
					stream.append(" ")
				}
			}
			if p.Pretty {
				if i == len(e.Expressions)-1 {
					stream.unindent()
				}
				stream.newLine()
			}
		}
		stream.append("}")
	case *ast.This:
		stream.append("@")
	case *ast.Array:
		stream.append("[")
		for i, expr := range e.Expressions {
			if i > 0 {
				stream.append(", ")
			}
			p.print(expr)
		}
		stream.append("]")
	case *ast.FunctionCall:
		if e.Namespace != "" {
			stream.append(e.Namespace)
			stream.append("::")
		}
		stream.append(e.Name)
		stream.append("(")
		if e.Arguments != nil {
			for i, expr := range e.Arguments {
				if i > 0 {
					stream.append(", ")
				}
				p.print(expr)
			}
		}
		stream.append(")")
	case *ast.Subscript:
		p.print(e.Value)
	case *ast.Range:
		p.print(e.Start)
		if e.Inclusive {
			stream.append("..")
		} else {
			stream.append("...")
		}
		p.print(e.End)
	case *ast.Parent:
		stream.append("^")
	case *ast.Attribute:
		stream.append(e.Name)
	case *ast.Everything:
		stream.append("*")
	case *ast.Ellipsis:
		stream.append("...")
	case *ast.StringLiteral:
		if p.HideLiterals {
			stream.append("$str")
		} else {
			stream.append("\"")
			stream.append(strings.ReplaceAll(e.Value, "\"", "\\\\"))
			stream.append("\"")
		}
	case *ast.IntegerLiteral:
		if p.HideLiterals {
			stream.append("$int")
		} else {
			stream.append(strconv.Itoa(e.Value))
		}
	case *ast.FloatLiteral:
		if p.HideLiterals {
			stream.append("$float")
		} else {
			stream.append(strconv.FormatFloat(e.Value, 'f', -1, 64))
		}
	case *ast.BooleanLiteral:
		switch {
		case p.HideLiterals:
			stream.append("$bool")
		case e.Value:
			stream.append("true")
		default:
			stream.append("false")
		}
	case *ast.NullLiteral:
		stream.append("null")
	case *ast.BinaryOperator:
		if parenthesizeSubexpr(e, e.LHS) {
			stream.append("(")
			p.print(e.LHS)
			stream.append(")")
		} else {
			p.print(e.LHS)
		}
		if e.Operator != ast.Colon {
			stream.append(" ")
		}
		p.printOperator(e.Operator)
		stream.append(" ")
		if parenthesizeSubexpr(e, e.RHS) {
			stream.append("(")
			p.print(e.RHS)
			stream.append(")")
		} else {
			p.print(e.RHS)
		}
	case *ast.PrefixOperator:
		p.printOperator(e.Operator)
		if parenthesizeSubexpr(e, e.RHS) {
			stream.append("(")
			p.print(e.RHS)
			stream.append(")")
		} else {
			p.print(e.RHS)
		}
	case *ast.PostfixOperator:
		switch e.Operator {
		case ast.AscOperator:
			p.print(e.LHS)
			stream.append(" asc")
		case ast.DescOperator:
			p.print(e.LHS)
			stream.append(" desc")
		default:
			if _, ok := e.LHS.(*ast.BinaryOperator); ok {
				stream.append("(")
				p.print(e.LHS)
				stream.append(")")
			} else {
				p.print(e.LHS)
			}
			p.printOperator(e.Operator)
		}
	case *ast.Filter:
		p.print(e.LHS)
		p.print(e.Constraint)
	case *ast.Projection:
		p.print(e.LHS)
		stream.append(" ")
		p.print(e.Object)
	case *ast.FunctionPipe:
		p.print(e.LHS)
		stream.append(" | ")
		p.print(e.Func)
	case *ast.Element:
		p.print(e.LHS)
		stream.append("[")
		p.print(e.Idx)
		stream.append("]")
	case *ast.Slice:
		p.print(e.LHS)
		stream.append("[")
		p.print(e.Range)
		stream.append("]")
	case *ast.Param:
		stream.append("$")
		stream.append(e.Name)
	case nil:
	default:
		p.unhandled(expr)
	}
}

func (p *Printer) printOperator(t ast.Token) {
	p.stream.append(t.Literal())
}

func (p *Printer) unhandled(expr ast.Expression) {
	for _, fn := range unhandlers {
		if fn(p, p.stream, expr) {
			return
		}
	}
	p.stream.setError(fmt.Errorf("don't know how to print expression %#v", expr))
}

const indentation = "  "

func parenthesizeSubexpr(parent, expr ast.Expression) bool {
	if _, ok := expr.(*ast.Group); ok {
		return true
	}
	switch parent := parent.(type) {
	case *ast.BinaryOperator:
		if expr, ok := expr.(*ast.BinaryOperator); ok {
			if expr.Operator == parent.Operator {
				// E.g. (a && b) && c
				return false
			}
			a, _ := groq.PrecedenceAndAssociativity(parent.Operator)
			b, _ := groq.PrecedenceAndAssociativity(expr.Operator)
			return a > b
		}
	case *ast.PrefixOperator:
		switch expr.(type) {
		case *ast.Attribute, ast.Literal:
			return false
		default:
			return true
		}
	}
	return false
}

func simplifyGroups(expr ast.Expression) ast.Expression {
	return ast.Transform(expr, func(e ast.Expression) ast.Expression {
		if g, ok := e.(*ast.Group); ok {
			// The group expression is for terminating array traversal; don't group if not necessary
			if !ast.Contains(g.Expression, func(e ast.Expression) bool {
				_, ok := e.(*ast.ArrayTraversal)
				return ok
			}) {
				return g.Expression
			}
		}
		return e
	})
}
