package ast

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"strconv"
	"strings"
)

// Print prints an AST to GROQ format.
func Print(expr Expression, w io.Writer) error {
	return print(expr, false, false, w)
}

// PrettyPrint prints an AST to GROQ format, with indentation enabled.
func PrettyPrint(expr Expression, w io.Writer) error {
	return print(expr, true, false, w)
}

// Stringify converts an AST to GROQ format.
func Stringify(expr Expression, pretty bool, hideLiterals bool) (string, error) {
	var b bytes.Buffer
	if err := print(expr, pretty, hideLiterals, &b); err != nil {
		return "", err
	}
	return b.String(), nil
}

// MustStringify converts an AST to GROQ format, and panics on failure.
func MustStringify(expr Expression, pretty bool, hideLiterals bool) string {
	if s, err := Stringify(expr, pretty, hideLiterals); err != nil {
		panic(err)
	} else {
		return s
	}
}

func print(expr Expression, pretty bool, hideLiterals bool, w io.Writer) error {
	printer := NewPrinter(pretty, hideLiterals, w)
	printer.Print(expr)
	printer.Flush()
	return printer.stream.LastError
}

type PrintStream struct {
	LastError   error
	writer      *bufio.Writer
	indentLevel int
}

func (stream *PrintStream) Flush() {
	stream.SetError(stream.writer.Flush())
}

func (stream *PrintStream) Indent() {
	stream.indentLevel++
}

func (stream *PrintStream) Unindent() {
	stream.indentLevel--
}

func (stream *PrintStream) WriteString(s string) {
	if stream.LastError == nil {
		_, err := stream.writer.WriteString(s)
		stream.SetError(err)
	}
}

func (stream *PrintStream) NewLine() {
	stream.WriteString("\n" + strings.Repeat(indentation, stream.indentLevel))
}

func (stream *PrintStream) SetError(err error) {
	if stream.LastError == nil {
		stream.LastError = err
	}
}

type unhandledFunc func(*Printer, *PrintStream, Expression) bool

var unhandlers []unhandledFunc

func AddUnhandledFunc(fn unhandledFunc) {
	unhandlers = append(unhandlers, fn)
}

type Printer struct {
	HideLiterals bool
	Pretty       bool

	stream *PrintStream
}

func NewPrinter(pretty bool, hideLiterals bool, w io.Writer) *Printer {
	return &Printer{
		Pretty:       pretty,
		HideLiterals: hideLiterals,
		stream: &PrintStream{
			writer: bufio.NewWriter(w),
		},
	}
}

func (p *Printer) Flush() {
	p.stream.Flush()
}

func (p *Printer) BeginBrace() {
	p.stream.WriteString("{")
	if p.Pretty {
		p.stream.Indent()
		p.stream.NewLine()
	}
}

func (p *Printer) EndBrace() {
	if p.Pretty {
		p.stream.Unindent()
		p.stream.NewLine()
	}
	p.stream.WriteString("}")
}

func (p *Printer) BeginBracket() {
	p.stream.WriteString("[")
	if p.Pretty {
		p.stream.Indent()
		p.stream.NewLine()
	}
}

func (p *Printer) EndBracket() {
	if p.Pretty {
		p.stream.Unindent()
		p.stream.NewLine()
	}
	p.stream.WriteString("]")
}

func (p *Printer) Print(expr Expression) {
	stream := p.stream
	switch e := expr.(type) {
	case *PipeOperator:
		p.Print(e.LHS)
		switch e.RHS.(type) {
		case *Subscript, *ArrayTraversal, *Constraint:
		default:
			stream.WriteString("|")
		}
		p.Print(e.RHS)
	case *DotOperator:
		p.Print(e.LHS)
		stream.WriteString(".")
		p.Print(e.RHS)
	case *ArrayTraversal:
		if e.Expr != nil {
			p.Print(e.Expr)
		}
		stream.WriteString("[]")
	case *Group:
		stream.WriteString("(")
		p.Print(e.Expression)
		stream.WriteString(")")
	case *Tuple:
		stream.WriteString("(")
		for idx, expr := range e.Members {
			if idx > 0 {
				stream.WriteString(", ")
			}
			p.Print(expr)
		}
		stream.WriteString(")")
	case *Constraint:
		stream.WriteString("[")
		p.Print(e.Expression)
		stream.WriteString("]")
	case *Object:
		stream.WriteString("{")
		if p.Pretty {
			stream.Indent()
			stream.NewLine()
		}
		for i, expr := range e.Expressions {
			p.Print(expr)
			if i != len(e.Expressions)-1 {
				stream.WriteString(",")
				if !p.Pretty {
					stream.WriteString(" ")
				}
			}
			if p.Pretty {
				if i == len(e.Expressions)-1 {
					stream.Unindent()
				}
				stream.NewLine()
			}
		}
		stream.WriteString("}")
	case *This:
		stream.WriteString("@")
	case *Array:
		stream.WriteString("[")
		for i, expr := range e.Expressions {
			if i > 0 {
				stream.WriteString(", ")
			}
			p.Print(expr)
		}
		stream.WriteString("]")
	case *FunctionCall:
		if e.Namespace != "" {
			stream.WriteString(e.Namespace)
			stream.WriteString("::")
		}
		stream.WriteString(e.Name)
		stream.WriteString("(")
		if e.Arguments != nil {
			for i, expr := range e.Arguments {
				if i > 0 {
					stream.WriteString(", ")
				}
				p.Print(expr)
			}
		}
		stream.WriteString(")")
	case *Subscript:
		stream.WriteString("[")
		p.Print(e.Value)
		stream.WriteString("]")
	case *Range:
		p.Print(e.Start)
		if e.Inclusive {
			stream.WriteString("..")
		} else {
			stream.WriteString("...")
		}
		p.Print(e.End)
	case *Parent:
		stream.WriteString("^")
	case *Attribute:
		stream.WriteString(e.Name)
	case *Everything:
		stream.WriteString("*")
	case *Ellipsis:
		stream.WriteString("...")
	case *StringLiteral:
		if p.HideLiterals {
			stream.WriteString("$str")
		} else {
			stream.WriteString("\"")
			stream.WriteString(strings.ReplaceAll(e.Value, "\"", "\\\\"))
			stream.WriteString("\"")
		}
	case *IntegerLiteral:
		if p.HideLiterals {
			stream.WriteString("$int")
		} else {
			stream.WriteString(strconv.Itoa(e.Value))
		}
	case *FloatLiteral:
		if p.HideLiterals {
			stream.WriteString("$float")
		} else {
			stream.WriteString(strconv.FormatFloat(e.Value, 'f', -1, 64))
		}
	case *BooleanLiteral:
		switch {
		case p.HideLiterals:
			stream.WriteString("$bool")
		case e.Value:
			stream.WriteString("true")
		default:
			stream.WriteString("false")
		}
	case *NullLiteral:
		stream.WriteString("null")
	case *BinaryOperator:
		if _, ok := e.LHS.(*BinaryOperator); ok {
			stream.WriteString("(")
			p.Print(e.LHS)
			stream.WriteString(")")
		} else {
			p.Print(e.LHS)
		}
		if e.Operator != Colon {
			stream.WriteString(" ")
		}
		p.printOperator(e.Operator)
		stream.WriteString(" ")
		if _, ok := e.RHS.(*BinaryOperator); ok {
			stream.WriteString("(")
			p.Print(e.RHS)
			stream.WriteString(")")
		} else {
			p.Print(e.RHS)
		}
	case *PrefixOperator:
		p.printOperator(e.Operator)
		if _, ok := e.RHS.(*BinaryOperator); ok {
			stream.WriteString("(")
			p.Print(e.RHS)
			stream.WriteString(")")
		} else {
			p.Print(e.RHS)
		}
	case *PostfixOperator:
		switch e.Operator {
		case AscOperator:
			p.Print(e.LHS)
			stream.WriteString(" asc")
		case DescOperator:
			p.Print(e.LHS)
			stream.WriteString(" desc")
		default:
			if _, ok := e.LHS.(*BinaryOperator); ok {
				stream.WriteString("(")
				p.Print(e.LHS)
				stream.WriteString(")")
			} else {
				p.Print(e.LHS)
			}
			p.printOperator(e.Operator)
		}
	case *Filter:
		p.Print(e.LHS)
		p.Print(e.Constraint)
	case *Projection:
		p.Print(e.LHS)
		stream.WriteString("|")
		p.Print(e.Object)
	case *FunctionPipe:
		p.Print(e.LHS)
		stream.WriteString("|")
		p.Print(e.Func)
	case *Element:
		p.Print(e.LHS)
		stream.WriteString("[")
		p.Print(e.Idx)
		stream.WriteString("]")
	case *Slice:
		p.Print(e.LHS)
		stream.WriteString("[")
		p.Print(e.Range)
		stream.WriteString("]")
	case nil:
	default:
		p.unhandled(expr)
	}
}

func (p *Printer) printOperator(t Token) {
	p.stream.WriteString(t.Literal())
}

func (p *Printer) unhandled(expr Expression) {
	for _, fn := range unhandlers {
		if fn(p, p.stream, expr) {
			return
		}
	}
	p.stream.SetError(fmt.Errorf("don't know how to print expression %#v", expr))
}

const indentation = "  "
