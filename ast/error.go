package ast

import "fmt"

// ParseError represents an error during parsing
type ParseError struct {
	msg string
	pos Position
}

func NewParseError(msg string, pos Position) *ParseError {
	return &ParseError{msg: msg, pos: pos}
}

func (e *ParseError) Pos() Position   { return e.pos }
func (e *ParseError) Message() string { return e.msg }

func (e *ParseError) Error() string {
	if e.pos.Start == e.pos.End {
		return fmt.Sprintf("parse error at position %d: %s", e.pos.End, e.msg)
	}
	return fmt.Sprintf("parse error at positions %d..%d: %s", e.pos.Start, e.pos.End, e.msg)
}
