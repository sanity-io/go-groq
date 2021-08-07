package groq

import (
	"fmt"

	"github.com/sanity-io/go-groq/ast"
)

// Params represents a set of parameters to be interpolated into a GROQ statement
type Params map[string]interface{}

// ParamPrefixCharacter specifies the character with which all param names must start in order to get recognized
const ParamPrefixCharacter = '$'

// ParseError represents an error during parsing
type ParseError struct {
	Message string
	Pos     ast.Position
}

func (e *ParseError) Error() string {
	if e.Pos.Start == e.Pos.End {
		return fmt.Sprintf("parse error at position %d: %s", e.Pos.End, e.Message)
	}
	return fmt.Sprintf("parse error at positions %d..%d: %s", e.Pos.Start, e.Pos.End, e.Message)
}
