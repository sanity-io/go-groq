package ast_test

import (
	"fmt"
	"testing"

	"github.com/sanity-io/go-groq/ast"
	"github.com/stretchr/testify/assert"
)

func TestToken_String(t *testing.T) {
	for i := ast.Illegal + 1; i < ast.TokenMax; i++ {
		assert.True(t, i.String() != "UNKNOWN TOKEN",
			fmt.Sprintf("Token with index %d must have a String value", i))
		assert.True(t, i.Literal() != "<unknown token>",
			fmt.Sprintf("Token with index %d must have a Literal value", i))
	}
}
