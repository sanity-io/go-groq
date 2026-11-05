package print_test

import (
	"bytes"
	"io/ioutil"
	"reflect"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/sanity-io/go-groq/ast"
	"github.com/sanity-io/go-groq/parser"
	"github.com/sanity-io/go-groq/print"
	"github.com/sanity-io/litter"
)

func TestPrettyPrint(t *testing.T) {
	groq, err := ioutil.ReadFile("big.groq")
	require.NoError(t, err)

	origAST, err := parse(string(groq))
	require.NoError(t, err)

	var buf bytes.Buffer
	require.NoError(t, print.PrettyPrint(origAST, &buf))

	newAST, err := parse(string(buf.Bytes()))
	require.NoError(t, err)

	// Remove position information
	origAST = simplifyGroups(removePositions(origAST))
	newAST = simplifyGroups(removePositions(newAST))

	assert.EqualValues(t, litter.Sdump(origAST), litter.Sdump(newAST))
}

func TestParseSpecialAttrs(t *testing.T) {
	groqPretty := `*[_type == "foo"] {
  "out1": @["true"],
  "out2": @["01g"],
  "out3": @["Hello World"]
}`

	parsed, err := parse(groqPretty)
	require.NoError(t, err)

	parsedStr, err := print.Stringify(parsed, true, false)
	require.NoError(t, err)

	assert.EqualValues(t, groqPretty, parsedStr)
}

func TestPrintRoundTrip(t *testing.T) {
	tests := []struct {
		name  string
		query string
	}{
		{"quote", `s == "\""`},
		{"backslash", `s == "\\"`},
		{"multiple backslashes", `s == "\\\\"`},
		{"newline", `s == "line\nbreak"`},
		{"tab", `s == "left\tright"`},
		{"nul", `s == "\u0000"`},
		{"unicode", `s == "Grüße 世界 👋"`},
		{"subtraction left", `(a - b) - c`},
		{"subtraction right", `a - (b - c)`},
		{"addition left", `(a + b) + c`},
		{"addition right", `a + (b + c)`},
		{"multiplication left", `(a * b) * c`},
		{"multiplication right", `a * (b * c)`},
		{"division left", `(a / b) / c`},
		{"division right", `a / (b / c)`},
		{"modulo left", `(a % b) % c`},
		{"modulo right", `a % (b % c)`},
		{"comparison left", `(a < b) < c`},
		{"comparison right", `a < (b < c)`},
		{"exponentiation left", `(a ** b) ** c`},
		{"exponentiation right", `a ** (b ** c)`},
		{"mixed addition and subtraction", `a + (b - c)`},
		{"mixed multiplication and division", `a * (b / c)`},
		{"mixed comparisons", `a == (b != c)`},
		{"and right", `a && (b && c)`},
		{"or right", `a || (b || c)`},
		{"lower precedence left", `(a + b) * c`},
		{"lower precedence right", `a * (b + c)`},
		{"hyphenated attribute", `@["a-b"]`},
		{"dotted attribute", `@["a.b"]`},
		{"parameter-like attribute", `@["$name"]`},
		{"dollar in attribute", `@["a$b"]`},
		{"empty attribute", `@[""]`},
		{"unicode attribute", `@["é"]`},
		{"keyword attribute", `@["true"]`},
		{"attribute with space", `@["Hello World"]`},
	}

	for _, tt := range tests {
		for _, pretty := range []bool{false, true} {
			mode := "compact"
			if pretty {
				mode = "pretty"
			}
			t.Run(tt.name+"/"+mode, func(t *testing.T) {
				original, err := parse(tt.query)
				require.NoError(t, err)

				printed, err := print.Stringify(original, pretty, false)
				require.NoError(t, err)
				reparsed, err := parse(printed)
				require.NoError(t, err, printed)

				// Ensure we ignore changed source offsets and redundant groups when comparing AST meaning.
				original = simplifyGroups(removePositions(original))
				reparsed = simplifyGroups(removePositions(reparsed))
				assert.EqualValues(t, litter.Sdump(original), litter.Sdump(reparsed), printed)
			})
		}
	}
}

func removePositions(expr ast.Expression) ast.Expression {
	return ast.Transform(expr, func(e ast.Expression) ast.Expression {
		v := reflect.ValueOf(e).Elem()
		p := v.FieldByName("Pos")
		p.Set(reflect.ValueOf(ast.Position{}))

		n := reflect.New(v.Type())
		n.Elem().Set(v)
		return n.Interface().(ast.Expression)
	})
}

func parse(groq string) (ast.Expression, error) {
	e, err := parser.Parse(groq, parser.WithParamNodes())
	if err != nil {
		return nil, err
	}
	return e, nil
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
