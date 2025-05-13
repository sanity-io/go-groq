package parserv2_test

import (
	"errors"
	"reflect"
	"testing"

	"github.com/blang/semver"

	"github.com/sanity-io/litter"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/sanity-io/go-groq"
	"github.com/sanity-io/go-groq/ast"
	"github.com/sanity-io/go-groq/internal/testhelpers"
	"github.com/sanity-io/go-groq/parser"
	"github.com/sanity-io/go-groq/parser/internal/parserv2"
)

func TestParser(t *testing.T) {
	testhelpers.WithEachTest(t, func(t *testing.T, test *testhelpers.Test) {
		if includeTest(test) {
			testhelpers.ASTTest(t, test, "snapshots",
				func(query string, params groq.Params, functions map[ast.FunctionID]*ast.FunctionDefinition) (ast.Expression, error) {
					return parserv2.Parse(query, parserv2.WithParams(params), parserv2.WithFunctions(functions))
				})
		}
	})
}

func includeTest(test *testhelpers.Test) bool {
	return test.Version == nil || (*test.Version)(semver.MustParse("2.0.0"))
}

func TestBackwardsCompatibility(t *testing.T) {
	testhelpers.WithEachTest(t, func(t *testing.T, test *testhelpers.Test) {
		if test.Version != nil && (*test.Version)(semver.MustParse("2.0.0")) {
			return
		}

		expected, err := parser.Parse(test.Query)
		if err != nil {
			t.Skipf("Skipping query [%s] since it's not valid in old parser: %s", test.Query, err)
		}

		actual, err := parserv2.Parse(test.Query)
		require.NoError(t, err)

		actual = actual.Transform(transformToLegacyAST)

		litterOpts := litter.Options{
			FieldFilter: func(f reflect.StructField, _ reflect.Value) bool {
				return f.Name != "Pos" // Skip the unnecessary position fields
			},
			HomePackage: "ast",
			Compact:     true, // Save space
		}
		assert.Equal(t, litterOpts.Sdump(expected), litterOpts.Sdump(actual))
	})
}

func TestParseWithoutFunctions(t *testing.T) {
	query := "fn foo::bar($baz) = $baz{a, b}; *[]"

	// should parse function definitions without functions: "noop"
	{
		_, err := parserv2.Parse(query, parserv2.WithFunctions(nil))
		require.NoError(t, err)
	}

	// should parse function definitions with functions defined.
	{
		functions := make(map[ast.FunctionID]*ast.FunctionDefinition)
		_, err := parserv2.Parse(query, parserv2.WithFunctions(functions))
		require.NoError(t, err)
		require.Len(t, functions, 1)
		function := functions[ast.FunctionID{"foo", "bar"}]
		require.NotNil(t, function)
	}
}

func transformToLegacyAST(expr ast.Expression) ast.Expression {
	switch expr := expr.(type) {
	case *ast.FunctionPipe:
		return &ast.PipeOperator{
			LHS: expr.LHS,
			RHS: expr.Func,
		}
	case *ast.Projection:
		return &ast.PipeOperator{
			LHS: expr.LHS,
			RHS: expr.Object,
		}
	case *ast.Element:
		return &ast.PipeOperator{
			LHS: expr.LHS,
			RHS: expr.Idx,
		}
	case *ast.Slice:
		return &ast.PipeOperator{
			LHS: expr.LHS,
			RHS: expr.Range,
		}
	case *ast.Filter:
		return &ast.PipeOperator{
			LHS: expr.LHS,
			RHS: expr.Constraint,
		}
	case *ast.ArrayTraversal:
		return &ast.PipeOperator{
			LHS: expr.Expr,
			RHS: &ast.ArrayTraversal{},
		}
	case *ast.Group:
		if ast.HasArrayTraversal(expr.Expression) {
			return expr
		}
		return expr.Expression
	default:
		return expr
	}
}

func TestErrors(t *testing.T) {
	assertParseFailure(t, "(person", "expected ')' following parenthesized expression", 0, 8)
	assertParseFailure(t, "count(person", "expected ')' following function arguments", 5, 13)
	assertParseFailure(t, "[1,2", "expected ']' following array body", 0, 5)
	assertParseFailure(t, "{a", "expected '}' following object body", 0, 3)
	assertParseFailure(t, "[1,2,3]....", "unable to parse entire expression", 7, 7)
	assertParseFailure(t, "a.^.c", "^ is not allowed here", 2, 3)
	assertParseFailure(t, "a.@.c", "@ is not allowed here", 2, 3)
	assertParseFailure(t, "*{,}", "expected '}' following object body", 1, 3)
	assertParseFailure(t, "*[true] / ]", "unexpected token \"]\", expected expression", 10, 11)
	assertParseFailure(t, "*[true] /", "unexpected end-of-file, expected expression", 9, 9)
	assertParseFailure(t, "1.", `unexpected token "1.", expected expression`, 0, 2)
	assertParseFailure(t, "count (*)", "unable to parse entire expression", 6, 6)
	assertParseFailure(t, "count x", "unable to parse entire expression", 6, 6)
	assertParseFailure(t, "string::length", "expected a function following namespace expression", 8, 14)
	assertParseFailure(t, "string::+(1, 2)'", "expected a function following namespace expression", 8, 9)
	assertParseFailure(t, "::length", "unexpected token \"::\", expected expression", 0, 2)
	assertParseFailure(t, "fn ($bar)", "expected function namespace", 3, 3)
	assertParseFailure(t, "fn foo($bar)", "expected '::' followed by a function name", 6, 6)
	assertParseFailure(t, "fn ::foo($bar)", "expected function namespace", 3, 3)
	assertParseFailure(t, "fn foo::($bar)", "expected a function name", 8, 8)
	assertParseFailure(t, "fn foo::bar $baz", "expected '(' following function name", 12, 12)
	assertParseFailure(t, "fn foo::bar($baz", "expected ')' following function arguments", 17, 17)
	assertParseFailure(t, "fn foo::bar($baz) { a, b }", "expected '=' following ()", 18, 18)
	assertParseFailure(t, "fn foo::bar(baz) = { a, b }", "expected parameter name", 12, 12)
	assertParseFailure(t, "fn foo::moo($woo) = $woo{ a, b }; fn foo::bar($baz) = $woo{ a, b }", "param $woo referenced, but not provided", 54, 57)
	assertParseFailure(t, "fn foo::bar($baz) = $baz{ a, b } fn", "expected ';' at the end of function definition", 33, 33)

}

func assertParseFailure(t *testing.T, src string, message string, start int, end int) {
	_, err := parserv2.Parse(src)
	assert.Error(t, err)
	var parseErr parser.ParseError
	require.True(t, errors.As(err, &parseErr))
	require.Equal(t, message, parseErr.Message())
	assert.Equal(t, start, parseErr.Pos().Start)
	assert.Equal(t, end, parseErr.Pos().End)
}
