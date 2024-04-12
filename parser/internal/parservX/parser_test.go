package parservX_test

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
	"github.com/sanity-io/go-groq/parser/internal/parservX"
)

func TestParser(t *testing.T) {
	testhelpers.WithEachTest(t, func(t *testing.T, test *testhelpers.Test) {
		if includeTest(test) {
			testhelpers.ASTTest(t, test, "snapshots",
				func(query string, params groq.Params) (ast.Expression, error) {
					return parservX.Parse(query, parservX.WithParams(params))
				})
		}
	})
}

func includeTest(test *testhelpers.Test) bool {
	// Old parser does not have some features
	return test.Version == nil || (*test.Version)(semver.MustParse("0.0.0"))
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

		actual, err := parservX.Parse(test.Query)
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
	assertParseFailure(t, "fn(person", "expected ')' following function arguments", 2, 10)
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
	assertParseFailure(t, "fragment { a, b } *[true]", "expected fragment name", 9, 10)
	assertParseFailure(t, "fragment Foo ( a, b ) *[true]", "expected '{'", 13, 14)
	assertParseFailure(t, "fragment Foo { a, b ) *[true]", "expected '}'", 20, 21)
	assertParseFailure(t, "fragment Foo { a, b } *[true]{fragment::Bar()}", "fragment \"Bar\" not found", 30, 43)
}

func assertParseFailure(t *testing.T, src string, message string, start int, end int) {
	_, err := parservX.Parse(src)
	assert.Error(t, err)
	var parseErr parser.ParseError
	require.True(t, errors.As(err, &parseErr))
	require.Equal(t, message, parseErr.Message())
	assert.Equal(t, start, parseErr.Pos().Start)
	assert.Equal(t, end, parseErr.Pos().End)
}
