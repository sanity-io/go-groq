package parser_test

import (
	"path/filepath"
	"testing"

	"github.com/blang/semver"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/sanity-io/go-groq"
	"github.com/sanity-io/go-groq/ast"
	"github.com/sanity-io/go-groq/groqtest"
	"github.com/sanity-io/go-groq/parser"
	"github.com/sanity-io/go-groq/testhelpers"
)

func TestParser(t *testing.T) {
	testhelpers.WithEachTest(t, func(t *testing.T, test *groqtest.Test) {
		if includeTest(test) {
			testhelpers.ASTTest(t, test, filepath.Join("snapshots", "nonstrict"),
				func(query string, params groq.Params) (ast.Expression, error) {
					return parser.Parse(query,
						parser.WithParams(groq.Params{"identity": "someuser"}),
						parser.WithParams(params),
					)
				})
		}
	})
}

func includeTest(test *groqtest.Test) bool {
	// Old parser does not have some features
	return (test.Version == nil || (*test.Version)(semver.MustParse("1.0.0"))) && len(test.Features) == 0
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
}

func assertParseFailure(t *testing.T, src string, message string, start int, end int, opts ...parser.Option) {
	t.Helper()

	_, err := parser.Parse(src, opts...)
	require.Error(t, err)
	require.IsType(t, &groq.ParseError{}, err)

	parseErr := err.(*groq.ParseError)
	require.Equal(t, message, parseErr.Message)
	assert.Equal(t, start, parseErr.Pos.Start)
	assert.Equal(t, end, parseErr.Pos.End)
}
