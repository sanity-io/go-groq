package parservX_test

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"io/ioutil"
	"os"
	"path/filepath"
	"reflect"
	"regexp"
	"runtime"
	"sort"
	"strings"
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
	WithEachTest(t, func(t *testing.T, test *Test) {
		if includeTest(test) {
			ASTTest(t, test, "snapshots",
				func(query string, params groq.Params, functions map[ast.FunctionID]*ast.FunctionDefinition) (ast.Expression, error) {
					return parservX.Parse(query, parservX.WithParams(params), parservX.WithFunctions(functions))
				})
		}
	})
}

func includeTest(test *Test) bool {
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

func TestParseWithoutFunctions(t *testing.T) {
	query := "def foo::bar($baz) = $baz{a, b}; *[]"

	// should parse function definitions without functions: "noop"
	{
		_, err := parservX.Parse(query, parservX.WithFunctions(nil))
		require.NoError(t, err)
	}

	// should parse function definitions with functions defined.
	{
		functions := make(map[ast.FunctionID]*ast.FunctionDefinition)
		_, err := parservX.Parse(query, parservX.WithFunctions(functions))
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
	assertParseFailure(t, "def ($bar)", "expected function namespace", 4, 4)
	assertParseFailure(t, "def foo($bar)", "expected '::' followed by a function name", 7, 7)
	assertParseFailure(t, "def ::foo($bar)", "expected function namespace", 4, 4)
	assertParseFailure(t, "def foo::($bar)", "expected a function name", 9, 9)
	assertParseFailure(t, "def foo::bar $baz", "expected '(' following function name", 13, 13)
	assertParseFailure(t, "def foo::bar($baz", "expected ')' following function arguments", 18, 18)
	assertParseFailure(t, "def foo::bar($baz) { a, b }", "expected '=' following ()", 19, 19)
	assertParseFailure(t, "def foo::bar(baz) = { a, b }", "expected parameter name", 13, 13)
	assertParseFailure(t, "def foo::moo($woo) = $woo{ a, b }; def foo::bar($baz) = $woo{ a, b }", "param $woo referenced, but not provided", 56, 59)
	assertParseFailure(t, "def foo::bar($baz) = $baz{ a, b } def", "expected ';' at the end of function definition", 34, 34)

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

// Copy of testhelpers which allows functions to be passed to the parser
var versionRegex = regexp.MustCompile(`//groq:version=(\S+)`)
var paramRegex = regexp.MustCompile(`//groq:param:([^=]+)=([^\n]+)`)

type Test struct {
	Name      string
	Query     string
	Params    groq.Params
	Functions map[ast.FunctionID]*ast.FunctionDefinition
	Valid     bool
	Version   *semver.Range
	FileName  string
}

func ASTTest(
	t *testing.T,
	test *Test,
	outputDir string,
	parser func(query string, params groq.Params, functions map[ast.FunctionID]*ast.FunctionDefinition) (ast.Expression, error),
) {
	fileName := filepath.Join(outputDir, SnapshotFileName(test))

	require.NoError(t, os.MkdirAll(filepath.Dir(fileName), 0755))

	t.Logf("File: %s", fileName)

	parsed, err := parser(test.Query, test.Params, test.Functions)
	require.NoError(t, err)

	// Make sure we can walk the AST
	assert.True(t, ast.Walk(parsed, func(expr ast.Expression) bool { return true }))

	type Result struct {
		Query     string
		AST       ast.Expression
		Functions []*ast.FunctionDefinition
	}

	result := Result{
		Query: test.Query,
		AST:   parsed,
	}

	// append to a slice and sort since maps are not ordered
	for _, fn := range test.Functions {
		// Make sure we can walk the function body
		assert.True(t, ast.Walk(fn.Body, func(expr ast.Expression) bool { return true }))

		result.Functions = append(result.Functions, fn)
	}
	sort.Slice(result.Functions, func(i, j int) bool {
		return result.Functions[i].ID.String() < result.Functions[j].ID.String()
	})

	actual := []byte(litter.Options{
		FieldFilter: func(f reflect.StructField, _ reflect.Value) bool {
			return f.Name != "Source" // Skip the unnecessary source fields
		},
		HomePackage: "ast",
		Compact:     true, // Save space
	}.Sdump(result))

	expected := testhelpers.ReadSnapshot(t, fileName, actual)

	assert.Equal(t,
		strings.TrimSpace(string(expected)),
		strings.TrimSpace(string(actual)),
	)
}

func WithEachTest(t *testing.T, f func(t *testing.T, test *Test)) {
	_, filename, _, _ := runtime.Caller(0)

	pattern := filepath.Join(filepath.Dir(filename), "*.groq")
	files, err := filepath.Glob(pattern)
	require.NoError(t, err)

	for _, file := range files {
		name := filepath.Base(file)
		queryBytes, err := ioutil.ReadFile(file)
		require.NoError(t, err)
		query := string(queryBytes)

		test := Test{
			Name:      name,
			Query:     query,
			Params:    make(groq.Params),
			Functions: make(map[ast.FunctionID]*ast.FunctionDefinition),
			Valid:     true,
			FileName:  file,
		}

		for _, param := range paramRegex.FindAllStringSubmatch(query, -1) {
			var paramValue interface{}
			err := json.Unmarshal([]byte(param[2]), &paramValue)
			require.NoError(t, err)
			test.Params[param[1]] = paramValue
		}

		versionMatch := versionRegex.FindStringSubmatch(query)
		if versionMatch != nil {
			rng := semver.MustParseRange(versionMatch[1])
			test.Version = &rng
		}

		t.Run(name, func(t *testing.T) {
			f(t, &test)
		})
	}
}

func SnapshotFileName(test *Test) string {
	// Hash by both name and query, since queries are not unique
	s := sha256.New()
	if _, err := s.Write([]byte(test.Name)); err != nil {
		panic(err)
	}
	if _, err := s.Write([]byte(test.Query)); err != nil {
		panic(err)
	}
	return hex.EncodeToString(s.Sum(nil)) + ".txt"
}
