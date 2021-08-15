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
