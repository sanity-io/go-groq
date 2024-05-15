package ast_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/sanity-io/go-groq/ast"
)

func TestAST_ObjectEquality(t *testing.T) {
	a := &ast.Object{
		Expressions: []ast.Expression{
			&ast.Attribute{Name: "a"},
			&ast.Attribute{Name: "b"},
		},
	}

	b := &ast.Object{
		Expressions: []ast.Expression{
			&ast.Attribute{Name: "a"},
			&ast.Attribute{Name: "b"},
		},
	}

	c := &ast.Object{
		Expressions: []ast.Expression{
			&ast.Attribute{Name: "b"},
			&ast.Attribute{Name: "a"},
		},
	}

	assert.True(t, a.Equal(a)) // should be equal to itself
	assert.True(t, a.Equal(b)) // should be equal to another identical object
	assert.False(t, a.Equal(c))
}

func TestAST_ArrayEquality(t *testing.T) {
	a := &ast.Array{
		Expressions: []ast.Expression{
			&ast.Attribute{Name: "a"},
			&ast.Attribute{Name: "b"},
		},
	}

	b := &ast.Array{
		Expressions: []ast.Expression{
			&ast.Attribute{Name: "a"},
			&ast.Attribute{Name: "b"},
		},
	}

	c := &ast.Array{
		Expressions: []ast.Expression{
			&ast.Attribute{Name: "b"},
			&ast.Attribute{Name: "a"},
		},
	}

	assert.True(t, a.Equal(a)) // should be equal to itself
	assert.True(t, a.Equal(b)) // should be equal to another identical array
	assert.False(t, a.Equal(c))
}

func TestAST_TupleEquality(t *testing.T) {
	a := &ast.Tuple{
		Members: []ast.Expression{
			&ast.Attribute{Name: "a"},
			&ast.Attribute{Name: "b"},
		},
	}

	b := &ast.Tuple{
		Members: []ast.Expression{
			&ast.Attribute{Name: "a"},
			&ast.Attribute{Name: "b"},
		},
	}

	c := &ast.Tuple{
		Members: []ast.Expression{
			&ast.Attribute{Name: "b"},
			&ast.Attribute{Name: "a"},
		},
	}

	assert.True(t, a.Equal(a)) // should be equal to itself
	assert.True(t, a.Equal(b)) // should be equal to another identical tuple
	assert.False(t, a.Equal(c))
}
