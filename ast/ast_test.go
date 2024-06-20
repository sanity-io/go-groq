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

// Note that ValidateParentAccess is exclusively used for custom defined functions,
// where we don't allow reaching outside the function scope.
func TestAST_ValidateParentAccess(t *testing.T) {
	// This is a valid AST, as parent access is used correctly (within a filter)
	// def foo::bar($baz) = $baz[]->{"foo": *[_type == ^.bar]};
	valid_parentWithinFilter := &ast.Object{
		Expressions: []ast.Expression{
			&ast.BinaryOperator{
				LHS: &ast.StringLiteral{
					Value: "foo",
				},
				RHS: &ast.Filter{
					LHS: &ast.Everything{},
					Constraint: &ast.Constraint{
						Expression: &ast.BinaryOperator{
							LHS: &ast.Attribute{
								Name: "_type",
							},
							RHS: &ast.DotOperator{
								LHS: &ast.Parent{},
								RHS: &ast.Attribute{
									Name: "bar",
								},
							},
						},
					},
				},
			},
		},
	}

	// This is a valid AST, as parent access is used correctly (within a projection)
	// def foo::bar($baz) = $baz[]->{"foo": *[_type == "bar"]{^.name}};
	valid_parentWithinProjection := &ast.Object{
		Expressions: []ast.Expression{
			&ast.BinaryOperator{
				LHS: &ast.StringLiteral{
					Value: "foo",
				},
				RHS: &ast.Projection{
					LHS: &ast.Filter{
						LHS: &ast.Everything{},
						Constraint: &ast.Constraint{
							Expression: &ast.BinaryOperator{
								LHS: &ast.Attribute{
									Name: "_type",
								},
								RHS: &ast.StringLiteral{
									Value: "bar",
								},
							},
						},
					},
					Object: &ast.Object{
						Expressions: []ast.Expression{
							&ast.DotOperator{
								LHS: &ast.Parent{},
								RHS: &ast.Attribute{
									Name: "name",
								},
							},
						},
					},
				},
			},
		},
	}

	// This is a valid AST, as parent access is used correctly (within an order())
	// def foo::bar($baz) = $baz[]->{"foo": *[_type == "person"] | order(^.foo)};
	valid_parentWithinFunctionOrder := &ast.Object{
		Expressions: []ast.Expression{
			&ast.BinaryOperator{
				LHS: &ast.StringLiteral{
					Value: "foo",
				},
				RHS: &ast.FunctionPipe{
					LHS: &ast.Filter{
						LHS: &ast.Everything{},
						Constraint: &ast.Constraint{
							Expression: &ast.BinaryOperator{
								LHS: &ast.Attribute{
									Name: "_type",
								},
								RHS: &ast.StringLiteral{
									Value: "person",
								},
							},
						},
					},
					Func: &ast.FunctionCall{
						Namespace: "global",
						Name:      "order",
						Arguments: []ast.Expression{
							&ast.DotOperator{
								LHS: &ast.Parent{},
								RHS: &ast.Attribute{
									Name: "foo",
								},
							},
						},
					},
				},
			},
		},
	}

	// This is an invalid way of using parent in the AST
	// def foo::bar($baz) = $baz[]->{ foo: ^.name};
	invalid_parentReachesOutsideOfScope := &ast.Object{
		Expressions: []ast.Expression{
			&ast.DotOperator{
				LHS: &ast.Parent{},
				RHS: &ast.Attribute{
					Name: "name",
				},
			},
		},
	}

	assert.NoError(t, ast.ValidateParentAccess(valid_parentWithinFilter))
	assert.NoError(t, ast.ValidateParentAccess(valid_parentWithinProjection))
	assert.NoError(t, ast.ValidateParentAccess(valid_parentWithinFunctionOrder))

	assert.Error(t, ast.ValidateParentAccess(invalid_parentReachesOutsideOfScope))
}
