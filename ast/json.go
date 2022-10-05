package ast

import (
	"encoding/json"
	"fmt"
)

// MarshalJSON marshals an AST node to JSON.
func (token Token) MarshalJSON() ([]byte, error) {
	return []byte(fmt.Sprintf("\"%v\"", token.String())), nil
}

// MarshalJSON marshals an AST node to JSON.
func (*Everything) MarshalJSON() ([]byte, error) {
	return []byte(`{"node": "everything"}`), nil
}

// MarshalJSON marshals an AST node to JSON.
func (*Parent) MarshalJSON() ([]byte, error) {
	return []byte(`{"node": "parent"}`), nil
}

// MarshalJSON marshals an AST node to JSON.
func (e *Constraint) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Node       string     `json:"node,omitempty"`
		Pos        int        `json:"pos"`
		Expression Expression `json:"expression,omitempty"`
	}{
		"constraint",
		e.Pos.Start,
		e.Expression,
	})
}

// MarshalJSON marshals an AST node to JSON.
func (expr *ArrayTraversal) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Node string `json:"node,omitempty"`
		Pos  int    `json:"pos"`
	}{
		"arrayTraversal",
		expr.Pos.Start,
	})
}

// MarshalJSON marshals an AST node to JSON.
func (expr *Object) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Node        string       `json:"node,omitempty"`
		Pos         int          `json:"pos"`
		Expressions []Expression `json:"expressions,omitempty"`
	}{
		"object",
		expr.Pos.Start,
		expr.Expressions,
	})
}

// MarshalJSON marshals an AST node to JSON.
func (expr *Array) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Node        string       `json:"node,omitempty"`
		Pos         int          `json:"pos"`
		Expressions []Expression `json:"expressions,omitempty"`
	}{
		"array",
		expr.Pos.Start,
		expr.Expressions,
	})
}

// MarshalJSON marshals an AST node to JSON.
func (expr *Subscript) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Node  string     `json:"node,omitempty"`
		Pos   int        `json:"pos"`
		Value Expression `json:"value,omitempty"`
	}{
		"subscript",
		expr.Pos.Start,
		expr.Value,
	})
}

// MarshalJSON marshals an AST node to JSON.
func (expr *Range) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Node      string     `json:"node,omitempty"`
		Pos       int        `json:"pos"`
		Start     Expression `json:"start"`
		End       Expression `json:"end"`
		Inclusive bool       `json:"inclusive"`
	}{
		"range",
		expr.Pos.Start,
		expr.Start,
		expr.End,
		expr.Inclusive,
	})
}

// MarshalJSON marshals an AST node to JSON.
func (expr *FunctionCall) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Node      string       `json:"node,omitempty"`
		Pos       int          `json:"pos"`
		Namespace string       `json:"namespace,omitempty"`
		Name      string       `json:"name"`
		Arguments []Expression `json:"arguments,omitempty"`
	}{
		"functionCall",
		expr.Pos.Start,
		expr.Namespace,
		expr.Name,
		expr.Arguments,
	})
}

// MarshalJSON marshals an AST node to JSON.
func (expr *BinaryOperator) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Node     string     `json:"node,omitempty"`
		Pos      int        `json:"pos"`
		Operator Token      `json:"operator"`
		LHS      Expression `json:"lhs"`
		RHS      Expression `json:"rhs"`
	}{
		"binaryOperator",
		expr.Pos.Start,
		expr.Operator,
		expr.LHS,
		expr.RHS,
	})
}

// MarshalJSON marshals an AST node to JSON.
func (expr *PipeOperator) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Node string     `json:"node,omitempty"`
		Pos  int        `json:"pos"`
		LHS  Expression `json:"lhs"`
		RHS  Expression `json:"rhs"`
	}{
		"pipeOperator",
		expr.Pos.Start,
		expr.LHS,
		expr.RHS,
	})
}

// MarshalJSON marshals an AST node to JSON.
func (e *DotOperator) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Node string     `json:"node,omitempty"`
		Pos  int        `json:"pos"`
		LHS  Expression `json:"lhs"`
		RHS  Expression `json:"rhs"`
	}{
		"dotOperator",
		e.Pos.Start,
		e.LHS,
		e.RHS,
	})
}

// MarshalJSON marshals an AST node to JSON.
func (expr *Group) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Node       string     `json:"node,omitempty"`
		Pos        int        `json:"pos"`
		Expression Expression `json:"expression,omitempty"`
	}{
		"group",
		expr.Pos.Start,
		expr.Expression,
	})
}

// MarshalJSON marshals an AST node to JSON.
func (expr *PrefixOperator) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Node     string     `json:"node,omitempty"`
		Pos      int        `json:"pos"`
		Operator Token      `json:"operator"`
		RHS      Expression `json:"rhs"`
	}{
		"prefixOperator",
		expr.Pos.Start,
		expr.Operator,
		expr.RHS,
	})
}

// MarshalJSON marshals an AST node to JSON.
func (expr *PostfixOperator) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Node     string     `json:"node,omitempty"`
		Pos      int        `json:"pos"`
		Operator Token      `json:"operator"`
		LHS      Expression `json:"lhs"`
	}{
		"postfixOperator",
		expr.Pos.Start,
		expr.Operator,
		expr.LHS,
	})
}

// MarshalJSON marshals an AST node to JSON.
func (e *Attribute) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Node string `json:"node,omitempty"`
		Pos  int    `json:"pos"`
		Name string `json:"path,omitempty"`
	}{
		"attribute",
		e.Pos.Start,
		e.Name,
	})
}

// MarshalJSON marshals an AST node to JSON.
func (e *StringLiteral) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Node  string `json:"node,omitempty"`
		Pos   int    `json:"pos"`
		Value string `json:"value"`
	}{
		"string",
		e.Pos.Start,
		e.Value,
	})
}

// MarshalJSON marshals an AST node to JSON.
func (expr *IntegerLiteral) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Node  string `json:"node,omitempty"`
		Pos   int    `json:"pos"`
		Value int    `json:"value"`
	}{
		"integer",
		expr.Pos.Start,
		expr.Value,
	})
}

// MarshalJSON marshals an AST node to JSON.
func (expr *FloatLiteral) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Node  string  `json:"node,omitempty"`
		Pos   int     `json:"pos"`
		Value float64 `json:"value"`
	}{
		"float",
		expr.Pos.Start,
		expr.Value,
	})
}

// MarshalJSON marshals an AST node to JSON.
func (expr *BooleanLiteral) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Node  string `json:"node,omitempty"`
		Pos   int    `json:"pos"`
		Value bool   `json:"value"`
	}{
		"bool",
		expr.Pos.Start,
		expr.Value,
	})
}

// MarshalJSON marshals an AST node to JSON.
func (expr *NullLiteral) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Node string `json:"node,omitempty"`
		Pos  int    `json:"pos"`
	}{
		"null",
		expr.Pos.Start,
	})
}

// MarshalJSON marshals an AST node to JSON.
func (*Ellipsis) MarshalJSON() ([]byte, error) {
	return []byte(`{"node": "ellipsis"}`), nil
}
