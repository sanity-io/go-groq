package ast

//go-sumtype:decl Expression

// Position represents a position in the source code
type Position struct {
	Start  int
	End    int
	Source string
}

// Expression is the common interface for all GROQ expressions
type Expression interface {
	GetPos() Position
	Transform(TransformFunc) Expression
	sealed()
}

// Literal is the common interface implemented by all literal nodes
type Literal interface {
	LiteralValue() interface{}
}

// Everything is the `*`, the root of all queries
type Everything struct {
	Pos Position
}

// This `@` is the current root value of the scope
type This struct {
	Pos Position
}

// Parent is the `^`, the item being processed in a mapping operation
type Parent struct {
	Pos Position
}

// A Constraint operation, on the form `*[is "article" && expiresAt < $now && _createdAt desc]`
type Constraint struct {
	Pos        Position
	Expression Expression
}

// Object is a `{title, body, somethingsomething}`
type Object struct {
	Pos         Position
	Expressions []Expression
}

// Array is a an array on the form `[a, b, 1, 2, 3]`
type Array struct {
	Pos         Position
	Expressions []Expression
}

// Subscript is on the form `[0]`
type Subscript struct {
	Pos   Position
	Value Expression
}

// Range is a literal range expression on the form `[1..10]` (inclusive),
// `[1...10]` (exclusive)
type Range struct {
	Pos       Position
	Start     Expression
	End       Expression
	Inclusive bool
}

// FunctionCall represents a function invocation on the form function(expression).
// Namespace represents namespaced function call like string::length()
type FunctionCall struct {
	Namespace string
	Pos       Position
	Name      string
	Arguments []Expression
}

// BinaryOperator represents two expressions joint with an operator
type BinaryOperator struct {
	Pos      Position
	LHS      Expression
	RHS      Expression
	Operator Token
}

// DotOperator is a convenience class for the common operator '.'
type DotOperator struct {
	Pos Position
	LHS Expression
	RHS Expression
}

// ArrayTraversal marks the expression for array traversal.
type ArrayTraversal struct {
	Pos Position

	// Expr is NOT used by the old engine, where it is combined with a PipeOperator.
	Expr Expression
}

// Group is a node inserted around an expression to indicate places where
// explicit parentheses have special meaning.
//
// It is currently only used to stop array traversal, i.e. to indicate that
// an expression should not be folded over by subsequent expressions.
//
// For example, while a[][0] means "the first element of each a", (a[])[0]
// means "the first element of a". Rather than resolving this in the parser,
// we wrap the left side in a group node.
type Group struct {
	Pos        Position
	Expression Expression
}

// Tuple represents parenthesized expression: (foo, bar)
type Tuple struct {
	Pos     Position
	Members []Expression
}

// PipeOperator is a convenience class for the very common operator '|'
type PipeOperator struct {
	Pos Position
	LHS Expression
	RHS Expression
}

// PrefixOperator represents a prefix operator `!completed``
type PrefixOperator struct {
	Pos      Position
	RHS      Expression
	Operator Token
}

// PostfixOperator represents an expression followed by an operator `_createdAt desc`
type PostfixOperator struct {
	Pos      Position
	LHS      Expression
	Operator Token
}

// Attribute represents specification of a field on the form `title` as in `book.title`
type Attribute struct {
	Pos  Position
	Name string
}

// Param represents a placeholder parameter, e.g. $foo.
type Param struct {
	Pos  Position
	Name string
}

// StringLiteral is literal strings
type StringLiteral struct {
	Pos   Position
	Value string
}

// LiteralValue implements Literal.
func (e *StringLiteral) LiteralValue() interface{} {
	return e.Value
}

// NullLiteral is a literal null
type NullLiteral struct {
	Pos Position
}

// LiteralValue implements Literal.
func (e *NullLiteral) LiteralValue() interface{} {
	return nil
}

// IntegerLiteral is literal integer
type IntegerLiteral struct {
	Pos   Position
	Value int
}

// LiteralValue implements Literal.
func (e *IntegerLiteral) LiteralValue() interface{} {
	return e.Value
}

// FloatLiteral is literal float
type FloatLiteral struct {
	Pos   Position
	Value float64
}

// LiteralValue implements Literal.
func (e *FloatLiteral) LiteralValue() interface{} {
	return e.Value
}

// BooleanLiteral is a literal bool
type BooleanLiteral struct {
	Pos   Position
	Value bool
}

// LiteralValue implements Literal.
func (e *BooleanLiteral) LiteralValue() interface{} {
	return e.Value
}

// Ellipsis represents a '...'-statement
type Ellipsis struct {
	Pos Position
}

// A Projection maps an object or array of objects to a new object.
type Projection struct {
	Pos    Position
	LHS    Expression
	Object *Object
}

// A FunctionPipe maps an object or array of objects through a function.
type FunctionPipe struct {
	Pos  Position
	LHS  Expression
	Func *FunctionCall
}

// A Filter filters an array using another expression.
type Filter struct {
	Pos        Position
	LHS        Expression
	Constraint *Constraint
}

// Element returns an indexed element of an array.
type Element struct {
	Pos Position
	LHS Expression
	Idx *Subscript
}

// Slice retrieves a range of an array.
type Slice struct {
	Pos   Position
	LHS   Expression
	Range *Subscript
}

func (e *Everything) sealed()      {}
func (e *This) sealed()            {}
func (e *Parent) sealed()          {}
func (e *Constraint) sealed()      {}
func (e *Object) sealed()          {}
func (e *Array) sealed()           {}
func (e *Subscript) sealed()       {}
func (e *Range) sealed()           {}
func (e *FunctionCall) sealed()    {}
func (e *BinaryOperator) sealed()  {}
func (e *DotOperator) sealed()     {}
func (e *ArrayTraversal) sealed()  {}
func (e *Group) sealed()           {}
func (e *Tuple) sealed()           {}
func (e *PipeOperator) sealed()    {}
func (e *PrefixOperator) sealed()  {}
func (e *PostfixOperator) sealed() {}
func (e *Attribute) sealed()       {}
func (e *StringLiteral) sealed()   {}
func (e *IntegerLiteral) sealed()  {}
func (e *FloatLiteral) sealed()    {}
func (e *BooleanLiteral) sealed()  {}
func (e *NullLiteral) sealed()     {}
func (e *Ellipsis) sealed()        {}
func (*Projection) sealed()        {}
func (*FunctionPipe) sealed()      {}
func (*Filter) sealed()            {}
func (*Element) sealed()           {}
func (*Slice) sealed()             {}
func (*Param) sealed()             {}

func (e *Everything) GetPos() Position      { return e.Pos }
func (e *This) GetPos() Position            { return e.Pos }
func (e *Parent) GetPos() Position          { return e.Pos }
func (e *Constraint) GetPos() Position      { return e.Pos }
func (e *Object) GetPos() Position          { return e.Pos }
func (e *Array) GetPos() Position           { return e.Pos }
func (e *Subscript) GetPos() Position       { return e.Pos }
func (e *Range) GetPos() Position           { return e.Pos }
func (e *FunctionCall) GetPos() Position    { return e.Pos }
func (e *BinaryOperator) GetPos() Position  { return e.Pos }
func (e *DotOperator) GetPos() Position     { return e.Pos }
func (e *ArrayTraversal) GetPos() Position  { return e.Pos }
func (e *Group) GetPos() Position           { return e.Pos }
func (e *Tuple) GetPos() Position           { return e.Pos }
func (e *PipeOperator) GetPos() Position    { return e.Pos }
func (e *PrefixOperator) GetPos() Position  { return e.Pos }
func (e *PostfixOperator) GetPos() Position { return e.Pos }
func (e *Attribute) GetPos() Position       { return e.Pos }
func (e *StringLiteral) GetPos() Position   { return e.Pos }
func (e *IntegerLiteral) GetPos() Position  { return e.Pos }
func (e *FloatLiteral) GetPos() Position    { return e.Pos }
func (e *BooleanLiteral) GetPos() Position  { return e.Pos }
func (e *NullLiteral) GetPos() Position     { return e.Pos }
func (e *Ellipsis) GetPos() Position        { return e.Pos }
func (e *Projection) GetPos() Position      { return e.Pos }
func (e *FunctionPipe) GetPos() Position    { return e.Pos }
func (e *Filter) GetPos() Position          { return e.Pos }
func (e *Element) GetPos() Position         { return e.Pos }
func (e *Slice) GetPos() Position           { return e.Pos }
func (e *Param) GetPos() Position           { return e.Pos }
