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
	Equal(Expression) bool
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

// PrefixOperator represents a prefix operator `!completed`
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

type FunctionID struct {
	Namespace string
	Name      string
}

func (id FunctionID) String() string {
	if id.Namespace == "" {
		return id.Name
	}
	return id.Namespace + "::" + id.Name
}

type FunctionDefinition struct {
	Pos        Position
	ID         FunctionID
	Body       Expression
	Parameters []*FunctionParamDefinition
}

type FunctionParamDefinition struct {
	Index int
	Name  string
}

func (fd *FunctionDefinition) GetID() FunctionID {
	return fd.ID
}

type FunctionParam struct {
	Definition *FunctionParamDefinition
	Pos        Position
}

func (fp FunctionParam) GetPos() Position {
	return fp.Pos
}

func (fp FunctionParam) Transform(f TransformFunc) Expression {
	return f(fp)
}

func (fp FunctionParam) Equal(e Expression) bool {
	if e, ok := e.(*FunctionParam); ok {
		return fp.Definition.Name == e.Definition.Name
	}
	return false
}

func (fp FunctionParam) sealed() {}

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

func (*Ellipsis) Equal(b Expression) bool   { _, ok := b.(*Ellipsis); return ok }
func (*This) Equal(b Expression) bool       { _, ok := b.(*This); return ok }
func (*Parent) Equal(b Expression) bool     { _, ok := b.(*Parent); return ok }
func (*Everything) Equal(b Expression) bool { _, ok := b.(*Everything); return ok }

func (e *Attribute) Equal(b Expression) bool {
	if b, ok := b.(*Attribute); ok {
		return ok && e.Name == b.Name
	}
	return false
}

func (e *ArrayTraversal) Equal(b Expression) bool {
	if b, ok := b.(*ArrayTraversal); ok {
		return ok && e.Expr.Equal(b.Expr)
	}
	return false
}

func (e *PrefixOperator) Equal(b Expression) bool {
	if b, ok := b.(*PrefixOperator); ok {
		return ok && e.Operator == b.Operator && e.RHS.Equal(b.RHS)
	}
	return false
}

func (e *PostfixOperator) Equal(b Expression) bool {
	if b, ok := b.(*PostfixOperator); ok {
		return ok && e.Operator == b.Operator && e.LHS.Equal(b.LHS)
	}
	return false
}

func (e *Projection) Equal(b Expression) bool {
	if b, ok := b.(*Projection); ok {
		return ok && e.LHS.Equal(b.LHS) && e.Object.Equal(b.Object)
	}
	return false
}

func (e *Object) Equal(b Expression) bool {
	if b, ok := b.(*Object); ok && len(e.Expressions) == len(b.Expressions) {
		for idx, ae := range e.Expressions {
			if !ae.Equal(b.Expressions[idx]) {
				return false
			}
		}
		return true
	}
	return false
}

func (e *Array) Equal(b Expression) bool {
	if b, ok := b.(*Array); ok && len(e.Expressions) == len(b.Expressions) {
		for idx, ae := range e.Expressions {
			if !ae.Equal(b.Expressions[idx]) {
				return false
			}
		}
		return true
	}
	return false
}

func (e *FunctionCall) Equal(b Expression) bool {
	if b, ok := b.(*FunctionCall); ok && e.Namespace == b.Namespace && e.Name == b.Name && len(e.Arguments) == len(b.Arguments) {
		for idx, ae := range e.Arguments {
			if !ae.Equal(b.Arguments[idx]) {
				return false
			}
		}
		return true
	}
	return false
}

func (e *BinaryOperator) Equal(b Expression) bool {
	if b, ok := b.(*BinaryOperator); ok {
		return e.LHS.Equal(b.LHS) && e.RHS.Equal(b.RHS)
	}
	return false
}

func (e *Group) Equal(b Expression) bool {
	if b, ok := b.(*Group); ok {
		return e.Expression.Equal(b.Expression)
	}
	return false
}

func (e *StringLiteral) Equal(b Expression) bool {
	if b, ok := b.(*StringLiteral); ok {
		return e.Value == b.Value
	}
	return false
}

func (e *NullLiteral) Equal(b Expression) bool {
	_, ok := b.(*NullLiteral)
	return ok
}

func (e *BooleanLiteral) Equal(b Expression) bool {
	if b, ok := b.(*BooleanLiteral); ok {
		return e.Value == b.Value
	}
	return false
}

func (e *FloatLiteral) Equal(b Expression) bool {
	if b, ok := b.(*FloatLiteral); ok {
		return e.Value == b.Value
	}
	return false
}

func (e *IntegerLiteral) Equal(b Expression) bool {
	if b, ok := b.(*IntegerLiteral); ok {
		return e.Value == b.Value
	}
	return false
}

func (e *Range) Equal(b Expression) bool {
	if b, ok := b.(*Range); ok {
		return e.Inclusive == b.Inclusive && e.Start.Equal(b.Start) && e.End.Equal(b.End)
	}
	return false
}

func (e *Slice) Equal(b Expression) bool {
	if b, ok := b.(*Slice); ok {
		return e.LHS.Equal(b.LHS) && e.Range.Equal(b.Range)
	}
	return false
}

func (e *Subscript) Equal(b Expression) bool {
	if b, ok := b.(*Subscript); ok {
		return e.Value.Equal(b.Value)
	}
	return false
}

func (e *Tuple) Equal(b Expression) bool {
	if b, ok := b.(*Tuple); ok && len(e.Members) == len(b.Members) {
		for idx, ae := range e.Members {
			if !ae.Equal(b.Members[idx]) {
				return false
			}
		}
		return true
	}
	return false
}

func (e *PipeOperator) Equal(b Expression) bool {
	if b, ok := b.(*PipeOperator); ok {
		return e.LHS.Equal(b.LHS) && e.RHS.Equal(b.RHS)
	}
	return false
}

func (e *DotOperator) Equal(b Expression) bool {
	if b, ok := b.(*DotOperator); ok {
		return e.LHS.Equal(b.LHS) && e.RHS.Equal(b.RHS)
	}
	return false
}

func (e *Constraint) Equal(b Expression) bool {
	if b, ok := b.(*Constraint); ok {
		return e.Expression.Equal(b.Expression)
	}
	return false
}

func (e *Element) Equal(b Expression) bool {
	if b, ok := b.(*Element); ok {
		return e.LHS.Equal(b.LHS) && e.Idx.Equal(b.Idx)
	}
	return false
}

func (e *Filter) Equal(b Expression) bool {
	if b, ok := b.(*Filter); ok {
		return e.LHS.Equal(b.LHS) && e.Constraint.Equal(b.Constraint)
	}
	return false
}

func (e *FunctionPipe) Equal(b Expression) bool {
	if b, ok := b.(*FunctionPipe); ok {
		return e.LHS.Equal(b.LHS) && e.Func.Equal(b.Func)
	}
	return false
}

func (e *Param) Equal(b Expression) bool {
	if b, ok := b.(*Param); ok {
		return e.Name == b.Name
	}
	return false
}
