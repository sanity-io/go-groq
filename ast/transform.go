package ast

type TransformFunc func(Expression) Expression

// Transform walks an expression depth-first.
func Transform(expr Expression, f TransformFunc) Expression {
	if expr == nil {
		return nil
	}
	return expr.Transform(f)
}

func (e *Everything) Transform(f TransformFunc) Expression     { return f(e) }
func (e *This) Transform(f TransformFunc) Expression           { return f(e) }
func (e *Ellipsis) Transform(f TransformFunc) Expression       { return f(e) }
func (e *Parent) Transform(f TransformFunc) Expression         { return f(e) }
func (e *StringLiteral) Transform(f TransformFunc) Expression  { return f(e) }
func (e *FloatLiteral) Transform(f TransformFunc) Expression   { return f(e) }
func (e *BooleanLiteral) Transform(f TransformFunc) Expression { return f(e) }
func (e *IntegerLiteral) Transform(f TransformFunc) Expression { return f(e) }
func (e *NullLiteral) Transform(f TransformFunc) Expression    { return f(e) }
func (e *Attribute) Transform(f TransformFunc) Expression      { return f(e) }
func (e *Param) Transform(f TransformFunc) Expression          { return f(e) }

func (e *ArrayTraversal) Transform(f TransformFunc) Expression {
	out := *e
	out.Expr = Transform(e.Expr, f)
	return f(&out)
}

func (e *Constraint) Transform(f TransformFunc) Expression {
	out := *e
	out.Expression = Transform(e.Expression, f)
	return f(&out)
}

func (e *Subscript) Transform(f TransformFunc) Expression {
	out := *e
	out.Value = Transform(e.Value, f)
	return f(&out)
}

func (e *Range) Transform(f TransformFunc) Expression {
	out := *e
	out.Start = Transform(e.Start, f)
	out.End = Transform(e.End, f)
	return f(&out)
}

func (e *BinaryOperator) Transform(f TransformFunc) Expression {
	out := *e
	out.LHS = Transform(e.LHS, f)
	out.RHS = Transform(e.RHS, f)
	return f(&out)
}

func (e *DotOperator) Transform(f TransformFunc) Expression {
	out := *e
	out.LHS = Transform(e.LHS, f)
	out.RHS = Transform(e.RHS, f)
	return f(&out)
}

func (e *Group) Transform(f TransformFunc) Expression {
	out := *e
	out.Expression = Transform(e.Expression, f)
	return f(&out)
}

func (e *Tuple) Transform(f TransformFunc) Expression {
	out := *e
	out.Members = transformExprs(e.Members, f)
	return f(&out)
}

func (e *PipeOperator) Transform(f TransformFunc) Expression {
	out := *e
	out.LHS = Transform(e.LHS, f)
	out.RHS = Transform(e.RHS, f)
	return f(&out)
}

func (e *PrefixOperator) Transform(f TransformFunc) Expression {
	out := *e
	out.RHS = Transform(e.RHS, f)
	return f(&out)
}

func (e *PostfixOperator) Transform(f TransformFunc) Expression {
	out := *e
	out.LHS = Transform(e.LHS, f)
	return f(&out)
}

func (e *Object) Transform(f TransformFunc) Expression {
	out := *e
	out.Expressions = transformExprs(e.Expressions, f)
	return f(&out)
}

func (e *Array) Transform(f TransformFunc) Expression {
	out := *e
	out.Expressions = transformExprs(e.Expressions, f)
	return f(&out)
}

func (e *FunctionCall) Transform(f TransformFunc) Expression {
	out := *e
	out.Arguments = transformExprs(e.Arguments, f)
	return f(&out)
}

func (e *Element) Transform(f TransformFunc) Expression {
	out := *e
	out.LHS = Transform(e.LHS, f)
	out.Idx = Transform(e.Idx, f).(*Subscript)
	return f(&out)
}

func (e *Filter) Transform(f TransformFunc) Expression {
	out := *e
	out.LHS = Transform(e.LHS, f)
	out.Constraint = Transform(e.Constraint, f).(*Constraint)
	return f(&out)
}

func (e *Slice) Transform(f TransformFunc) Expression {
	out := *e
	out.LHS = Transform(e.LHS, f)
	out.Range = Transform(e.Range, f).(*Subscript)
	return f(&out)
}

func (e *Projection) Transform(f TransformFunc) Expression {
	out := *e
	out.LHS = Transform(e.LHS, f)
	out.Object = Transform(e.Object, f).(*Object)
	return f(&out)
}

func (e *FunctionPipe) Transform(f TransformFunc) Expression {
	out := *e
	out.LHS = Transform(e.LHS, f)
	out.Func = Transform(e.Func, f).(*FunctionCall)
	return f(&out)
}

func transformExprs(in []Expression, f TransformFunc) []Expression {
	if len(in) == 0 {
		return nil
	}
	result := make([]Expression, len(in))
	for i, e := range in {
		result[i] = Transform(e, f)
	}
	return result
}
