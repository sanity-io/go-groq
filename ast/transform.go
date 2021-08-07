package ast

type TransformFunc func(Expression) Expression

// Transform walks an expression depth-first.
func Transform(expr Expression, f TransformFunc) Expression {
	if expr == nil {
		return nil
	}
	return expr.Transform(f)
}

func (e *Everything) Transform(f TransformFunc) Expression     { return e }
func (e *This) Transform(f TransformFunc) Expression           { return e }
func (e *Ellipsis) Transform(f TransformFunc) Expression       { return e }
func (e *Parent) Transform(f TransformFunc) Expression         { return e }
func (e *StringLiteral) Transform(f TransformFunc) Expression  { return e }
func (e *FloatLiteral) Transform(f TransformFunc) Expression   { return e }
func (e *BooleanLiteral) Transform(f TransformFunc) Expression { return e }
func (e *IntegerLiteral) Transform(f TransformFunc) Expression { return e }
func (e *NullLiteral) Transform(f TransformFunc) Expression    { return e }
func (e *Attribute) Transform(f TransformFunc) Expression      { return e }

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
	for idx, expr := range e.Members {
		e.Members[idx] = Transform(expr, f)
	}
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
	newExprs := make([]Expression, len(e.Expressions))
	for i, x := range e.Expressions {
		newExprs[i] = Transform(x, f)
	}
	out := *e
	out.Expressions = newExprs
	return &out
}

func (e *Array) Transform(f TransformFunc) Expression {
	newExprs := make([]Expression, len(e.Expressions))
	for i, x := range e.Expressions {
		newExprs[i] = Transform(x, f)
	}
	out := *e
	out.Expressions = newExprs
	return f(&out)
}

func (e *FunctionCall) Transform(f TransformFunc) Expression {
	newArgs := make([]Expression, len(e.Arguments))
	for i, x := range e.Arguments {
		newArgs[i] = Transform(x, f)
	}
	out := *e
	out.Arguments = newArgs
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
