use std::rc::Rc;

use unionfind::HashUnionFindByRank;

use crate::ast::{self, UnaOp, BinOp};
use super::*;

/// Storage for type-checking.
struct Storage {
    /// The type-checked version of every AST variable.
    names: HashMap<*const ast::Variable, Rc<Variable>>,

    /// The type of every variable in the AST.
    varts: HashMap<*const Variable, Type>,

    /// A union-find structure for equating scalar types.
    ///
    /// This structure groups together variables whose types have identical
    /// scalar components, allowing unknown types to be resolved.
    tvars: HashUnionFindByRank<*const Variable>,
}

impl Storage {
    /// Type-check the given function.
    fn tck_fn(&mut self, r#fn: &ast::Function) -> Result<Function> {
        let name = r#fn.name.clone();

        let args = r#fn.args.iter()
            // Prepare the type-checked version of the variable.
            .map(|arg| (arg, Variable {
                name: arg.variable.name.clone(),
                expr: Typed {
                    data: Expr::Arg,
                    r#type: arg.r#type.clone().into(),
                },
            }))
            .map(|(arg, var)| (arg, Rc::new(var)))
            // Update type-checker state with the new variable.
            .inspect(|(arg, var)| {
                let arg_id = Rc::as_ptr(arg.variable);
                let var_id = Rc::as_ptr(var);
                self.names.insert(arg_id, var.clone());
                self.varts.insert(var_id, var.r#type);
                self.tvars.add(var_id).unwrap();
            })
            .map(|(_, variable)| Argument { variable })
            .collect();

        let rett = r#fn.rett.into();

        let body = self.tck_expr(&r#fn.body, rett.scalar)?;

        Ok(Function { name, args, rett, body })
    }

    /// Type-check a statement.
    fn tck_stmt(&mut self, stmt: &ast::Stmt) -> Result<Stmt> {
        match stmt {
            ast::Stmt::Let(old) => {
                let new = Rc::new(Variable {
                    name: old.name.clone(),
                    expr: self.tck_expr(&old.expr, Type::Any)?,
                });
                let old_id = Rc::as_ptr(old);
                let new_id = Rc::as_ptr(new);
                self.names.insert(old_id, new.clone());
                self.varts.insert(new_id, var.expr.r#type);
                self.tvars.add(new_id).unwrap();
                Ok(Stmt::Let(new))
            },
        }

        Ok(())
    }

    /// Type-check an expression.
    ///
    /// Given a type the expression's scalar type must be a subtype of, the
    /// expression's type is resolved and returned.  Note that the type might
    /// not be fully resolved.
    fn tck_expr(
        &mut self,
        expr: &ast::Expr,
        sup: ScalarType,
    ) -> Result<(Typed<Expr>, Option<Rc<Variable>>)> {
        Ok(match expr {
            ast::Expr::Una(op, x) => {
                let x_sup = self.tck_una_sup(sup)?;
                let (x, xv) = self.tck_expr(x, x_sup)?;
                let o = self.tck_una_res(op, sup, x.r#type)?;
                let v = self.tck_una_var(op, xv);
                (Typed { data: Expr::Una(op, x), r#type: o }, v)
            },

            ast::Expr::Bin(op, [l, r]) => {
                let [l_sup, r_sup] = self.tck_bin_sup(op, sup)?;
                let (l, lv) = self.tck_expr(l, l_sup)?;
                let (r, rv) = self.tck_expr(r, r_sup)?;
                let o = self.tck_bin_res(op, sup, l.r#type, r.r#type)?;
                let v = self.tck_bin_var(op, lv, rv);
                (Typed {
                    data: Expr::Bin(op, [l, r].map(Box::new)),
                    r#type: o,
                }, v)
            },

            ast::Expr::Par(x) => self.tck_expr(x, sup),

            ast::Expr::Cast(t, x) => {
                let (x, _) = self.tck_expr(x, Type::Any)?;
                let t = Type::from(t);

                // Ensure that the output type fits the supertype.
                if !t.scalar.is_subtype_of(&sup) {
                    return Err(logic::Error::Subtype.into());
                }

                // It is not possible to cast from or into an option.
                if x.r#type.option.is_some() || t.option.is_some() {
                    return Err(logic::Error::Subtype.into());
                }

                // It is not possible to cast from or into a stream.
                if t.stream.is_some() || t.vector.is_some() {
                    if x.r#type.stream.is_some() != t.stream.is_some() {
                        return Err(logic::Error::Subtype.into());
                    }
                }

                (Typed { data: x.data, r#type: t }, None)
            },

            ast::Expr::Int(n) => {
                // Ensure that an integer is allowed here.
                if !Type::Int(IntType::Any).is_subtype_of(&sup) {
                    return Err(logic::Error::Subtype.into());
                }

                Typed {
                    data: Expr::Int(n),
                    r#type: Type {
                        scalar: Type::Int(IntType::Any),
                        option: None,
                        vector: None,
                        stream: None,
                    },
                }
            },

            ast::Expr::Var(v) => {
                // Resolve the expression node.
                let v = self.names.get(&Rc::as_ptr(v)).unwrap().clone();
                let x = self.tvars.find_shorten(&Rc::as_ptr(v)).unwrap();

                // Infer the underlying type value.
                let mut xt = self.types.get(&x).unwrap();
                xt.scalar = xt.scalar.infer_min(sup)?;
                self.types.insert(x, xt);

                Typed { data: Expr::Var(v), r#type: xt }
            },

            // 'Arg' is only possible within function argument definitions,
            // which are never visited by this function.
            ast::Expr::Arg(_) => unreachable!(),

            ast::Expr::Blk { stmts, rexpr } => {
                for stmt in stmts {
                    self.tck_stmt(stmt)?;
                }

                self.tck_expr(rexpr, sup)?
            },
        })
    }

    fn tck_una_sup(
        &mut self,
        op: UnaOp,
        sup: ScalarType,
    ) -> Result<ScalarType> {
        match op {
            UnaOp::Neg => ScalarType::merge_min(ScalarType::Int(None), sup),
            UnaOp::Not => ScalarType::merge_min(ScalarType::Int(None), sup),
        }
    }

    fn tck_una_res(
        &mut self,
        op: UnaOp,
        x: Type,
    ) -> Result<Type> {
        match op {
            UnaOp::Neg => Ok(x),
            UnaOp::Not => Ok(x),
        }
    }

    fn tck_una_var(
        &mut self,
        op: UnaOp,
        x: Option<Rc<Variable>>,
    ) -> Option<Rc<Variable>> {
        match op {
            UnaOp::Neg => x,
            UnaOp::Not => x,
        }
    }

    fn tck_bin_sup(
        &mut self,
        op: BinOp,
        sup: ScalarType,
    ) -> Result<[ScalarType; 2]> {
        Ok(match op {
            BinOp::Add | BinOp::Sub
                | BinOp::Mul | BinOp::Div | BinOp::Rem
                | UnaOp::And | UnaOp::IOr | UnaOp::XOr
                | UnaOp::ShL | UnaOp::ShR =>
                [ScalarType::merge_min(ScalarType::Int(None), sup)?; 2],

            BinOp::Cat => [sup; 2],
            BinOp::Ind => [sup, ScalarType::Int(None)],
            BinOp::Exp => [sup, ScalarType::Int(None)],
            BinOp::Red => [sup, ScalarType::Int(None)],

            BinOp::Cmp(_) => [ScalarType::Any; 2],

            BinOp::Cond => [ScalarType::Int(None), sup],
            BinOp::Else => [sup; 2],
        })
    }

    fn tck_bin_res(
        &mut self,
        op: BinOp,
        lhs: Type,
        rhs: Type,
    ) -> Result<Type> {
        Ok(match op {
            BinOp::Add | BinOp::Sub
                | BinOp::Mul | BinOp::Div | BinOp::Rem
                | UnaOp::And | UnaOp::IOr | UnaOp::XOr
                | UnaOp::ShL | UnaOp::ShR =>
                Type::merge_max(lhs, rhs)?,

            BinOp::Cat => Type {
                stream: Subtyping::merge_max(lhs.stream, rhs.stream)?,
                vector: Some(VectorPart {
                    size: [lhs, rhs].into_iter()
                        .map(|x| x.vector.map_or(1, |x| x.size))
                        .sum(),
                }),
                option: Subtyping::merge_max(lhs.option, rhs.option)?,
                scalar: Subtyping::merge_max(lhs.scalar, rhs.scalar)?,
            },

            BinOp::Ind => {
                if lhs.vector.is_none() {
                    return Err(logic::Error::Subtype.into());
                }

                Type {
                    stream: Subtyping::merge_max(lhs.stream, rhs.stream)?,
                    vector: rhs.vector,
                    option: Subtyping::merge_max(lhs.option, rhs.option)?,
                    scalar: lhs.scalar,
                }
            },

            BinOp::Exp => {
                if rhs.stream.is_none() {
                    return Err(logic::Error::Subtype.into());
                }

                Type {
                    stream: rhs.stream,
                    vector: VectorSize::merge_max(lhs.vector, rhs.vector)?,
                    option: Some(OptionPart {}),
                    scalar: lhs.scalar,
                }
            },

            BinOp::Red => {
                if lhs.stream.is_none() || rhs.stream.is_none() {
                    return Err(logic::Error::Subtype.into());
                }

                if rhs.option.is_some() {
                    return Err(logic::Error::Subtype.into());
                }

                Type {
                    stream: Some(StreamPart {}),
                    vector: Subtyping::merge_max(lhs.vector, rhs.vector)?,
                    option: lhs.option,
                    scalar: lhs.scalar,
                }
            },

            BinOp::Cmp(_) => Type {
                stream: Subtyping::merge_max(lhs.stream, rhs.stream)?,
                vector: Subtyping::merge_max(lhs.vector, rhs.vector)?,
                option: Subtyping::merge_max(lhs.option, rhs.option)?,
                scalar: ScalarType::Int(Some(IntType {
                    sign: IntSign::U,
                    size: NonZeroU32::MIN,
                })),
            },

            BinOp::Cond => Type {
                stream: Subtyping::merge_max(lhs.stream, rhs.stream)?,
                vector: Subtyping::merge_max(lhs.vector, rhs.vector)?,
                option: Some(OptionPart {}),
                scalar: rhs.scalar,
            },

            BinOp::Else => {
                if lhs.option.is_none() {
                    return Err(logic::Error::Subtype.into());
                }

                Type {
                    stream: Subtyping::merge_max(lhs.stream, rhs.stream)?,
                    vector: Subtyping::merge_max(lhs.vector, rhs.vector)?,
                    option: rhs.option,
                    scalar: Subtyping::merge_max(lhs.scalar, rhs.scalar)?,
                }
            },
        })
    }

    fn tck_bin_var(
        &mut self,
        op: BinOp,
        lhs: Option<Rc<Variable>>,
        rhs: Option<Rc<Variable>>,
    ) -> Result<Option<Rc<Variable>>> {
        let union = |lhs, rhs| {
            let [lhs, rhs] = [lhs?, rhs?].map(Rc::as_ptr);
            self.tvars.union_by_rank(&lhs, &rhs).unwrap();

        };

        Ok(match op {
            BinOp::Add | BinOp::Sub
                | BinOp::Mul | BinOp::Div | BinOp::Rem
                | UnaOp::And | UnaOp::IOr | UnaOp::XOr
                | UnaOp::ShL | UnaOp::ShR =>
                Type::merge_max(lhs, rhs)?,

            BinOp::Cat => Type {
                stream: Subtyping::merge_max(lhs.stream, rhs.stream)?,
                vector: Some(VectorPart {
                    size: [lhs, rhs].into_iter()
                        .map(|x| x.vector.map_or(1, |x| x.size))
                        .sum(),
                }),
                option: Subtyping::merge_max(lhs.option, rhs.option)?,
                scalar: Subtyping::merge_max(lhs.scalar, rhs.scalar)?,
            },

            BinOp::Ind => {
                if lhs.vector.is_none() {
                    return Err(logic::Error::Subtype.into());
                }

                Type {
                    stream: Subtyping::merge_max(lhs.stream, rhs.stream)?,
                    vector: rhs.vector,
                    option: Subtyping::merge_max(lhs.option, rhs.option)?,
                    scalar: lhs.scalar,
                }
            },

            BinOp::Exp => {
                if rhs.stream.is_none() {
                    return Err(logic::Error::Subtype.into());
                }

                Type {
                    stream: rhs.stream,
                    vector: VectorSize::merge_max(lhs.vector, rhs.vector)?,
                    option: Some(OptionPart {}),
                    scalar: lhs.scalar,
                }
            },

            BinOp::Red => lhs,
            BinOp::Cmp(_) => None,
            BinOp::Cond => rhs,

            BinOp::Else => {
                if lhs.option.is_none() {
                    return Err(logic::Error::Subtype.into());
                }

                Type {
                    stream: Subtyping::merge_max(lhs.stream, rhs.stream)?,
                    vector: Subtyping::merge_max(lhs.vector, rhs.vector)?,
                    option: rhs.option,
                    scalar: Subtyping::merge_max(lhs.scalar, rhs.scalar)?,
                }
            },
        })
    }

        let r#type = match **expr {
            ast::Expr::Not(x) => {
                // Resolve the sub-expression.
                let x_sup = Type::merge_min(Type::Int(IntType::Any), sup)?;
                self.tck_expr(x, x_sup)?
            },

            ast::Expr::Add(x) | ast::Expr::Sub(x)
                | ast::Expr::Mul(x) | ast::Expr::Div(x) | ast::Expr::Rem(x)
                | ast::Expr::And(x) | ast::Expr::IOr(x) | ast::Expr::XOr(x)
                | ast::Expr::ShL(x) | ast::Expr::ShR(x) => {

                // Resolve the sub-expressions.
                let x_sup = Type::merge_min(Type::Int(IntType::Any), sup)?;
                let lhs = self.tck_expr(x[0], x_sup)?;
                let rhs = self.tck_expr(x[1], x_sup)?;

                // Equate the sub-expressions' types.
                let res = self.merge_exprs(x[0].id(), x[1].id())?;

                // Incorporate any mapping components.
                MapType {
                    scalar: res,
                    option: lhs.option || rhs.option,
                    vector: VectorSize::merge_max(lhs.vector, rhs.vector)?,
                    stream: lhs.stream || rhs.stream,
                }
            },

            ast::Expr::Cat(x) => {
                // Resolve the sub-expressions.
                let lhs = self.tck_expr(x[0], sup)?;
                let rhs = self.tck_expr(x[1], sup)?;

                // Equate the sub-expressions' types.
                let res = self.merge_exprs(x[0].id(), x[1].id())?;

                // Calculate the new vector size.
                let vector_size =
                    lhs.vector.get().unwrap_or(1) +
                    rhs.vector.get().unwrap_or(1);

                // Incorporate any mapping components.
                MapType {
                    scalar: res,
                    option: lhs.option || rhs.option,
                    vector: Some(vector_size).into(),
                    stream: lhs.stream || rhs.stream,
                }
            },

            ast::Expr::Ind(x) => {
                // Resolve the sub-expressions.
                let lhs = self.tck_expr(x[0], sup)?;
                let rhs = self.tck_expr(x[1], Type::Int(IntType::Any))?;

                // Ensure that the left-hand side is an array.
                if !lhs.vector.exists() {
                    return Err(logic::Error::Subtype.into());
                }

                // Incorporate any mapping components.
                MapType {
                    scalar: lhs.scalar,
                    option: lhs.option || rhs.option,
                    vector: rhs.vector,
                    stream: lhs.stream || rhs.stream,
                }
            },

            ast::Expr::Exp(x) => {
                // Resolve the sub-expressions.
                let lhs = self.tck_expr(x[0], sup)?;
                let rhs = self.tck_expr(x[1], Type::Int(IntType::Any))?;

                // Ensure that the right-hand side is a stream.
                if !rhs.stream { return Err(logic::Error::Subtype.into()); }

                // Incorporate any mapping components.
                MapType {
                    scalar: lhs.scalar,
                    option: true,
                    vector: VectorSize::merge_max(lhs.vector, rhs.vector)?,
                    stream: true,
                }
            },

            ast::Expr::Red(x) => {
                // Resolve the sub-expressions.
                let lhs = self.tck_expr(x[0], sup)?;
                let rhs = self.tck_expr(x[1], Type::Int(IntType::Any))?;

                // Ensure that the left-hand side is a stream.
                if !lhs.stream { return Err(logic::Error::Subtype.into()); }

                // Ensure that the right-hand side is a stream.
                if !rhs.stream { return Err(logic::Error::Subtype.into()); }

                // Ensure that the right-hand side is not optional.
                if rhs.option { return Err(logic::Error::Subtype.into()); }

                // Incorporate any mapping components.
                MapType {
                    scalar: lhs.scalar,
                    option: lhs.option,
                    vector: VectorSize::merge_max(lhs.vector, rhs.vector)?,
                    stream: true,
                }
            },

            ast::Expr::IsEq(x) | ast::Expr::IsNE(x)
                | ast::Expr::IsLT(x) | ast::Expr::IsLE(x)
                | ast::Expr::IsGT(x) | ast::Expr::IsGE(x) => {

                // Resolve the sub-expressions.
                let lhs = self.tck_expr(x[0], sup)?;
                let rhs = self.tck_expr(x[1], sup)?;

                // Equate the sub-expressions' types.
                self.merge_exprs(x[0].id(), x[1].id())?;

                // Incorporate any mapping components.
                MapType {
                    scalar: Type::Int(IntType::U(NonZeroU32::MIN)),
                    option: lhs.option || rhs.option,
                    vector: VectorSize::merge_max(lhs.vector, rhs.vector)?,
                    stream: lhs.stream || rhs.stream,
                }
            }

            ast::Expr::Cond(x) => {
                // Resolve the sub-expressions.
                let lhs = self.tck_expr(x[0], Type::Int(IntType::Any))?;
                let rhs = self.tck_expr(x[1], sup)?;

                // Incorporate any mapping components.
                MapType {
                    scalar: rhs.scalar,
                    option: true,
                    vector: VectorSize::merge_max(lhs.vector, rhs.vector)?,
                    stream: lhs.stream || rhs.stream,
                }
            },

            ast::Expr::Else(x) => {
                // Resolve the sub-expressions.
                let lhs = self.tck_expr(x[0], sup)?;
                let rhs = self.tck_expr(x[1], sup)?;

                // Equate the sub-expressions' types.
                let res = self.merge_exprs(x[0].id(), x[1].id())?;

                // Ensure that the left-hand side is optional.
                if !lhs.option { return Err(logic::Error::Subtype.into()); }

                // Incorporate any mapping components.
                MapType {
                    scalar: res,
                    option: rhs.option,
                    vector: VectorSize::merge_max(lhs.vector, rhs.vector)?,
                    stream: lhs.stream || rhs.stream,
                }
            },

            ast::Expr::BitCast(t, x) => {
                // Resolve the sub-expression.
                let x = self.tck_expr(x, Type::Any)?;
                let t = MapType::from(t);

                // Ensure that the output type fits the supertype.
                if !t.scalar.is_subtype_of(&sup) {
                    return Err(logic::Error::Subtype.into());
                }

                // Ensure that the subexpression is not optional.
                if x.option { return Err(logic::Error::Subtype.into()); }

                // Ensure that the target type is not optional.
                if t.option { return Err(logic::Error::Subtype.into()); }

                // Ensure that both the source and the target types have the
                // same disposition to streams.
                if x.stream != t.stream {
                    return Err(logic::Error::Subtype.into());
                }

                t
            },

            ast::Expr::MapCast(t, x) => {
                // Resolve the sub-expression.
                let x = self.tck_expr(x, Type::Any)?;
                let t = Type::from(t);

                // Ensure that the output type fits the supertype.
                if !t.is_subtype_of(&sup) {
                    return Err(logic::Error::Subtype.into());
                }

                MapType {
                    scalar: t,
                    option: x.option,
                    vector: x.vector,
                    stream: x.stream,
                }
            },

            ast::Expr::Int(_) => {
                // Ensure that an integer is allowed here.
                if !Type::Int(IntType::Any).is_subtype_of(&sup) {
                    return Err(logic::Error::Subtype.into());
                }

                MapType {
                    scalar: Type::Int(IntType::Any),
                    option: false,
                    vector: None.into(),
                    stream: false,
                }
            },

            ast::Expr::Var(_, x) => self.infer_expr(x.id(), sup)?,

            // 'Arg' is only possible within function argument definitions,
            // which are never visited by this function.
            ast::Expr::Arg(_) => unreachable!(),

            ast::Expr::Blk { stmts, rexpr } => {
                for stmt in stmts {
                    self.tck_stmt(stmt)?;
                }

                self.tck_expr(rexpr, sup)?
            },
        };

        // Save the result for later and return.
        self.types[usize::from(expr.id())] = Some(r#type);
        Ok(r#type)
    }

    /// Merge the scalar types of the given expressions.
    fn merge_exprs(
        &mut self,
        lhs: ID<ast::Expr<'ast>>,
        rhs: ID<ast::Expr<'ast>>,
    ) -> Result<Type> {
        let [lhs, rhs] = [lhs, rhs].map(usize::from);

        // Resolve both nodes all the way in.
        let l = self.tvars.find_shorten(&lhs).unwrap();
        let r = self.tvars.find_shorten(&rhs).unwrap();

        // Merge the underlying type values.
        let [lt, rt] = [l, r].map(|x| self.types[x].unwrap().scalar);
        let xt = Type::merge_max(lt, rt)?;

        // Update the union-find to combine the two.
        self.tvars.union_by_rank(&l, &r).unwrap();

        // Update the types to use the merged result.
        self.types[l].as_mut().unwrap().scalar = xt;
        self.types[r].as_mut().unwrap().scalar = xt;

        Ok(xt)
    }

    /// Get the type of an expression.
    ///
    /// The function containing the expression must have already been resolved
    /// using [`Storage::tck_fn()`].
    pub fn get_expr_type(&mut self, expr: ID<ast::Expr<'ast>>) -> MapType {
        let expr = self.tvars.find_shorten(&usize::from(expr)).unwrap();
        self.types[expr].unwrap()
    }
}
