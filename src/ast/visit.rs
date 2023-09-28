//! Traversal of Solo ASTs.

use num_bigint::BigInt;
use symbol_table::Symbol;

use super::*;

pub trait Visit<'ast> {
    fn visit_module(&mut self, s: &Storage<'ast>, i: Module<'ast>) {
        self::visit_module(self, s, i)
    }

    fn visit_func(&mut self, s: &Storage<'ast>, i: Function<'ast>) {
        self::visit_func(self, s, i)
    }

    fn visit_func_arg(&mut self, s: &Storage<'ast>, i: (Symbol, Type)) {
        self::visit_func_arg(self, s, i)
    }

    fn visit_type(&mut self, s: &Storage<'ast>, i: Type) {
        self::visit_type(self, s, i)
    }

    fn visit_scalar_type(&mut self, s: &Storage<'ast>, i: ScalarType) {
        self::visit_scalar_type(self, s, i)
    }

    fn visit_stmt(&mut self, s: &Storage<'ast>, i: Stmt<'ast>) {
        self::visit_stmt(self, s, i)
    }

    fn visit_expr(&mut self, s: &Storage<'ast>, i: Expr<'ast>) {
        self::visit_expr(self, s, i)
    }

    fn visit_int(&mut self, s: &Storage<'ast>, i: BigInt) {
        self::visit_int(self, s, i)
    }

    fn visit_name(&mut self, s: &Storage<'ast>, i: Symbol) {
        self::visit_name(self, s, i)
    }
}

pub fn visit_module<'ast, V>(v: &mut V, s: &Storage<'ast>, i: Module<'ast>)
where V: ?Sized + Visit<'ast> {
    i.funcs.iter()
        .map(|func| s.funcs.get(func).clone())
        .for_each(|func| v.visit_func(s, func));
}

pub fn visit_func<'ast, V>(v: &mut V, s: &Storage<'ast>, i: Function<'ast>)
where V: ?Sized + Visit<'ast> {
    i.args.iter()
        .map(|arg| s.func_args.get(arg).clone())
        .for_each(|func| v.visit_func_arg(s, func));
    v.visit_type(s, i.rett);
    v.visit_expr(s, s.exprs.get(i.body).clone());
}

pub fn visit_func_arg<'ast, V>(v: &mut V, s: &Storage<'ast>, i: (Symbol, Type))
where V: ?Sized + Visit<'ast> {
    v.visit_name(s, i.0);
    v.visit_type(s, i.1);
}

pub fn visit_type<'ast, V>(v: &mut V, s: &Storage<'ast>, i: Type)
where V: ?Sized + Visit<'ast> {
    v.visit_scalar_type(s, i.scalar);
}

pub fn visit_scalar_type<'ast, V>(v: &mut V, s: &Storage<'ast>, i: ScalarType)
where V: ?Sized + Visit<'ast> {
    let _ = (v, s, i);
}

pub fn visit_stmt<'ast, V>(v: &mut V, s: &Storage<'ast>, i: Stmt<'ast>)
where V: ?Sized + Visit<'ast> {
    match i {
        Stmt::Let(name, expr) => {
            v.visit_name(s, name);
            v.visit_expr(s, s.exprs.get(expr).clone());
        },
    }
}

pub fn visit_expr<'ast, V>(v: &mut V, s: &Storage<'ast>, i: Expr<'ast>)
where V: ?Sized + Visit<'ast> {
    match i {
        Expr::Not(x) => {
            v.visit_expr(s, s.exprs.get(x).clone());
        },

        Expr::Add(x) | Expr::Sub(x) |
        Expr::Mul(x) | Expr::Div(x) | Expr::Rem(x) |
        Expr::And(x) | Expr::IOr(x) | Expr::XOr(x) |
        Expr::ShL(x) | Expr::ShR(x) |
        Expr::IsEq(x) | Expr::IsNE(x) |
        Expr::IsLT(x) | Expr::IsLE(x) |
        Expr::IsGT(x) | Expr::IsGE(x) => {
            let [l, r] = x.map(|e| s.exprs.get(e).clone());
            v.visit_expr(s, l);
            v.visit_expr(s, r);
        },

        Expr::Int(i) => {
            v.visit_int(s, i);
        },

        Expr::Var(n) => {
            v.visit_name(s, n);
        },

        Expr::Blk { stmts, rexpr } => {
            stmts.iter()
                .map(|stmt| s.stmts.get(stmt).clone())
                .for_each(|stmt| v.visit_stmt(s, stmt));
            v.visit_expr(s, s.exprs.get(rexpr).clone());
        },
    }
}

pub fn visit_int<'ast, V>(v: &mut V, s: &Storage<'ast>, i: BigInt)
where V: ?Sized + Visit<'ast> {
    let _ = (v, s, i);
}

pub fn visit_name<'ast, V>(v: &mut V, s: &Storage<'ast>, i: Symbol)
where V: ?Sized + Visit<'ast> {
    let _ = (v, s, i);
}
