//! Syntactic printing for Solo ASTs.

use std::fmt;

use super::*;

/// An AST visitor that prints to the given output.
pub struct Printer<'a, W: ?Sized + fmt::Write> {
    /// The writer data is output to, or an error.
    writer: Result<&'a mut W, fmt::Error>,
    /// The indentation at which to write.
    indent: usize,
}

impl<'a, W: ?Sized + fmt::Write> Printer<'a, W> {
    /// Construct a new [`Printer`].
    pub fn new(writer: &'a mut W) -> Self {
        Self {
            writer: Ok(writer),
            indent: 0,
        }
    }

    fn write_str(&mut self, data: &str) {
        if let Ok(w) = &mut self.writer {
            if let Err(e) = w.write_str(data) {
                self.writer = Err(e);
            }
        }
    }

    fn write_fmt(&mut self, data: fmt::Arguments<'_>) {
        if let Ok(w) = &mut self.writer {
            if let Err(e) = w.write_fmt(data) {
                self.writer = Err(e);
            }
        }
    }

    fn write_newline(&mut self) {
        self.write_str("\n");
        for _ in 0 .. self.indent {
            self.write_str("    ");
        }
    }
}

impl<'ast, 'a, W: ?Sized + fmt::Write> Visit<'ast> for Printer<'a, W> {
    fn visit_module(&mut self, s: &Storage<'ast>, i: Module<'ast>) {
        for (index, func) in i.funcs.iter().enumerate() {
            if index != 0 {
                self.write_newline();
            }

            self.visit_func(s, s.funcs.get(func).clone());
            self.write_str("\n");
        }
    }

    fn visit_func(&mut self, s: &Storage<'ast>, i: Function<'ast>) {
        self.write_str("fn ");
        self.visit_name(s, i.name);
        self.write_str("(");
        for (index, arg) in i.args.iter().enumerate() {
            if index != 0 {
                self.write_str(", ");
            }

            let arg = s.func_args.get(arg);
            self.visit_name(s, arg.0);
            self.write_str(": ");
            self.visit_type(s, arg.1.clone());
        }
        self.write_str(") -> ");
        self.visit_type(s, i.rett);
        self.write_str(" ");
        self.visit_expr(s, s.exprs.get(i.body).clone());
    }

    fn visit_type(&mut self, s: &Storage<'ast>, i: Type) {
        if i.stream {
            self.write_str("[]");
        }

        self.visit_scalar_type(s, i.scalar);
    }

    fn visit_scalar_type(&mut self, _: &Storage<'ast>, i: ScalarType) {
        match i {
            ScalarType::U64 => {
                self.write_str("u64");
            },
        }
    }

    fn visit_stmt(&mut self, s: &Storage<'ast>, i: Stmt<'ast>) {
        match i {
            Stmt::Let(name, expr) => {
                self.write_str("let ");
                self.visit_name(s, name);
                self.write_str(" = ");
                self.visit_expr(s, s.exprs.get(expr).clone());
            },
        }
        self.write_str(";");
    }

    fn visit_expr(&mut self, s: &Storage<'ast>, i: Expr<'ast>) {
        fn visit_inner<'ast, 'a, W: ?Sized + fmt::Write>(
            v: &mut Printer<'a, W>,
            s: &Storage<'ast>,
            i: Expr<'ast>,
            p: (Prec, Prec),
        ) {
            let (prec, code) = (i.prec(), i.code());
            if Prec::cmp(p.0, prec) != Some(Assoc::Right)
            || Prec::cmp(prec, p.1) != Some(Assoc::Left) {
                v.write_str("(");
                visit_inner(v, s, i, (Prec::Min, Prec::Min));
                v.write_str(")");
                return;
            }

            match i {
                Expr::Not(x) => {
                    v.write_str("-");
                    let x = s.exprs.get(x).clone();
                    visit_inner(v, s, x, (Prec::Max, Prec::Max));
                },

                Expr::Add(x) | Expr::Sub(x) |
                Expr::Mul(x) | Expr::Div(x) | Expr::Rem(x) |
                Expr::And(x) | Expr::IOr(x) | Expr::XOr(x) |
                Expr::ShL(x) | Expr::ShR(x) |
                Expr::IsEq(x) | Expr::IsNE(x) |
                Expr::IsLT(x) | Expr::IsLE(x) |
                Expr::IsGT(x) | Expr::IsGE(x) => {
                    let [l, r] = x.map(|e| s.exprs.get(e).clone());
                    visit_inner(v, s, l, (p.0, prec));
                    v.write_str(" ");
                    v.write_str(code.unwrap());
                    v.write_str(" ");
                    visit_inner(v, s, r, (prec, p.1));
                },

                Expr::Int(i) => {
                    v.write_fmt(format_args!("{}", i));
                },

                Expr::Var(n) => {
                    v.visit_name(s, n);
                },

                Expr::Blk { stmts, rexpr } => {
                    v.write_str("{");
                    v.indent += 1;
                    v.write_newline();
                    for stmt in stmts.iter() {
                        v.visit_stmt(s, s.stmts.get(stmt).clone());
                        v.write_newline();
                    }
                    v.visit_expr(s, s.exprs.get(rexpr).clone());
                    v.indent -= 1;
                    v.write_newline();
                    v.write_str("}");
                },
            }
        }

        visit_inner(self, s, i, (Prec::Min, Prec::Min))
    }

    fn visit_name(&mut self, s: &Storage<'ast>, i: Symbol) {
        self.write_str(s.syms.resolve(i));
    }
}
