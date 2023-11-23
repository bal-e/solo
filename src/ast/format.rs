//! Textual representation of ASTs.

use core::fmt;

use super::*;

/// Format the AST of a module.
pub fn format_module(w: impl fmt::Write, ast: &Module) -> fmt::Result {
    let mut this = Formatter { inner: w, indent: String::new() };
    this.format_module(ast)
}

/// A syntactic formatter for ASTs.
struct Formatter<W: fmt::Write> {
    /// The underlying writer.
    inner: W,

    /// The current indentation.
    indent: String,
}

impl<W: fmt::Write> Formatter<W> {
    /// Format the AST of a module.
    fn format_module(&mut self, ast: &Module) -> fmt::Result {
        self.inner.write_str("mod ")?;
        self.format_ident(&ast.name)?;
        self.inner.write_char(' ')?;
        self.write_block(|this| {
            for (i, function) in ast.functions.iter().enumerate() {
                if i != 0 {
                    this.inner.write_char('\n')?;
                }

                this.write_indent()?;
                this.format_function(function)?;
                this.inner.write_char('\n')?;
            }

            Ok(())
        })
    }

    /// Format the AST of a function.
    fn format_function(&mut self, ast: &Function) -> fmt::Result {
        self.inner.write_str("fn ")?;
        self.format_ident(&ast.name)?;

        self.inner.write_char('(')?;
        for (i, argument) in ast.args.iter().enumerate() {
            if i != 0 {
                self.inner.write_str(", ")?;
            }

            self.format_argument(argument)?;
        }
        self.inner.write_str(") ")?;

        self.format_expr(&**ast.body, Prec::Min)
    }

    /// Format the AST of an argument.
    fn format_argument(&mut self, ast: &Argument) -> fmt::Result {
        self.format_ident(&ast.variable.name)?;
        self.inner.write_str(": ")?;
        self.format_mapped_type(&ast.r#type)?;
        Ok(())
    }

    /// Format the AST of a mapped type.
    fn format_mapped_type(&mut self, ast: &MappedType) -> fmt::Result {
        let _ = ast;
        self.inner.write_str("<type>")
    }

    /// Format the AST of a statement.
    fn format_stmt(&mut self, ast: &Stmt) -> fmt::Result {
        match ast {
            Stmt::Let(var) => {
                self.format_ident(&var.name)?;
                self.inner.write_str(" := ")?;
                self.format_expr(&**var.expr, Prec::Min)?;
                self.inner.write_str(";")?;
                Ok(())
            },
        }
    }

    /// Format the AST of an expression.
    fn format_expr(&mut self, ast: &Expr, prec: Prec) -> fmt::Result {
        match ast {
            Expr::Una(uop, src) => {
                let _ = (uop, src);
                self.inner.write_str("una expr")
            },

            Expr::Bin(bop, src) => {
                let _ = (bop, src);
                self.inner.write_str("bin expr")
            },

            Expr::Par(src) => {
                self.inner.write_char('(')?;
                self.format_expr(&**src, Prec::Min)?;
                self.inner.write_char(')')?;
                Ok(())
            },

            Expr::Cast(r#type, src) => {
                self.inner.write_char('(')?;
                self.format_mapped_type(&r#type)?;
                self.inner.write_str(") ")?;
                self.format_expr(&**src, Prec::Max)?;
                Ok(())
            },

            Expr::Int(val) => {
                write!(self.inner, "{}", val)
            },

            Expr::Var(var) => {
                self.format_ident(&var.name)
            },

            Expr::Arg => unreachable!(),

            Expr::Blk { stmts, rexpr } => {
                self.write_block(|this| {
                    for stmt in stmts {
                        this.write_indent()?;
                        this.format_stmt(stmt)?;
                        this.inner.write_char('\n')?;
                    }

                    this.write_indent()?;
                    this.format_expr(&**rexpr, Prec::Min)?;
                    this.inner.write_char('\n')
                })
            },
        }
    }

    /// Format the AST of an identifier.
    fn format_ident(&mut self, ast: &str) -> fmt::Result {
        self.inner.write_str(ast)
    }

    /// Write a braced block.
    fn write_block<F>(&mut self, f: F) -> fmt::Result
    where F: FnOnce(&mut Self) -> fmt::Result {
        self.inner.write_str("{\n")?;
        let outer_len = self.indent.len();
        self.indent.push_str("    ");
        (f)(self)?;
        self.indent.truncate(outer_len);
        self.write_indent()?;
        self.inner.write_str("}")?;
        Ok(())
    }

    /// Write indent.
    fn write_indent(&mut self) -> fmt::Result {
        self.inner.write_str(&self.indent)
    }
}
