use core::fmt::{self, Write};

use crate::prec::*;

use super::*;

impl Module {
    /// Write out the syntax for this node.
    pub fn write_syntax<W: fmt::Write>(
        &self,
        w: &mut Writer<'_, W>,
    ) -> fmt::Result {
        let functions = w.storage.functions.get_seq(self.functions);

        w.write_str("mod ")?;
        w.write_str(&self.name)?;
        w.write_str(" ")?;
        w.write_block(|w| {
            for (i, function) in functions.iter().enumerate() {
                if i != 0 {
                    w.write_char('\n')?;
                }

                w.write_indent()?;
                function.write_syntax(w)?;
                w.write_char('\n')?;
            }

            Ok(())
        })
    }
}

impl Function {
    /// Write out the syntax for this node.
    pub fn write_syntax<W: fmt::Write>(
        &self,
        w: &mut Writer<'_, W>,
    ) -> fmt::Result {
        let arguments = w.storage.arguments.get_seq(self.args);
        let body = w.storage.exprs.get(self.body);

        write!(w, "fn {}(", self.name)?;
        for (i, argument) in arguments.iter().enumerate() {
            if i != 0 {
                w.write_str(", ")?;
            }

            argument.write_syntax(w)?;
        }

        write!(w, ") -> {} ", self.rett)?;

        body.write_syntax(w)
    }
}

impl Argument {
    /// Write out the syntax for this node.
    pub fn write_syntax<W: fmt::Write>(
        &self,
        w: &mut Writer<'_, W>,
    ) -> fmt::Result {
        let variable = w.storage.variables.get(self.variable);

        write!(w, "{}: {}", variable.name, self.r#type)
    }
}

impl Stmt {
    /// Write out the syntax for this node.
    pub fn write_syntax<W: fmt::Write>(
        &self,
        w: &mut Writer<'_, W>,
    ) -> fmt::Result {
        match *self {
            Self::Let(variable) => {
                let variable = w.storage.variables.get(variable);
                let expr = w.storage.exprs.get(variable.expr);

                write!(w, "{} :=", variable.name)?;
                expr.write_syntax(w)?;
                write!(w, ";")?;
                Ok(())
            },
        }
    }
}

impl Expr {
    /// Write out the syntax for this node.
    pub fn write_syntax<W: fmt::Write>(
        &self,
        w: &mut Writer<'_, W>,
    ) -> fmt::Result {
        match *self {
            Expr::Una(uop, src) => {
                let src = w.storage.exprs.get(src[0]);

                w.write_str(uop.syntax())?;
                w.write_parens_maybe(
                    Prec::cmp(uop.prec(), src.prec()) != Some(Assoc::Right),
                    |w| src.write_syntax(w))?;

                Ok(())
            },

            Expr::Bin(bop, src) => {
                let src = src.map(|x| w.storage.exprs.get(x));

                w.write_parens_maybe(
                    Prec::cmp(src[0].prec(), bop.prec()) != Some(Assoc::Left),
                    |w| src[0].write_syntax(w))?;
                write!(w, " {} ", bop.syntax())?;
                w.write_parens_maybe(
                    Prec::cmp(bop.prec(), src[1].prec()) != Some(Assoc::Right),
                    |w| src[1].write_syntax(w))?;

                Ok(())
            },

            Expr::Par(src) => {
                let src = w.storage.exprs.get(src);
                w.write_parens(|w| src.write_syntax(w))
            },

            Expr::BitCast(r#type, src) => {
                let src = w.storage.exprs.get(src);

                w.write_parens(|w| write!(w, "{}", r#type))?;
                w.write_char(' ')?;
                w.write_parens_maybe(
                    Prec::cmp(Prec::UnaPre, src.prec()) != Some(Assoc::Right),
                    |w| src.write_syntax(w))?;

                Ok(())
            },

            Expr::Int(ref val) => {
                write!(w, "{}", val)
            },

            Expr::Var(variable) => {
                let variable = w.storage.variables.get(variable);
                write!(w, "{}", variable.name)
            },

            Expr::Arg => unreachable!(),

            Expr::Blk { stmts, rexpr } => {
                let stmts = w.storage.stmts.get_seq(stmts);
                let rexpr = w.storage.exprs.get(rexpr);

                w.write_block(|w| {
                    for stmt in stmts {
                        w.write_indent()?;
                        stmt.write_syntax(w)?;
                        w.write_char('\n')?;
                    }

                    w.write_indent()?;
                    rexpr.write_syntax(w)?;
                    w.write_char('\n')
                })
            },
        }
    }
}

/// A writer for Solo syntax.
pub struct Writer<'ast, W: fmt::Write> {
    /// The underlying AST.
    pub storage: &'ast Storage,

    /// The underlying writer.
    writer: W,

    /// The current indentation.
    indent: u16,
}

impl<'ast, W: fmt::Write> Writer<'ast, W> {
    /// Construct a new [`Writer`].
    pub fn new(storage: &'ast Storage, writer: W) -> Self {
        Self { storage, writer, indent: 0 }
    }

    /// Write a braced block.
    fn write_block<F>(&mut self, f: F) -> fmt::Result
    where F: FnOnce(&mut Self) -> fmt::Result {
        self.write_str("{\n")?;
        let indent = self.indent;
        self.indent += 1;
        (f)(self)?;
        self.indent = indent;
        self.write_indent()?;
        self.write_char('}')?;
        Ok(())
    }

    /// Write indentation.
    fn write_indent(&mut self) -> fmt::Result {
        for _ in 0 .. self.indent {
            self.write_str("    ")?;
        }

        Ok(())
    }

    /// Wrap in parentheses.
    fn write_parens<F>(&mut self, f: F) -> fmt::Result
    where F: FnOnce(&mut Self) -> fmt::Result {
        self.write_char('(')?;
        (f)(self)?;
        self.write_char(')')?;
        Ok(())
    }

    /// Conditionally wrap in parentheses.
    fn write_parens_maybe<F>(&mut self, c: bool, f: F) -> fmt::Result
    where F: FnOnce(&mut Self) -> fmt::Result {
        if c { self.write_char('(')?; }
        (f)(self)?;
        if c { self.write_char(')')?; }
        Ok(())
    }
}

impl<'ast, W: fmt::Write> fmt::Write for Writer<'ast, W> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.writer.write_str(s)
    }

    fn write_char(&mut self, c: char) -> fmt::Result {
        self.writer.write_char(c)
    }

    fn write_fmt(&mut self, args: fmt::Arguments<'_>) -> fmt::Result {
        self.writer.write_fmt(args)
    }
}
