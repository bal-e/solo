//! The Medium Intermediary Representation (MIR).

use unionfind::VecUnionFind;

use crate::{ast, tck::{self, logic::*}};
use crate::storage::*;

/// Storage for the MIR.
struct Storage<'ast: 'tck, 'tck> {
    /// Storage for the AST.
    ast: &'ast ast::Storage<'ast>,

    /// Storage for type information.
    tck: &'tck tck::Storage<'ast>,

    /// The current variable counter.
    next_var: u32,

    /// Equivalent IR variables.
    names: VecUnionFind<usize>,

    /// Single-run instructions.
    singles: Vec<String>,

    /// Streaming instructions.
    streams: Vec<String>,
}

impl<'ast: 'tck, 'tck> Storage<'ast, 'tck> {
    /// Print the given function.
    pub fn print_fn(&mut self, r#fn: &'ast Stored<ast::Fn<'ast>>) {
        print!("define {} @{} (",
               Self::ffi_type(r#fn.rett.into()),
               r#fn.name);

        for (i, arg) in r#fn.args.into_iter().enumerate() {
            if i != 0 {
                print!(", ");
            }

            print!("{} %v{}",
                Self::ffi_type(arg.r#type.into()),
                usize::from(arg.expr.id()));
            self.names.add(arg.expr.id().into()).unwrap();
        }

        println!(") {{");

        self.parse_expr(r#fn.body);

        println!("entry:");
        for single in &self.singles {
            println!("  {}", single);
        }
        println!("  br label %loop");

        println!("loop:");
        println!("  %i = phi i64 [0, %entry], [%i1, %loop]");
        println!("  %i1 = add nuw nsw i64 %i, 1");
        for stream in &self.streams {
            println!("  {}", stream);
        }
        println!("  br label %loop");

        println!("}}");
    }

    fn parse_stmt(&mut self, stmt: &'ast Stored<ast::Stmt<'ast>>) {
        todo!()
    }

    fn parse_expr(&mut self, expr: &'ast Stored<ast::Expr<'ast>>) {
        match **expr {
            ast::Expr::Not(x) => {
                self.parse_expr(x);

                let t = self.tck.get_expr_type(expr.id());
                let o = if t.stream {
                    &mut self.streams
                } else {
                    &mut self.singles
                };

                let sts = Self::ffi_scalar_type(t.scalar);
                let (dts, mts) = match t.vector.get() {
                    Some(s) => (
                        format!("<{} x {}>", s, sts),
                        format!("<{} x i1>", s)),
                    None => (sts, "i1".to_string()),
                };

                if t.option {
                    let ots = format!("{{ {}, {} }}", dts, mts);
                    let ia = self.names.find(&x.id().into()).unwrap();
                    let ib = self.next_var + 0;
                    o.push(format!("%{} = extractvalue {} %v{}, 0", ib, ots, ia));
                    let ic = self.next_var + 1;
                    o.push(format!("%{} = xor {} %{}, -1", ic, dts, ib));
                    let id = usize::from(expr.id());
                    o.push(format!("%v{} = insertvalue {} %v{}, {} %{}, 0", id, ots, ia, dts, ic));
                    self.names.add(id).unwrap();
                    self.next_var += 2;
                } else {
                    let ia = self.names.find(&x.id().into()).unwrap();
                    let ib = usize::from(expr.id());
                    o.push(format!("%v{} = xor {} %v{}, -1", ib, dts, ia));
                    self.names.add(ib).unwrap();
                }
            },

            ast::Expr::Var(_, x) => {
                let t = self.tck.get_expr_type(expr.id());
                if !t.stream || !matches!(**x, ast::Expr::Arg(_)) {
                    let ia = usize::from(x.id());
                    let ib = usize::from(expr.id());
                    self.names.add(ib).unwrap();
                    self.names.union_by(&ia, &ib, |x, _| x).unwrap();
                    return;
                }

                let sts = Self::ffi_scalar_type(t.scalar);
                let (dts, mts) = match t.vector.get() {
                    Some(s) => (
                        format!("<{} x {}>", s, sts),
                        format!("<{} x i1>", s)),
                    None => (sts, "i1".to_string()),
                };

                if t.option {
                    let ia = self.names.find(&x.id().into()).unwrap();
                    let ib = self.next_var + 0;
                    self.streams.push(format!("%{} = extractvalue {{ i64, ptr, ptr }} %v{}, 1", ib, ia));
                    let ic = self.next_var + 1;
                    self.streams.push(format!("%{} = getelementptr {}, ptr %{}, i64 %i", ic, dts, ib));
                    let id = self.next_var + 2;
                    self.streams.push(format!("%{} = load {}, ptr %{}", id, dts, ic));
                    let ie = self.next_var + 3;
                    self.streams.push(format!("%{} = extractvalue ptr %v{}, 2", ie, ia));
                    let ig = self.next_var + 4;
                    self.streams.push(format!("%{} = getelementptr {}, ptr %{}, i64 %i", ig, mts, ie));
                    let ih = self.next_var + 5;
                    self.streams.push(format!("%{} = load {}, ptr %{}", ih, dts, ig));
                    let ii = usize::from(expr.id());
                    self.streams.push(format!("%v{} = {{ {} %{}, {} %{} }}", ii, dts, id, mts, ih));
                    self.names.add(ii).unwrap();
                    self.next_var += 6;
                } else {
                    let ia = self.names.find(&x.id().into()).unwrap();
                    let ib = self.next_var + 0;
                    self.streams.push(format!("%{} = extractvalue {{ i64, ptr }} %v{}, 1", ib, ia));
                    let ic = self.next_var + 1;
                    self.streams.push(format!("%{} = getelementptr {}, ptr %{}, i64 %i", ic, dts, ib));
                    let id = usize::from(expr.id());
                    self.streams.push(format!("%v{} = load {}, ptr %{}", id, dts, ic));
                    self.names.add(id).unwrap();
                    self.next_var += 2;
                }
            },

            ast::Expr::Blk { stmts, rexpr } => {
                for stmt in stmts {
                    self.parse_stmt(stmt);
                }

                self.parse_expr(rexpr);
            },

            _ => todo!(),
        }
    }

    /// Print the given type for FFI.
    fn ffi_type(r#type: MapType) -> String {
        if r#type.stream {
            if r#type.option {
                // Track the mask separately.
                format!("{{ i64, ptr, ptr }}")
            } else {
                // Just count the number of elements.
                format!("{{ i64, ptr }}")
            }
        } else if let Some(size) = r#type.vector.get() {
            if r#type.option {
                format!("{{ <{} x {}>, <{} x i1> }}",
                        size,
                        Self::ffi_scalar_type(r#type.scalar),
                        size)
            } else {
                format!("<{} x {}>", size, Self::ffi_scalar_type(r#type.scalar))
            }
        } else if r#type.option {
            format!("{{ {}, i1 }}", Self::ffi_scalar_type(r#type.scalar))
        } else {
            Self::ffi_scalar_type(r#type.scalar)
        }
    }

    /// Print the given scalar type for FFI.
    fn ffi_scalar_type(r#type: Type) -> String {
        match r#type {
            Type::Int(t) => Self::ffi_int_type(t),
            Type::Any => unreachable!(),
        }
    }

    /// Print the given integer type for FFI.
    fn ffi_int_type(r#type: IntType) -> String {
        match r#type {
            IntType::U(s) => format!("i{}", s),
            IntType::S(s) => format!("i{}", s),
            IntType::Any => unreachable!(),
        }
    }
}

/// Print the given function.
pub fn print_fn<'ast, 'tck>(
    ast: &'ast ast::Storage<'ast>,
    tck: &'tck tck::Storage<'ast>,
    r#fn: &'ast Stored<ast::Fn<'ast>>,
) {
    let mut storage = Storage {
        ast,
        tck,
        next_var: 0,
        names: VecUnionFind::new([]).unwrap(),
        singles: Vec::new(),
        streams: Vec::new(),
    };

    storage.print_fn(r#fn)
}
