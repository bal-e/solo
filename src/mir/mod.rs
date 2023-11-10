//! The Medium Intermediary Representation (MIR).

use num_bigint::BigInt;

use crate::{ast, tck::{self, logic::MapType}};
use crate::storage::*;

/// Storage for the MIR.
struct Storage<'mir> {
    /// Single operations.
    singles: ObjectStorage<SingleInst<'mir>>,

    /// Streaming operations.
    streams: ObjectStorage<StreamInst<'mir>>,
}

/// A reference to an arbitrary operation.
enum InstRef<'mir> {
    Single(&'mir Stored<SingleInst<'mir>>),
    Stream(&'mir Stored<StreamInst<'mir>>),
}

/// A single operation.
enum SingleInst<'mir> {
    /// A unary scalar operation.
    Una(ScalarUnaOp, &'mir Stored<Self>),
    /// A binary scalar operation.
    Bin(ScalarBinOp, [&'mir Stored<Self>; 2]),
    /// An integer literal.
    Int(BigInt),
    /// A function argument.
    Arg,
}

/// A streaming operation.
enum StreamInst<'mir> {
    /// The streamed result of a single operation.
    Map(&'mir Stored<SingleInst<'mir>>),
    /// A unary streaming operation.
    Una(ScalarUnaOp, &'mir Stored<Self>),
    /// A binary streaming operation.
    Bin(ScalarBinOp, [&'mir Stored<Self>; 2]),
}

/// A unary scalar operation.
enum ScalarUnaOp {
    Not,

    /// Add a vector component.
    Vec(u32),
    /// Add an option component.
    Opt,
}

/// A binary scalar operation.
enum ScalarBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    And,
    IOr,
    XOr,
    ShL,
    ShR,

    Cat,
    Ind,

    IsEq,
    IsNE,
    IsLT,
    IsLE,
    IsGT,
    IsGE,

    Cond,
    Else,
}

struct Parser<'ast, 'tck, 'mir> {
    /// Storage for the AST.
    ast: &'ast ast::Storage<'ast>,

    /// Storage for type information.
    tck: &'tck tck::Storage<'ast>,

    /// Storage for the MIR.
    mir: &'mir Storage<'mir>,

    /// A mapping from AST expressions to single operations.
    to_singles: Vec<Option<&'mir Stored<SingleInst<'mir>>>>,

    /// A mapping from AST expressions to streaming operations.
    to_streams: Vec<Option<&'mir Stored<StreamInst<'mir>>>>,
}

impl<'ast, 'tck, 'mir> Parser<'ast, 'tck, 'mir> {
    /// Parse the given function.
    fn parse_fn(
        &mut self,
        r#fn: &'ast Stored<ast::Fn<'ast>>,
    ) -> InstRef<'mir> {
        // Initialize the converters.
        self.to_singles.resize_with(self.ast.exprs.num(), Default::default);
        self.to_streams.resize_with(self.ast.exprs.num(), Default::default);

        // Add arguments as scalar instructions.
        for arg in r#fn.args {
            let single = self.mir.singles.store(SingleInst::Arg);
            self.to_singles[usize::from(arg.expr.id())] = Some(single);
        }

        // Begin parsing the function body.
        self.parse_expr(r#fn.body).0
    }

    /// Parse the given statement.
    fn parse_stmt(
        &mut self,
        stmt: &'ast Stored<ast::Stmt<'ast>>,
    ) {
        match **stmt {
            ast::Stmt::Let(_, e) => {
                match self.parse_expr(e).0 {
                    InstRef::Stream(i) => {
                        self.to_streams[usize::from(e.id())] = Some(i);
                    },
                    InstRef::Single(i) => {
                        self.to_singles[usize::from(e.id())] = Some(i);
                    },
                }
            },
        }
    }

    /// Parse the given expression.
    fn parse_expr(
        &mut self,
        expr: &'ast Stored<ast::Expr<'ast>>,
    ) -> (InstRef<'mir>, MapType) {
        let r#type = self.tck.get_expr_type(expr.id());
        match **expr {
            ast::Expr::Not(x) => {
                match self.parse_expr(x).0 {
                    InstRef::Single(x) => {
                        let inst = SingleInst::Una(ScalarUnaOp::Not, x);
                        let inst = self.mir.singles.store(inst);
                        (InstRef::Single(inst), r#type)
                    },
                    InstRef::Stream(x) => {
                        let inst = StreamInst::Una(ScalarUnaOp::Not, x);
                        let inst = self.mir.streams.store(inst);
                        (InstRef::Stream(inst), r#type)
                    },
                }
            },

            ast::Expr::Add(x) | ast::Expr::Sub(x)
                | ast::Expr::Mul(x) | ast::Expr::Div(x) | ast::Expr::Rem(x)
                | ast::Expr::And(x) | ast::Expr::IOr(x) | ast::Expr::XOr(x)
                | ast::Expr::ShL(x) | ast::Expr::ShR(x)
                | ast::Expr::IsEq(x) | ast::Expr::IsNE(x)
                | ast::Expr::IsLT(x) | ast::Expr::IsLE(x)
                | ast::Expr::IsGT(x) | ast::Expr::IsGE(x) => {

                let op = match **expr {
                    ast::Expr::Add(_) => ScalarBinOp::Add,
                    ast::Expr::Sub(_) => ScalarBinOp::Sub,
                    ast::Expr::Mul(_) => ScalarBinOp::Mul,
                    ast::Expr::Div(_) => ScalarBinOp::Div,
                    ast::Expr::Rem(_) => ScalarBinOp::Rem,

                    ast::Expr::And(_) => ScalarBinOp::And,
                    ast::Expr::IOr(_) => ScalarBinOp::IOr,
                    ast::Expr::XOr(_) => ScalarBinOp::XOr,
                    ast::Expr::ShL(_) => ScalarBinOp::ShL,
                    ast::Expr::ShR(_) => ScalarBinOp::ShR,

                    ast::Expr::IsEq(_) => ScalarBinOp::IsEq,
                    ast::Expr::IsNE(_) => ScalarBinOp::IsNE,
                    ast::Expr::IsLT(_) => ScalarBinOp::IsLT,
                    ast::Expr::IsLE(_) => ScalarBinOp::IsLE,
                    ast::Expr::IsGT(_) => ScalarBinOp::IsGT,
                    ast::Expr::IsGE(_) => ScalarBinOp::IsGE,

                    _ => unreachable!(),
                };

                let x = x.map(|x| self.parse_expr(x));

                // Insert an 'option' wherever necessary.
                let x = x.map(|(i, t)| if r#type.option && !t.option {
                    let i = match i {
                        InstRef::Single(i) => {
                            let i = SingleInst::Una(ScalarUnaOp::Opt, i);
                            let i = self.mir.singles.store(i);
                            InstRef::Single(i)
                        },
                        InstRef::Stream(i) => {
                            let i = StreamInst::Una(ScalarUnaOp::Opt, i);
                            let i = self.mir.streams.store(i);
                            InstRef::Stream(i)
                        },
                    };
                    (i, MapType { option: true, .. t })
                } else { (i, t) });

                // Insert a 'vector' wherever necessary.
                let x = if let Some(size) = r#type.vector.get() {
                    x.map(|(i, t)| {
                        if t.vector.exists() { return (i, t); }
                        let op = ScalarUnaOp::Vec(size);
                        let i = match i {
                            InstRef::Single(i) => {
                                let i = SingleInst::Una(op, i);
                                let i = self.mir.singles.store(i);
                                InstRef::Single(i)
                            },
                            InstRef::Stream(i) => {
                                let i = StreamInst::Una(op, i);
                                let i = self.mir.streams.store(i);
                                InstRef::Stream(i)
                            },
                        };
                        (i, MapType { vector: Some(size).into(), .. t })
                    })
                } else { x };

                if r#type.stream {
                    // Insert a 'stream' wherever necessary.
                    let x = x.map(|(i, _)| match i {
                        InstRef::Single(i) => {
                            let i = StreamInst::Map(i);
                            self.mir.streams.store(i)
                        },
                        InstRef::Stream(i) => i,
                    });

                    let i = StreamInst::Bin(op, x);
                    let i = self.mir.streams.store(i);
                    (InstRef::Stream(i), r#type)
                } else {
                    let x = x.map(|(i, _)| match i {
                        InstRef::Single(i) => i,
                        _ => unreachable!(),
                    });

                    let i = SingleInst::Bin(op, x);
                    let i = self.mir.singles.store(i);
                    (InstRef::Single(i), r#type)
                }
            },

            ast::Expr::Cat(x) => {
                let op = ScalarBinOp::Cat;

                let x = x.map(|x| self.parse_expr(x));

                // Insert an 'option' wherever necessary.
                let x = x.map(|(i, t)| if r#type.option && !t.option {
                    let i = match i {
                        InstRef::Single(i) => {
                            let i = SingleInst::Una(ScalarUnaOp::Opt, i);
                            let i = self.mir.singles.store(i);
                            InstRef::Single(i)
                        },
                        InstRef::Stream(i) => {
                            let i = StreamInst::Una(ScalarUnaOp::Opt, i);
                            let i = self.mir.streams.store(i);
                            InstRef::Stream(i)
                        },
                    };
                    (i, MapType { option: true, .. t })
                } else { (i, t) });

                // Insert a 'vector' wherever necessary.
                let x = x.map(|(i, t)| {
                    if t.vector.exists() { return (i, t); }
                    let op = ScalarUnaOp::Vec(1);
                    let i = match i {
                        InstRef::Single(i) => {
                            let i = SingleInst::Una(op, i);
                            let i = self.mir.singles.store(i);
                            InstRef::Single(i)
                        },
                        InstRef::Stream(i) => {
                            let i = StreamInst::Una(op, i);
                            let i = self.mir.streams.store(i);
                            InstRef::Stream(i)
                        },
                    };
                    (i, MapType { vector: Some(1).into(), .. t })
                });

                if r#type.stream {
                    // Insert a 'stream' wherever necessary.
                    let x = x.map(|(i, _)| match i {
                        InstRef::Single(i) => {
                            let i = StreamInst::Map(i);
                            self.mir.streams.store(i)
                        },
                        InstRef::Stream(i) => i,
                    });

                    let i = StreamInst::Bin(op, x);
                    let i = self.mir.streams.store(i);
                    (InstRef::Stream(i), r#type)
                } else {
                    let x = x.map(|(i, _)| match i {
                        InstRef::Single(i) => i,
                        _ => unreachable!(),
                    });

                    let i = SingleInst::Bin(op, x);
                    let i = self.mir.singles.store(i);
                    (InstRef::Single(i), r#type)
                }
            },

            ast::Expr::Ind(x) => {
                let op = ScalarBinOp::Ind;

                let x = x.map(|x| self.parse_expr(x));

                // Insert an 'option' wherever necessary.
                let x = x.map(|(i, t)| if r#type.option && !t.option {
                    let i = match i {
                        InstRef::Single(i) => {
                            let i = SingleInst::Una(ScalarUnaOp::Opt, i);
                            let i = self.mir.singles.store(i);
                            InstRef::Single(i)
                        },
                        InstRef::Stream(i) => {
                            let i = StreamInst::Una(ScalarUnaOp::Opt, i);
                            let i = self.mir.streams.store(i);
                            InstRef::Stream(i)
                        },
                    };
                    (i, MapType { option: true, .. t })
                } else { (i, t) });

                if r#type.stream {
                    // Insert a 'stream' wherever necessary.
                    let x = x.map(|(i, _)| match i {
                        InstRef::Single(i) => {
                            let i = StreamInst::Map(i);
                            self.mir.streams.store(i)
                        },
                        InstRef::Stream(i) => i,
                    });

                    let i = StreamInst::Bin(op, x);
                    let i = self.mir.streams.store(i);
                    (InstRef::Stream(i), r#type)
                } else {
                    let x = x.map(|(i, _)| match i {
                        InstRef::Single(i) => i,
                        _ => unreachable!(),
                    });

                    let i = SingleInst::Bin(op, x);
                    let i = self.mir.singles.store(i);
                    (InstRef::Single(i), r#type)
                }
            },

            ast::Expr::Cond(x) | ast::Expr::Else(x) => {
                let op = match **expr {
                    ast::Expr::Cond(_) => ScalarBinOp::Cond,
                    ast::Expr::Else(_) => ScalarBinOp::Else,

                    _ => unreachable!(),
                };

                let x = x.map(|x| self.parse_expr(x));

                // Insert a 'vector' wherever necessary.
                let x = if let Some(size) = r#type.vector.get() {
                    x.map(|(i, t)| {
                        if t.vector.exists() { return (i, t); }
                        let op = ScalarUnaOp::Vec(size);
                        let i = match i {
                            InstRef::Single(i) => {
                                let i = SingleInst::Una(op, i);
                                let i = self.mir.singles.store(i);
                                InstRef::Single(i)
                            },
                            InstRef::Stream(i) => {
                                let i = StreamInst::Una(op, i);
                                let i = self.mir.streams.store(i);
                                InstRef::Stream(i)
                            },
                        };
                        (i, MapType { vector: Some(size).into(), .. t })
                    })
                } else { x };

                if r#type.stream {
                    // Insert a 'stream' wherever necessary.
                    let x = x.map(|(i, _)| match i {
                        InstRef::Single(i) => {
                            let i = StreamInst::Map(i);
                            self.mir.streams.store(i)
                        },
                        InstRef::Stream(i) => i,
                    });

                    let i = StreamInst::Bin(op, x);
                    let i = self.mir.streams.store(i);
                    (InstRef::Stream(i), r#type)
                } else {
                    let x = x.map(|(i, _)| match i {
                        InstRef::Single(i) => i,
                        _ => unreachable!(),
                    });

                    let i = SingleInst::Bin(op, x);
                    let i = self.mir.singles.store(i);
                    (InstRef::Single(i), r#type)
                }
            },

            ast::Expr::Int(n) => {
                let i = SingleInst::Int((**n).clone());
                let i = self.mir.singles.store(i);
                let t = self.tck.get_expr_type(expr.id());
                (InstRef::Single(i), t)
            },

            ast::Expr::Var(_, e) => {
                let t = self.tck.get_expr_type(e.id());
                if t.stream {
                    let i = self.to_streams[usize::from(e.id())].unwrap();
                    (InstRef::Stream(i), t)
                } else {
                    let i = self.to_singles[usize::from(e.id())].unwrap();
                    (InstRef::Single(i), t)
                }
            },

            ast::Expr::Arg(..) => unreachable!(),

            ast::Expr::Blk { stmts, rexpr } => {
                for stmt in stmts {
                    self.parse_stmt(stmt);
                }

                self.parse_expr(rexpr)
            },

            _ => todo!(),
        }
    }
}

impl<'mir> Storage<'mir> {
    /// Print the given function.
    fn print_fn<'ast>(&mut self, r#fn: &'ast Stored<ast::Fn<'ast>>) {
        print!("declare ");
        self.print_ffi_type(r#fn.rett);
        print!(" @{} (", r#fn.name);
        for (i, arg) in r#fn.args.into_iter().enumerate() {
            if i != 0 {
                print!(", ");
            }

            self.print_ffi_type(arg.r#type);
            print!(" %{}", usize::from(arg.expr.id()));
        }
        print!(")");

        println!();
    }

    /// Print the given type for FFI.
    fn print_ffi_type(&mut self, r#type: ast::Type) {
        if r#type.stream {
            if r#type.option {
                // Track the mask separately.
                print!("{{ i64, ptr, ptr }}");
            } else {
                // Just count the number of elements.
                print!("{{ i64, ptr }}");
            }
        } else if let Some(size) = r#type.vector {
            if r#type.option {
                print!("{{ [{} x ", size);
                self.print_ffi_scalar_type(r#type.scalar);
                print!("], [{} x i1] }}", size);
            } else {
                print!("[{} x ", size);
                self.print_ffi_scalar_type(r#type.scalar);
                print!("]");
            }
        } else if r#type.option {
            print!("{{ ");
            self.print_ffi_scalar_type(r#type.scalar);
            print!(", i1 }}");
        } else {
            self.print_ffi_scalar_type(r#type.scalar);
        }
    }

    /// Print the given scalar type for FFI.
    fn print_ffi_scalar_type(&mut self, r#type: ast::ScalarType) {
        match r#type {
            ast::ScalarType::Int(t) => self.print_ffi_int_type(t),
        }
    }

    /// Print the given integer type for FFI.
    fn print_ffi_int_type(&mut self, r#type: ast::IntType) {
        match r#type {
            ast::IntType::U(s) => print!("i{}", s),
            ast::IntType::S(s) => print!("i{}", s),
        }
    }
}

/// Consume the given function.
pub fn consume<'ast, 'tck>(
    ast: &'ast ast::Storage<'ast>,
    tck: &'tck mut tck::Storage<'ast>,
    r#fn: &'ast Stored<ast::Fn<'ast>>,
) {
    let storage = Storage {
        singles: ObjectStorage::new(),
        streams: ObjectStorage::new(),
    };
    let mut parser = Parser {
        ast,
        tck,
        mir: &storage,
        to_singles: Vec::new(),
        to_streams: Vec::new(),
    };

    let _ = parser.parse_fn(r#fn);
}
