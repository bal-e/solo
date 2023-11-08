//! An Abstract Syntax Tree (AST) for Solo.

use std::num::NonZeroU32;
use std::path::PathBuf;

use crate::storage::{ID, SeqID};
use crate::storage::ints::Integer;
use crate::storage::syms::Symbol;

mod ops;
pub use ops::*;

mod prec;
pub use prec::*;

pub mod storage;
pub use storage::Storage;

pub mod parse;
pub use parse::Parser;

/// A module definition.
#[derive(Clone)]
pub struct Module {
    /// The name of the module.
    pub name: ID<Symbol>,
    /// Functions in the module.
    pub functions: SeqID<Function>,
    /// The source of the module.
    pub source: ModuleSource,
}

/// The source of a module.
#[derive(Clone)]
pub enum ModuleSource {
    /// Standard input.
    StdIn,

    /// A file at a certain path.
    File(PathBuf),
}

/// A function definition.
#[derive(Copy, Clone)]
pub struct Function {
    /// The name of the function.
    pub name: ID<Symbol>,
    /// The arguments to the function.
    pub args: SeqID<Argument>,
    /// The return type of the function.
    pub rett: ID<Type>,
    /// The function body.
    pub body: ID<Expr>,
}

/// An argument to a function.
#[derive(Copy, Clone)]
pub struct Argument {
    /// The argument variable.
    pub variable: ID<Variable>,
    /// The type of the argument.
    pub r#type: ID<Type>,
}

/// A variable.
#[derive(Copy, Clone)]
pub struct Variable {
    /// The name of the declared variable.
    pub name: ID<Symbol>,

    /// The expression defining the variable.
    pub expr: ID<Expr>,
}

/// A type.
#[derive(Copy, Clone)]
pub struct Type {
    /// The underlying scalar type.
    pub scalar: ScalarType,
    /// Whether the type has an option component.
    pub option: bool,
    /// Whether the type has a vector component.
    pub vector: Option<u32>,
    /// Whether the type has a stream component.
    pub stream: bool,
}

/// A scalar type.
#[derive(Copy, Clone)]
pub enum ScalarType {
    /// An integer type.
    Int(IntType),
}

/// An integer type.
#[derive(Copy, Clone)]
pub enum IntType {
    /// An unsigned integer type.
    U(NonZeroU32),
    /// A signed integer type.
    S(NonZeroU32),
}

/// A statement.
#[derive(Copy, Clone)]
pub enum Stmt {
    /// A variable declaration.
    Let(ID<Variable>),
}

/// An expression.
#[derive(Copy, Clone)]
pub enum Expr {
    /// A unary expression.
    Una(UnaOp, ID<Expr>),

    /// A binary expression.
    Bin(BinOp, [ID<Expr>; 2]),

    /// A parenthesized expression.
    Par(ID<Expr>),

    /// A cast expression.
    Cast(ID<Type>, ID<Expr>),

    /// An integer literal.
    Int(ID<Integer>),

    /// A reference to a local variable.
    Var(ID<Variable>),

    /// A reference to a function argument.
    Arg,

    /// A block expression.
    Blk {
        stmts: SeqID<Stmt>,
        rexpr: ID<Expr>,
    },
}
