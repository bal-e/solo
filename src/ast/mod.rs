//! An Abstract Syntax Tree (AST) for Solo.

use num_bigint::BigInt;

use crate::soa::{ID, SeqID};

use crate::ops::*;
use crate::tys::fix::*;

mod prec;

mod soa;
pub use soa::*;

pub mod syn;

pub mod parse;

/// A module definition.
#[derive(Clone, Debug)]
pub struct Module {
    /// The name of the module.
    pub name: String,

    /// Functions in the module.
    pub functions: SeqID<Function>,
}

/// A function definition.
#[derive(Clone, Debug)]
pub struct Function {
    /// The name of the function.
    pub name: String,

    /// The arguments to the function.
    pub args: SeqID<Argument>,

    /// The return type of the function.
    pub rett: MappedType,

    /// The function body.
    pub body: ID<Expr>,

    /// The ID of the first variable in the function.
    pub variables_beg: ID<Variable>,

    /// The ID of the first expression in the function.
    pub exprs_beg: ID<Expr>,
}

/// An argument to a function.
#[derive(Clone, Debug)]
pub struct Argument {
    /// The argument variable.
    pub variable: ID<Variable>,

    /// The type of the argument.
    pub r#type: MappedType,
}

/// A variable.
#[derive(Clone, Debug)]
pub struct Variable {
    /// The name of the declared variable.
    pub name: String,

    /// The expression defining the variable.
    pub expr: ID<Expr>,
}

/// A statement.
#[derive(Clone, Debug)]
pub enum Stmt {
    /// A variable declaration.
    Let(ID<Variable>),
}

/// An expression.
#[derive(Clone, Debug)]
pub enum Expr {
    /// A binary expression.
    Bin(BinOp, [ID<Self>; 2]),

    /// A unary expression.
    Una(UnaOp, [ID<Self>; 1]),

    /// A parenthesized expression.
    Par(ID<Self>),

    /// A bitwise cast expression.
    BitCast(MappedType, ID<Self>),

    /// An integer literal.
    Int(BigInt),

    /// A reference to a local variable.
    Var(ID<Variable>),

    /// A reference to a function argument.
    Arg,

    /// A block expression.
    Blk {
        stmts: SeqID<Stmt>,
        rexpr: ID<Self>,
    },
}
