//! Type-checking for Solo.

use core::num::NonZeroU32;

use std::rc::Rc;

use num_bigint::BigInt;
use thiserror::Error;

use crate::ast::{UnaOp, BinOp};

pub mod logic;

/// A function definition.
#[derive(Clone, Debug)]
pub struct Function {
    /// The name of the function.
    pub name: String,
    /// The arguments to the function.
    pub args: Vec<Argument>,
    /// The return type of the function.
    pub rett: Type,
    /// The function body.
    pub body: Expr,
}

/// An argument to a function.
#[derive(Clone, Debug)]
pub struct Argument {
    /// The argument variable.
    pub variable: Rc<Variable>,
}

/// A variable.
#[derive(Clone, Debug)]
pub struct Variable {
    /// The name of the variable.
    pub name: String,
    /// The expression defining the variable.
    pub expr: Typed<Expr>,
}

/// A type.
#[derive(Copy, Clone, Debug)]
pub struct Type {
    /// The stream component of the type.
    pub stream: Option<StreamPart>,
    /// The vector component of the type.
    pub vector: Option<VectorPart>,
    /// The option component of the type.
    pub option: Option<OptionPart>,
    /// The underlying scalar type.
    pub scalar: ScalarType,
}

/// The stream component of a type.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct StreamPart {}

/// The vector component of a type.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct VectorPart {
    /// The number of elements in each vector.
    size: u32,
}

/// The option component of a type.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct OptionPart {}

/// A scalar type.
#[derive(Copy, Clone, Debug)]
pub enum ScalarType {
    /// An arbitrary (unknown) type.
    Any,

    /// An integer type.
    Int(Option<IntType>),
}

/// An integer type.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct IntType {
    /// The sign of the integer type.
    pub sign: IntSign,

    /// The size of the integer type.
    pub size: IntSize,
}

/// The sign of an integer type.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IntSign {
    /// An unsigned integer type.
    U,
    /// A signed integer type.
    S,
}

/// The size of an integer type.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct IntSize {
    /// The number of bits.
    pub bits: NonZeroU32,
}

/// A statement.
#[derive(Clone, Debug)]
pub enum Stmt {
    /// A variable declaration.
    Let(Rc<Variable>),
}

/// An expression.
#[derive(Clone, Debug)]
pub enum Expr {
    /// A unary expression.
    Una(UnaOp, Box<Typed<Self>>),

    /// A binary expression.
    Bin(BinOp, [Box<Typed<Self>>; 2]),

    /// A parenthesized expression.
    Par(Box<Typed<Self>>),

    /// A cast expression.
    Cast(Type, Box<Typed<Self>>),

    /// An integer literal.
    Int(BigInt),

    /// A reference to a local variable.
    Var(Rc<Variable>),

    /// A reference to a function argument.
    Arg,

    /// A block expression.
    Blk {
        stmts: Vec<Stmt>,
        rexpr: Box<Typed<Self>>,
    },
}

/// A typed object.
#[derive(Clone, Debug)]
pub struct Typed<T> {
    /// The underlying object.
    pub data: T,
    /// The associated type.
    pub r#type: Type,
}

/// A type-checking error.
#[derive(Debug, Error)]
pub enum Error {
    #[error("A type logic error occurred: {0}")]
    Logic(#[from] logic::Error),

    #[error("An expression's type could not be resolved")]
    Unresolvable,
}

/// A type-checking result.
pub type Result<T> = core::result::Result<T, Error>;
