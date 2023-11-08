//! An Abstract Syntax Tree (AST) for Solo.

use std::num::NonZeroU32;
use std::path::PathBuf;
use std::rc::Rc;

use num_bigint::BigInt;

mod ops;
pub use ops::*;

mod prec;
pub use prec::*;

//pub mod parse;
//pub use parse::Parser;

/// A module definition.
#[derive(Clone, Debug)]
pub struct Module {
    /// The name of the module.
    pub name: String,
    /// Functions in the module.
    pub functions: Vec<Function>,
    /// The source of the module.
    pub source: ModuleSource,
}

/// The source of a module.
#[derive(Clone, Debug)]
pub enum ModuleSource {
    /// Standard input.
    StdIn,

    /// A file at a certain path.
    File(PathBuf),
}

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
    /// The type of the argument.
    pub r#type: Type,
}

/// A variable.
#[derive(Clone, Debug)]
pub struct Variable {
    /// The name of the declared variable.
    pub name: String,

    /// The expression defining the variable.
    pub expr: Expr,
}

/// A type.
#[derive(Clone, Debug)]
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
#[derive(Clone, Debug)]
pub enum ScalarType {
    /// An integer type.
    Int(IntType),
}

/// An integer type.
#[derive(Clone, Debug)]
pub enum IntType {
    /// An unsigned integer type.
    U(NonZeroU32),
    /// A signed integer type.
    S(NonZeroU32),
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
    Una(UnaOp, Box<Expr>),

    /// A binary expression.
    Bin(BinOp, [Box<Expr>; 2]),

    /// A parenthesized expression.
    Par(Box<Expr>),

    /// A cast expression.
    Cast(Type, Box<Expr>),

    /// An integer literal.
    Int(BigInt),

    /// A reference to a local variable.
    Var(Rc<Variable>),

    /// A reference to a function argument.
    Arg,

    /// A block expression.
    Blk {
        stmts: Vec<Stmt>,
        rexpr: Box<Expr>,
    },
}
