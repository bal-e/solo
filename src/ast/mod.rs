//! An Abstract Syntax Tree (AST) for Solo.

use std::num::NonZeroU32;
use std::path::PathBuf;
use std::ops::{Deref, DerefMut, Range};
use std::rc::Rc;

use num_bigint::BigInt;

mod ops;
pub use ops::*;

mod prec;
pub use prec::*;

pub mod parse;
pub use parse::parse_mod;

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
    pub rett: MappedType,

    /// The function body.
    pub body: Box<Stored<Expr>>,

    /// The IDs of variables.
    pub variable_ids: Range<u32>,

    /// The IDs of statements.
    pub stmt_ids: Range<u32>,

    /// The IDs of expressions.
    pub expr_ids: Range<u32>,
}

/// An argument to a function.
#[derive(Clone, Debug)]
pub struct Argument {
    /// The argument variable.
    pub variable: Rc<Stored<Variable>>,

    /// The type of the argument.
    pub r#type: MappedType,
}

/// A variable.
#[derive(Clone, Debug)]
pub struct Variable {
    /// The name of the declared variable.
    pub name: String,

    /// The expression defining the variable.
    pub expr: Box<Stored<Expr>>,
}

/// A mapped type.
#[derive(Clone, Debug)]
pub struct MappedType {
    /// The type's stream component.
    pub stream: Option<StreamPart>,

    /// The type's vector component.
    pub vector: Option<VectorPart>,

    /// The type's option component.
    pub option: Option<OptionPart>,

    /// The underlying scalar type.
    pub scalar: ScalarType,
}

/// The stream component of a type.
#[derive(Clone, Debug)]
pub struct StreamPart {}

/// The vector component of a type.
#[derive(Clone, Debug)]
pub struct VectorPart {
    /// The number of elements.
    pub size: u32,
}

/// The option component of a type.
#[derive(Clone, Debug)]
pub struct OptionPart {}

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
    Let(Rc<Stored<Variable>>),
}

/// An expression.
#[derive(Clone, Debug)]
pub enum Expr {
    /// A unary expression.
    Una(UnaOp, Box<Stored<Self>>),

    /// A binary expression.
    Bin(BinOp, [Box<Stored<Self>>; 2]),

    /// A parenthesized expression.
    Par(Box<Stored<Self>>),

    /// A cast expression.
    Cast(MappedType, Box<Stored<Self>>),

    /// An integer literal.
    Int(BigInt),

    /// A reference to a local variable.
    Var(Rc<Stored<Variable>>),

    /// A reference to a function argument.
    Arg,

    /// A block expression.
    Blk {
        stmts: Vec<Stored<Stmt>>,
        rexpr: Box<Stored<Self>>,
    },
}

/// A stored object.
///
/// This associates a 32-bit identifier with the object.
#[derive(Clone, Debug)]
pub struct Stored<T> {
    /// The identifier of the object.
    pub ident: u32,

    /// The object itself.
    pub inner: T,
}

impl<T> Deref for Stored<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for Stored<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
