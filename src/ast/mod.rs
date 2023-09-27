//! An Abstract Syntax Tree (AST) for Solo.

use std::io;
use std::path::Path;

use num_bigint::BigInt;
use pest::Parser;
use symbol_table::{Symbol, SymbolTable};
use thiserror::Error;

use crate::util::arena::{self, Arena};

mod parsing;

/// Parse a module from a file.
pub fn parse_module<'a>(
    storage: &Storage<'a>,
    name: &str,
    path: &'a Path,
) -> Result<Module<'a>, Error> {
    let input = std::fs::read_to_string(path)?;
    let mut input = parsing::SoloParser::parse(parsing::Rule::module, &input)?;
    let name = storage.syms.intern(name);
    let source = ModuleSource::File(path);
    Ok(parsing::parse_module(storage, input.next().unwrap(), name, source))
}

/// Errors from parsing.
#[derive(Debug, Error)]
pub enum Error {
    #[error("An I/O error occurred: {0}")]
    IO(#[from] io::Error),

    #[error("A parsing error occurred: {0}")]
    Parsing(#[from] parsing::Error),
}

/// Storage for AST data.
pub struct Storage<'a> {
    /// Storage for symbols.
    pub syms: SymbolTable,
    /// Storage for modules.
    pub modules: &'a Arena<Module<'a>>,
    /// Storage for functions.
    pub funcs: &'a Arena<Function<'a>>,
    /// Storage for function arguments.
    pub func_args: &'a Arena<(Symbol, Type)>,
    /// Storage for statements.
    pub stmts: &'a Arena<Stmt<'a>>,
    /// Storage for expressions.
    pub exprs: &'a Arena<Expr<'a>>,
    /// The Pratt parser for expressions.
    pub pratt: parsing::SoloPrattParser,
}

/// Configure a new Pratt parser.
pub fn new_pratt() -> parsing::SoloPrattParser {
    parsing::new_pratt()
}

/// A module definition.
#[derive(Clone)]
pub struct Module<'a> {
    /// The name of the module.
    pub name: Symbol,
    /// Functions in the module.
    pub funcs: arena::RefMany<'a, Function<'a>>,
    /// The source of the module.
    pub source: ModuleSource<'a>,
}

/// The source of a module.
#[derive(Clone)]
pub enum ModuleSource<'a> {
    /// Standard input.
    StdIn,
    /// A file at a certain path.
    File(&'a Path),
}

/// A function definition.
#[derive(Clone)]
pub struct Function<'a> {
    /// The name of the function.
    pub name: Symbol,
    /// The arguments to the function.
    pub args: arena::RefMany<'a, (Symbol, Type)>,
    /// The return type of the function.
    pub rett: Type,
    /// The function body.
    pub body: arena::Ref<'a, Expr<'a>>,
}

/// A type.
#[derive(Clone)]
pub struct Type {
    /// The underlying scalar type.
    pub scalar: ScalarType,
    /// Whether the type has a stream component.
    pub stream: bool,
}

/// A scalar type.
#[derive(Clone)]
pub enum ScalarType {
    /// An unsigned 64-bit integer.
    U64,
}

/// A statement.
#[derive(Clone)]
pub enum Stmt<'a> {
    /// A variable declaration.
    Let(Symbol, arena::Ref<'a, Expr<'a>>),
}

/// An expression.
#[derive(Clone)]
pub enum Expr<'a> {
    Not(arena::Ref<'a, Expr<'a>>),

    Add([arena::Ref<'a, Expr<'a>>; 2]),
    Sub([arena::Ref<'a, Expr<'a>>; 2]),
    Mul([arena::Ref<'a, Expr<'a>>; 2]),
    Div([arena::Ref<'a, Expr<'a>>; 2]),
    Rem([arena::Ref<'a, Expr<'a>>; 2]),

    And([arena::Ref<'a, Expr<'a>>; 2]),
    IOr([arena::Ref<'a, Expr<'a>>; 2]),
    XOr([arena::Ref<'a, Expr<'a>>; 2]),
    ShL([arena::Ref<'a, Expr<'a>>; 2]),
    ShR([arena::Ref<'a, Expr<'a>>; 2]),

    IsEq([arena::Ref<'a, Expr<'a>>; 2]),
    IsNE([arena::Ref<'a, Expr<'a>>; 2]),
    IsLT([arena::Ref<'a, Expr<'a>>; 2]),
    IsLE([arena::Ref<'a, Expr<'a>>; 2]),
    IsGT([arena::Ref<'a, Expr<'a>>; 2]),
    IsGE([arena::Ref<'a, Expr<'a>>; 2]),

    // TODO: array operations

    /// An integer literal.
    Int(BigInt),

    /// A reference to a variable.
    Var(Symbol),

    /// A block expression.
    Blk {
        stmts: arena::RefMany<'a, Stmt<'a>>,
        rexpr: arena::Ref<'a, Expr<'a>>,
    },
}
