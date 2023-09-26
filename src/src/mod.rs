use std::io;
use std::path::Path;

use num_bigint::BigInt;
use pest::Parser;
use symbol_table::{Symbol, SymbolTable};
use thiserror::Error;
use typed_arena::Arena;

mod parsing;

/// Parse a module from a file.
pub fn parse_module<'a>(
    storage: &Storage<'a>,
    name: &str,
    path: &'a Path,
) -> Result<Module<'a>, Error> {
    let input = std::fs::read_to_string(path)?;
    let mut input = parsing::SidParser::parse(parsing::Rule::module, &input)?;
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
    pub pratt: parsing::SidPrattParser,
}

/// Configure a new Pratt parser.
pub fn new_pratt() -> parsing::SidPrattParser {
    parsing::new_pratt()
}

/// A module definition.
#[derive(Clone, Debug)]
pub struct Module<'a> {
    /// The name of the module.
    pub name: Symbol,
    /// Functions in the module.
    pub funcs: &'a [Function<'a>],
    /// The source of the module.
    pub source: ModuleSource<'a>,
}

/// The source of a module.
#[derive(Clone, Debug)]
pub enum ModuleSource<'a> {
    /// Standard input.
    StdIn,
    /// A file at a certain path.
    File(&'a Path),
}

/// A function definition.
#[derive(Clone, Debug)]
pub struct Function<'a> {
    /// The name of the function.
    pub name: Symbol,
    /// The arguments to the function.
    pub args: &'a [(Symbol, Type)],
    /// The function body.
    pub body: &'a Expr<'a>,
}

/// A type.
#[derive(Clone, Debug)]
pub struct Type {
    /// The underlying scalar type.
    pub scalar: ScalarType,
    /// Whether the type has a stream component.
    pub stream: bool,
}

/// A scalar type.
#[derive(Clone, Debug)]
pub enum ScalarType {
    /// An unsigned 64-bit integer.
    U64,
}

/// A statement.
#[derive(Clone, Debug)]
pub enum Stmt<'a> {
    /// A variable declaration.
    Let(Symbol, &'a Expr<'a>),
}

/// An expression.
#[derive(Clone, Debug)]
pub enum Expr<'a> {
    Not(&'a Expr<'a>),

    Add(&'a Expr<'a>, &'a Expr<'a>),
    Sub(&'a Expr<'a>, &'a Expr<'a>),
    Mul(&'a Expr<'a>, &'a Expr<'a>),
    Div(&'a Expr<'a>, &'a Expr<'a>),
    Rem(&'a Expr<'a>, &'a Expr<'a>),

    And(&'a Expr<'a>, &'a Expr<'a>),
    IOr(&'a Expr<'a>, &'a Expr<'a>),
    XOr(&'a Expr<'a>, &'a Expr<'a>),
    ShL(&'a Expr<'a>, &'a Expr<'a>),
    ShR(&'a Expr<'a>, &'a Expr<'a>),

    IsEq(&'a Expr<'a>, &'a Expr<'a>),
    IsNE(&'a Expr<'a>, &'a Expr<'a>),
    IsLT(&'a Expr<'a>, &'a Expr<'a>),
    IsLE(&'a Expr<'a>, &'a Expr<'a>),
    IsGT(&'a Expr<'a>, &'a Expr<'a>),
    IsGE(&'a Expr<'a>, &'a Expr<'a>),

    // TODO: array operations

    /// An integer literal.
    Int(BigInt),

    /// A reference to a variable.
    Var(Symbol),

    /// A block expression.
    Blk(&'a [Stmt<'a>], &'a Expr<'a>),
}
