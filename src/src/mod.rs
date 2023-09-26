use num_bigint::BigInt;
use symbol_table::Symbol;

/// A function definition.
pub struct Func<'a> {
    /// The name of the function.
    pub name: Symbol,
    /// The arguments to the function.
    pub args: &'a [(Symbol, Type)],
    /// The function body.
    pub body: &'a Expr<'a>,
}

/// A type.
pub struct Type {
    /// The underlying primitive type.
    pub prim: PrimType,
    /// Whether the type has a stream component.
    pub is_stream: bool,
}

/// A primitive type.
pub enum PrimType {
    /// An unsigned 64-bit integer.
    U64,
}

/// A statement.
pub enum Stmt<'a> {
    /// A variable declaration.
    Let(Symbol, &'a Expr<'a>),
}

/// An expression.
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
