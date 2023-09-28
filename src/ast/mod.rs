//! An Abstract Syntax Tree (AST) for Solo.

use std::cmp::{Ordering, PartialOrd};
use std::path::Path;

use num_bigint::BigInt;
use symbol_table::{Symbol, SymbolTable};

use crate::util::arena::{self, Arena};

pub mod parse;
pub use parse::Parser;

pub mod print;
pub use print::Printer;

pub mod visit;
pub use visit::Visit;

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

impl<'a> Expr<'a> {
    /// Determine the precedence of this expression.
    ///
    /// This is the precedence of the primary operator in the expression.
    pub fn prec(&self) -> Prec {
        match self {
            Self::Not(..) => Prec::Max,
            Self::Add(..) | Self::Sub(..) => Prec::AddSub,
            Self::Mul(..) | Self::Div(..) | Self::Rem(..) => Prec::MulDiv,
            Self::And(..) | Self::IOr(..) | Self::XOr(..) => Prec::Bitwise,
            Self::ShL(..) | Self::ShR(..) => Prec::Shift,
            Self::IsEq(..) | Self::IsNE(..) => Prec::Compare,
            Self::IsLT(..) | Self::IsLE(..) => Prec::Compare,
            Self::IsGT(..) | Self::IsGE(..) => Prec::Compare,
            Self::Int(..) => Prec::Max,
            Self::Var(..) => Prec::Max,
            Self::Blk { .. } => Prec::Max,
        }
    }

    /// Determine the symbol for this operation.
    ///
    /// If this expression represents some kind of unary or binary operation,
    /// its symbol is returned.
    pub fn code(&self) -> Option<&'static str> {
        Some(match self {
            Self::Not(..) => "~",
            Self::Add(..) => "+",
            Self::Sub(..) => "-",
            Self::Mul(..) => "*",
            Self::Div(..) => "/",
            Self::Rem(..) => "%",
            Self::And(..) => "&",
            Self::IOr(..) => "|",
            Self::XOr(..) => "^",
            Self::ShL(..) => "<<",
            Self::ShR(..) => ">>",
            Self::IsEq(..) => "==",
            Self::IsNE(..) => "!=",
            Self::IsLT(..) => "<",
            Self::IsLE(..) => "<=",
            Self::IsGT(..) => ">",
            Self::IsGE(..) => ">=",
            _ => return None,
        })
    }
}

/// Operator precedence.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Prec {
    /// The lowest possible precedence.
    Min,

    /// The precedence of comparison operators.
    Compare,

    /// The precedence of shift operators.
    Shift,

    /// The precedence of addition and subtraction.
    AddSub,

    /// The precedence of multiplication and division.
    MulDiv,

    /// The precedence of bitwise operators.
    Bitwise,

    /// The highest possible precedence.
    ///
    /// This includes unary operators, because they cannot be divided into two
    /// distinct subexpressions.
    Max,
}

impl Prec {
    /// The associativity for operators at this precedence.
    ///
    /// If [`None`] is returned, then operators at this precedence cannot be
    /// chained to each other.
    pub fn assoc(&self) -> Option<Assoc> {
        match self {
            Self::Min => None,
            Self::Compare => None,
            Self::Shift => None,
            Self::AddSub => Some(Assoc::Left),
            Self::MulDiv => Some(Assoc::Left),
            Self::Bitwise => Some(Assoc::Left),
            Self::Max => None,
        }
    }

    /// Determine the associativity between two operators.
    ///
    /// Given two operators `a` and `b` in the expression `x <a> y <b> z`, this
    /// function returns [`Assoc::Left`] if `y` binds to `a` or [`Assoc::Right`]
    /// if `y` binds to `b`.  If `a` and `b` cannot be chained, [`None`] will be
    /// returned instead.
    pub fn cmp(lhs: Self, rhs: Self) -> Option<Assoc> {
        match PartialOrd::partial_cmp(&lhs, &rhs)? {
            Ordering::Less => Some(Assoc::Right),
            Ordering::Equal => lhs.assoc(),
            Ordering::Greater => Some(Assoc::Left),
        }
    }

    fn is_arithmetic(&self) -> bool {
        matches!(self, Self::AddSub | Self::MulDiv)
    }

    fn is_bitwise(&self) -> bool {
        matches!(self, Self::Bitwise)
    }
}

impl PartialOrd for Prec {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let (lhs, rhs) = (*self, *other);
        if lhs.is_arithmetic() && rhs.is_bitwise() { return None; }
        if lhs.is_bitwise() && rhs.is_arithmetic() { return None; }
        Some(Ord::cmp(&(lhs as usize), &(rhs as usize)))
    }
}

/// The associativity of an operator.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Assoc {
    /// Left associativity.
    ///
    /// `<x> o <y> o <z>` is parsed as `(<x> o <y>) o <z>`.
    Left,

    /// Right associativity.
    ///
    /// `<x> o <y> o <z>` is parsed as `<x> o (<y> o <z>)`.
    Right,
}
