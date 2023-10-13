//! An Abstract Syntax Tree (AST) for Solo.

use std::cmp::{Ordering, PartialOrd};
use std::ops::{Residual, Try};
use std::path::Path;

use num_bigint::BigInt;
use symbol_table::SymbolTable;

use crate::storage::*;

pub mod parse;
pub use parse::Parser;

/// Storage for AST data.
#[derive(Default)]
pub struct Storage<'a> {
    /// Storage for symbols.
    pub syms: SymbolTable,
    /// Storage for modules.
    pub mods: ObjectStorage<Mod<'a>>,
    /// Storage for functions.
    pub fns: ObjectStorage<Fn<'a>>,
    /// Storage for function arguments.
    pub fn_args: ObjectStorage<FnArg<'a>>,
    /// Storage for statements.
    pub stmts: ObjectStorage<Stmt<'a>>,
    /// Storage for expressions.
    pub exprs: ObjectStorage<Expr<'a>>,
    /// Storage for integer literals.
    pub ints: ObjectStorage<BigInt>,
}

impl<'a> Storage<'a> {
    /// Construct a new [`Storage`].
    pub fn new() -> Self {
        Self::default()
    }
}

impl<'a> StoreRef<str> for Storage<'a> {
    type Stored<'b> = &'b str where Self: 'b;

    fn store_ref(&self, object: &str) -> Self::Stored<'_> {
        let symbol = self.syms.intern(object);
        self.syms.resolve(symbol)
    }
}

macro_rules! def_field_store {
    ($type:ty, $field:ident) => {
        impl<'a> Store<$type> for Storage<'a> {
            type Stored<'b> = &'b Stored<$type> where Self: 'b;

            fn store(&self, object: $type) -> Self::Stored<'_> {
                self.$field.store(object)
            }
        }

        impl<'a> StoreMany<$type> for Storage<'a> {
            type Stored<'b> = &'b [Stored<$type>] where Self: 'b;

            fn store_many<I>(&self, objects: I) -> Self::Stored<'_>
            where I: IntoIterator<Item = $type> {
                self.$field.store_many(objects)
            }
        }

        impl<'a> StoreTryMany<$type> for Storage<'a> {
            type Stored<'b> = &'b [Stored<$type>] where Self: 'b;

            fn store_try_many<'b, E, F, I>(&'b self, objects: I)
                -> <F as Residual<Self::Stored<'_>>>::TryType
            where I: IntoIterator<Item = E>,
                  E: Try<Output = $type, Residual = F>,
                  F: Residual<Self::Stored<'b>> {
                self.$field.store_try_many(objects)
            }
        }
    };
}

def_field_store!(Mod<'a>, mods);
def_field_store!(Fn<'a>, fns);
def_field_store!(FnArg<'a>, fn_args);
def_field_store!(Stmt<'a>, stmts);
def_field_store!(Expr<'a>, exprs);
def_field_store!(BigInt, ints);

/// A module definition.
#[derive(Copy, Clone)]
pub struct Mod<'a> {
    /// The name of the module.
    pub name: &'a str,
    /// Functions in the module.
    pub funcs: &'a [Stored<Fn<'a>>],
    /// The source of the module.
    pub source: ModSource<'a>,
}

/// The source of a module.
#[derive(Copy, Clone)]
pub enum ModSource<'a> {
    /// Standard input.
    StdIn,
    /// A file at a certain path.
    File(&'a Path),
}

/// A function definition.
#[derive(Copy, Clone)]
pub struct Fn<'a> {
    /// The name of the function.
    pub name: &'a str,
    /// The arguments to the function.
    pub args: &'a [Stored<FnArg<'a>>],
    /// The return type of the function.
    pub rett: Type,
    /// The function body.
    pub body: &'a Stored<Expr<'a>>,
}

/// An argument to a function.
#[derive(Clone)]
pub struct FnArg<'a> {
    /// The name of the argument.
    pub name: &'a str,
    /// The type of the argument.
    pub r#type: Type,
}

/// A type.
#[derive(Copy, Clone)]
pub struct Type {
    /// The underlying scalar type.
    pub scalar: ScalarType,
    /// Whether the type has a stream component.
    pub stream: bool,
}

/// A scalar type.
#[derive(Copy, Clone)]
pub enum ScalarType {
    /// An unsigned 64-bit integer.
    U64,
}

/// A statement.
#[derive(Copy, Clone)]
pub enum Stmt<'a> {
    /// A variable declaration.
    Let(&'a str, &'a Stored<Expr<'a>>),
}

/// An expression.
#[derive(Copy, Clone)]
pub enum Expr<'a> {
    Not(&'a Stored<Expr<'a>>),

    Add([&'a Stored<Expr<'a>>; 2]),
    Sub([&'a Stored<Expr<'a>>; 2]),
    Mul([&'a Stored<Expr<'a>>; 2]),
    Div([&'a Stored<Expr<'a>>; 2]),
    Rem([&'a Stored<Expr<'a>>; 2]),

    And([&'a Stored<Expr<'a>>; 2]),
    IOr([&'a Stored<Expr<'a>>; 2]),
    XOr([&'a Stored<Expr<'a>>; 2]),
    ShL([&'a Stored<Expr<'a>>; 2]),
    ShR([&'a Stored<Expr<'a>>; 2]),

    IsEq([&'a Stored<Expr<'a>>; 2]),
    IsNE([&'a Stored<Expr<'a>>; 2]),
    IsLT([&'a Stored<Expr<'a>>; 2]),
    IsLE([&'a Stored<Expr<'a>>; 2]),
    IsGT([&'a Stored<Expr<'a>>; 2]),
    IsGE([&'a Stored<Expr<'a>>; 2]),

    // TODO: array operations

    /// An integer literal.
    Int(&'a Stored<BigInt>),

    /// A reference to a variable.
    Var(&'a str),

    /// A block expression.
    Blk {
        stmts: &'a [Stored<Stmt<'a>>],
        rexpr: &'a Stored<Expr<'a>>,
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
