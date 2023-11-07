use core::cmp::{Ordering, PartialOrd};

/// Operator precedence.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Prec {
    /// The lowest possible precedence.
    Min,

    /// The precedence of defaulting.
    Else,

    /// The precedence of conditioning.
    Cond,

    /// The precedence of array expansion and reduction.
    ExpRed,

    /// The precedence of vector concatenation and indexing.
    CatInd,

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
            Self::Else => Some(Assoc::Right),
            Self::Cond => None,
            Self::ExpRed => None,
            Self::CatInd => Some(Assoc::Left),
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
}

impl PartialOrd for Prec {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (*self, *other) {
            (Self::AddSub | Self::MulDiv, Self::Bitwise) => None,
            (Self::Bitwise, Self::AddSub | Self::MulDiv) => None,
            (lhs, rhs) => Some(Ord::cmp(&(lhs as usize), &(rhs as usize))),
        }
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
