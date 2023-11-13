use super::Prec;

/// A binary operator.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinOp {
    /// Add one to another.
    Add,

    /// Subtract another.
    Sub,

    /// Multiply one by another.
    Mul,

    /// Divide by another.
    Div,

    /// Take the remainder when divided by another.
    Rem,

    /// Bitwise AND.
    And,

    /// Bitwise inclusive OR.
    IOr,

    /// Bitwise exclusive OR.
    XOr,

    /// Shift bits left (increase significance).
    ShL,

    /// Shift bits right (decrease significance).
    ShR,

    /// Concatenate vectors.
    Cat,

    /// Index into a vector.
    Ind,

    /// Expand a stream into a mask.
    Exp,

    /// Reduce a stream by a mask.
    Red,

    /// A comparison operator.
    Cmp(CmpOp),

    /// Mask to an optional.
    Cond,

    /// Unmask an optional.
    Else,
}

impl BinOp {
    /// The symbol for this operation.
    pub fn code(&self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Rem => "%",

            Self::And => "&",
            Self::IOr => "|",
            Self::XOr => "^",
            Self::ShL => "<<",
            Self::ShR => ">>",

            Self::Cat => "~",
            Self::Ind => "@",
            Self::Exp => "<?",
            Self::Red => ">?",

            Self::Cmp(o) => o.code(),

            Self::Cond => "?",
            Self::Else => ":",
        }
    }

    /// The precedence of this operation.
    pub fn prec(&self) -> Prec {
        match self {
            Self::Add | Self::Sub => Prec::AddSub,
            Self::Mul | Self::Div | Self::Rem => Prec::MulDiv,

            Self::And | Self::IOr | Self::XOr => Prec::Bitwise,
            Self::ShL | Self::ShR => Prec::Shift,

            Self::Cat | Self::Ind => Prec::CatInd,
            Self::Exp | Self::Red => Prec::ExpRed,

            Self::Cmp(_) => Prec::Compare,

            Self::Cond => Prec::Cond,
            Self::Else => Prec::Else,
        }
    }
}

/// A comparison operator.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CmpOp {
    /// Test for equality.
    IsEq,

    /// Test for inequality.
    IsNE,

    /// Test that one is less than another.
    IsLT,

    /// Test that one is less than or equal to another.
    IsLE,

    /// Test than one is greater than another.
    IsGT,

    /// Test that one is greater than or equal to another.
    IsGE,
}

impl CmpOp {
    /// The code for this operation.
    pub fn code(&self) -> &'static str {
        match self {
            Self::IsEq => "==",
            Self::IsNE => "!=",
            Self::IsLT => "<",
            Self::IsLE => "<=",
            Self::IsGT => ">",
            Self::IsGE => ">=",
        }
    }
}

/// A unary operator.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaOp {
    /// Negation.
    Neg,

    /// Bitwise NOT.
    Not,
}

impl UnaOp {
    /// The symbol for this operation, if any.
    pub fn code(&self) -> Option<&'static str> {
        Some(match self {
            Self::Neg => "-",
            Self::Not => "~",
        })
    }
}
