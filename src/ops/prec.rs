use crate::prec::*;

use super::*;

impl StreamBinOp {
    /// The precedence of this operation.
    pub fn prec(&self) -> Prec {
        match self {
            Self::Exp | Self::Red => Prec::ExpRed,
            Self::Cat | Self::Ind => Prec::CatInd,
            Self::Cond => Prec::Cond,
            Self::Else => Prec::Else,
            Self::Int(o) => o.prec(),
            Self::Cmp(_) => Prec::Compare,
        }
    }
}

impl StreamUnaOp {
    /// The precedence of this operation.
    pub fn prec(&self) -> Prec {
        match self {
            Self::Int(o) => o.prec(),
        }
    }
}

impl IntBinOp {
    /// The precedence of this operation.
    pub fn prec(&self) -> Prec {
        match self {
            Self::Add | Self::Sub => Prec::AddSub,
            Self::Mul | Self::Div | Self::Rem => Prec::MulDiv,

            Self::And | Self::IOr | Self::XOr => Prec::Bitwise,
            Self::ShL | Self::ShR => Prec::Shift,
        }
    }
}

impl IntUnaOp {
    /// The precedence of this operation.
    pub fn prec(&self) -> Prec {
        match self {
            Self::Neg | Self::Not => Prec::UnaPre,
        }
    }
}
