use crate::prec::*;

use super::*;

impl Expr {
    /// The precedence of this expression.
    pub fn prec(&self) -> Prec {
        match self {
            Self::Bin(bop, _) => bop.prec(),
            Self::Una(uop, _) => uop.prec(),
            Self::Cast(..) => Prec::UnaPre,
            _ => Prec::Max,
        }
    }
}
