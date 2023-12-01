use core::fmt;

use super::*;

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Exp => "exp",
            Self::Red => "red",
            Self::Cat => "cat",
            Self::Ind => "ind",
            Self::Cond => "cond",
            Self::Else => "else",
            Self::Int(o) => return fmt::Display::fmt(&o, f),
            Self::Cmp(o) => return fmt::Display::fmt(&o, f),
        })
    }
}

impl fmt::Display for UnaOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(o) => return fmt::Display::fmt(&o, f),
        }
    }
}

impl fmt::Display for IntBinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Add => "add",
            Self::Sub => "sub",
            Self::Mul => "mul",
            Self::Div => "div",
            Self::Rem => "rem",
            Self::And => "and",
            Self::IOr => "ior",
            Self::XOr => "xor",
            Self::ShL => "shl",
            Self::ShR => "shr",
        })
    }
}

impl fmt::Display for CmpBinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::IsEq => "iseq",
            Self::IsNE => "isne",
            Self::IsLT => "islt",
            Self::IsLE => "isle",
            Self::IsGT => "isgt",
            Self::IsGE => "isge",
        })
    }
}

impl fmt::Display for IntUnaOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Neg => "neg",
            Self::Not => "not",
        })
    }
}
