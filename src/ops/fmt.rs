use core::fmt;

use super::*;

impl fmt::Display for StreamBinOp {
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

impl fmt::Display for SingleBinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Cat => "cat",
            Self::Ind => "ind",
            Self::Cond => "cond",
            Self::Else => "else",
            Self::Int(o) => return fmt::Display::fmt(&o, f),
            Self::Cmp(o) => return fmt::Display::fmt(&o, f),
        })
    }
}

impl fmt::Display for StreamUnaOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(o) => return fmt::Display::fmt(&o, f),
        }
    }
}

impl fmt::Display for SingleUnaOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(o) => return fmt::Display::fmt(&o, f),
        }
    }
}

impl fmt::Display for StreamCastOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Scalar => f.write_str("scalar"),
            Self::Option => f.write_str("option"),
            Self::Vector => f.write_str("vector"),
            Self::Stream => f.write_str("stream"),
        }
    }
}

impl fmt::Display for SingleCastOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Scalar => f.write_str("scalar"),
            Self::Option => f.write_str("option"),
            Self::Vector => f.write_str("vector"),
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
