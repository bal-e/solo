use super::*;

impl BinOp {
    /// The syntax of this operation.
    pub fn syntax(&self) -> &'static str {
        match self {
            Self::Exp => "<?",
            Self::Red => ">?",
            Self::Cat => "~",
            Self::Ind => "@",
            Self::Cond => "?",
            Self::Else => ":",
            Self::Int(o) => o.syntax(),
            Self::Cmp(o) => o.syntax(),
        }
    }
}

impl UnaOp {
    /// The syntax of this operation.
    pub fn syntax(&self) -> &'static str {
        match self {
            Self::Int(o) => o.syntax(),
        }
    }
}

impl IntBinOp {
    /// The syntax of this operation.
    pub fn syntax(&self) -> &'static str {
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
        }
    }
}

impl CmpBinOp {
    /// The syntax of this operation.
    pub fn syntax(&self) -> &'static str {
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

impl IntUnaOp {
    /// The syntax of this operation.
    pub fn syntax(&self) -> &'static str {
        match self {
            Self::Neg => "-",
            Self::Not => "~",
        }
    }
}
