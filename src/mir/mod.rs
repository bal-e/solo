//! A Middle Intermediary Representation (MIR) for sid.

use egg::{Id, define_language};

use num_bigint::BigInt;

define_language! {
    /// An MIR expression node.
    pub enum ExprNode {
        "[]" = NilArr,

        "-" = Neg([Id; 1]),
        "~" = Not([Id; 1]),
        "[]" = MapArr([Id; 1]),
        ".?" = MapOpt([Id; 1]),
        "&" = Arr([Id; 1]),
        "?" = Opt([Id; 1]),
        "#" = Len([Id; 1]),

        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "%" = Rem([Id; 2]),

        "&"  = And([Id; 2]),
        "|"  = IOr([Id; 2]),
        "^"  = XOr([Id; 2]),
        "<<" = ShL([Id; 2]),
        ">>" = ShR([Id; 2]),

        "==" = IsEq([Id; 2]),
        "!=" = IsNE([Id; 2]),
        "<"  = IsLT([Id; 2]),
        "<=" = IsLE([Id; 2]),
        ">"  = IsGT([Id; 2]),
        ">=" = IsGE([Id; 2]),

        ".." = ToLT([Id; 2]),
        "..=" = ToEq([Id; 2]),

        "?"  = Then([Id; 2]),
        ":"  = Else([Id; 2]),
        "~"  = Cat([Id; 2]),
        "<?" = Exp([Id; 2]),
        ">?" = Red([Id; 2]),
        "<|" = MvL([Id; 2]),
        "|>" = MvR([Id; 2]),

        Int(BigInt),
    }
}
