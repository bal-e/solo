//! A Middle Intermediary Representation (MIR) for sid.

use egg::{Id, define_language};

use num_bigint::BigInt;

define_language! {
    /// An MIR expression node.
    pub enum ExprNode {
        "not" = Not([Id; 1]),

        "add" = Add([Id; 2]),
        "sub" = Sub([Id; 2]),
        "mul" = Mul([Id; 2]),
        "div" = Div([Id; 2]),
        "rem" = Rem([Id; 2]),

        "and" = And([Id; 2]),
        "ior" = IOr([Id; 2]),
        "xor" = XOr([Id; 2]),
        "shl" = ShL([Id; 2]),
        "shr" = ShR([Id; 2]),

        "iseq" = IsEq([Id; 2]),
        "isne" = IsNE([Id; 2]),
        "islt" = IsLT([Id; 2]),
        "isle" = IsLE([Id; 2]),
        "isgt" = IsGT([Id; 2]),
        "isge" = IsGE([Id; 2]),

        // TODO: array operations

        Int(BigInt),
    }
}
