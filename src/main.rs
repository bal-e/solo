use std::io::{self, Read};

use sid::mir::ExprNode;
use egg::RecExpr;

pub fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let expr: RecExpr<ExprNode> = input.parse().unwrap();
    println!("{expr}");
}
