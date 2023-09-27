//! Solo source code.

use pest_derive::Parser;

/// The grammar for Solo source code.
#[derive(Parser)]
#[grammar = "src/grammar.pest"]
pub struct Grammar;
