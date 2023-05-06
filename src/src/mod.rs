///! Source code for sid.

use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "src/sid.pest"]
struct SidParser;
