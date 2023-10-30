use std::path::PathBuf;

use clap::{Parser, Subcommand};

use solo;

/// A compiler for the Solo programming language.
#[derive(Debug, Parser)]
#[command(author, version)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Compile a module of Solo code.
    Compile {
        /// The path to the module.
        path: PathBuf,
    },
}

pub fn main() {
    let args = Args::parse();

    match args.command {
        Command::Compile { path } => {
            cmd_compile(path)
        },
    }
}

/// Compile a module of Solo code.
fn cmd_compile<'a>(
    path: PathBuf,
) {
    use pest::Parser;

    // Determine the name of the module from its file path.
    let Some(name) = path.file_stem().and_then(|n| n.to_str()) else {
        eprintln!("'{}' is not a valid file path!", path.display());
        std::process::exit(1);
    };

    // Open and read the specified file.
    let input = match std::fs::read_to_string(&path) {
        Ok(input) => input,
        Err(err) => {
            eprintln!("Error: Could not compile '{}'", name);
            eprintln!("Could not open '{}': {}", path.display(), err);
            std::process::exit(1);
        },
    };

    // Parse the file into a grammatical representation.
    let src = match solo::src::Grammar::parse(solo::src::Rule::module, &input) {
        Ok(mut src) => src.next().unwrap(),
        Err(err) => {
            eprintln!("Error: could not compile '{}'", name);
            eprintln!("Could not parse '{}': {}", path.display(), err);
            std::process::exit(1);
        },
    };

    // Parse the grammatical representation into an AST.
    let storage = solo::ast::Storage::new();
    let mut parser = solo::ast::Parser::new(&storage);
    let source = solo::ast::ModSource::File(&path);
    let ast = match parser.parse_module(src, name, source) {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("Error: could not compile '{}'", name);
            eprintln!("Could not parse '{}': {}", path.display(), err);
            std::process::exit(1);
        },
    };

    // Convert the AST of each function into HIR.
    for func in ast.funcs {
        let hir = solo::hir::Parser::parse(&storage, &**func);
        println!("HIR of '{}': {}", func.name, hir);
    }
}
