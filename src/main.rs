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
    let src = match solo::src::Grammar::parse(solo::src::Rule::r#mod, &input) {
        Ok(mut src) => src.next().unwrap(),
        Err(err) => {
            eprintln!("Error: could not compile '{}'", name);
            eprintln!("Could not parse '{}': {}", path.display(), err);
            std::process::exit(1);
        },
    };

    // Parse the grammatical representation into an AST.
    let source = solo::ast::ModuleSource::File(path.clone());
    let r#mod = match solo::ast::parse_mod(src, name.to_string(), source) {
        Ok(r#mod) => r#mod,
        Err(err) => {
            eprintln!("Error: could not compile '{}'", name);
            eprintln!("Could not parse '{}': {}", path.display(), err);
            std::process::exit(1);
        },
    };

    for ast in r#mod.functions {
        // Type-check every function.
        let tck = match solo::tck::tck_fn(&ast) {
            Ok(tck) => tck,
            Err(err) => {
                eprintln!("Error: could not type-check '{}'", name);
                eprintln!("'{}' contained an error: {}", ast.name, err);
                std::process::exit(1);
            },
        };

        // Convert the function to HIR.
        let hir = solo::hir::parse_fn(&ast, &tck);

        println!("Compiled HIR: {:#?}", hir);
    }
}
