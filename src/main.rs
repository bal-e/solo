use std::path::PathBuf;

use clap::{Parser, Subcommand};

use solo;

/// A compiler for the Solo programming language.
#[derive(Debug, Parser)]
#[command(author, version)]
struct Args {
    #[command(subcommand)]
    command: Option<Command>,
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
        Some(Command::Compile { path }) => {
            cmd_compile(path)
        },
        None => {
            println!("No command provided!");
        },
    }
}

/// Compile a module of Solo code.
fn cmd_compile<'a>(
    path: PathBuf,
) {
    use solo::util::arena::Arena;

    let Some(name) = path.file_stem().and_then(|n| n.to_str()) else {
        eprintln!("'{}' is not a valid file path!", path.display());
        std::process::exit(1);
    };

    let storage = solo::ast::Storage {
        syms: Default::default(),
        modules: &Arena::new(),
        funcs: &Arena::new(),
        func_args: &Arena::new(),
        stmts: &Arena::new(),
        exprs: &Arena::new(),
        pratt: solo::ast::new_pratt(),
    };

    match solo::ast::parse_module(&storage, name, &path) {
        Ok(_) => {
            println!("Parsing successful!");
        },
        Err(e) => {
            eprintln!("An error occurred: {e}");
            std::process::exit(1);
        },
    }
}
