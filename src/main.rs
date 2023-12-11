use std::path::PathBuf;

use clap::{Parser, Subcommand};

use solo::*;

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
    let src = match src::Grammar::parse(src::Rule::r#mod, &input) {
        Ok(mut src) => src.next().unwrap(),
        Err(err) => {
            eprintln!("Error: could not compile '{}'", name);
            eprintln!("Could not parse '{}': {}", path.display(), err);
            std::process::exit(1);
        },
    };

    // Parse the grammatical representation into an AST.
    let mut parser = ast::parse::Parser::default();
    let module = match ast::Module::parse(name.to_string(), src, &mut parser) {
        Ok(module) => module,
        Err(err) => {
            eprintln!("Error: could not compile '{}'", name);
            eprintln!("Could not parse '{}': {}", path.display(), err);
            std::process::exit(1);
        },
    };
    let module_id = parser.storage.modules.put(module);
    let ast: ast::Storage = parser.storage.into();
    let module = ast.modules.get(module_id);

    // Format the AST and print it.
    let mut syntax = String::new();
    let mut writer = ast::syn::Writer::new(&ast, &mut syntax);
    module.write_syntax(&mut writer).unwrap();
    println!("AST for '{}':", module.name);
    println!("{}", syntax);

    // Type-check the module.
    let mut tck = tck::Storage::new(&ast);
    for function_id in module.functions.iter() {
        let function = ast.functions.get(function_id);
        if let Err(err) = tck.tck_function(function) {
            eprintln!("Error: could not type-check '{}'", name);
            eprintln!("'{}' contained an error: {}", function.name, err);
            std::process::exit(1);
        };
    }

    // Generate code for each function.
    for function_id in module.functions.iter() {
        let function = ast.functions.get(function_id);
        let body = hir::parse::Parser::parse(&ast, &tck, function);
        let hir_fn = hir::Function { name: function.name.clone(), body };
        println!();
        println!("HIR for '{}':", function.name);
        println!("{}", hir_fn.body);

        let (_, mir) = mir::Function::parse(&hir_fn);
        println!();
        println!("MIR for '{}':", function.name);

        println!("Streams:");
        let streams: &[_] = mir.streams.as_ref();
        for (id, stream) in streams.iter().enumerate() {
            println!("  {} | {}", id, stream);
        }

        println!("Singles:");
        let singles: &[_] = mir.singles.as_ref();
        for (id, single) in singles.iter().enumerate() {
            println!("  {} | {}", id, single);
        }

        println!("Loops:");
        let loops: &[_] = mir.loops.as_ref();
        for (id, r#loop) in loops.iter().enumerate() {
            println!("  {} | {}", id, r#loop);
        }
    }
}
