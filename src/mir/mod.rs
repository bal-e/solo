//! The Medium Intermediary Representation (MIR).

use crate::{ast, tck};
use crate::storage::Stored;

/// Storage for the MIR.
struct Storage<'ast, 'tck> {
    /// Storage for the underlying AST.
    _ast: &'ast ast::Storage<'ast>,

    /// Storage for type information.
    _tck: &'tck mut tck::Storage<'ast>,
}

impl<'ast, 'tck> Storage<'ast, 'tck> {
    /// Print the given function.
    fn print_fn(&mut self, r#fn: &'ast Stored<ast::Fn<'ast>>) {
        print!("declare ");
        self.print_ffi_type(r#fn.rett);
        print!(" @{} (", r#fn.name);
        for (i, arg) in r#fn.args.into_iter().enumerate() {
            if i != 0 {
                print!(", ");
            }

            self.print_ffi_type(arg.r#type);
            print!(" %{}", usize::from(arg.expr.id()));
        }
        print!(")");

        println!();
    }

    /// Print the given type for FFI.
    fn print_ffi_type(&mut self, r#type: ast::Type) {
        if r#type.stream {
            if r#type.option {
                // Track the mask separately.
                print!("{{ i64, ptr, ptr }}");
            } else {
                // Just count the number of elements.
                print!("{{ i64, ptr }}");
            }
        } else if let Some(size) = r#type.vector {
            if r#type.option {
                print!("{{ [{} x ", size);
                self.print_ffi_scalar_type(r#type.scalar);
                print!("], [{} x i1] }}", size);
            } else {
                print!("[{} x ", size);
                self.print_ffi_scalar_type(r#type.scalar);
                print!("]");
            }
        } else if r#type.option {
            print!("{{ ");
            self.print_ffi_scalar_type(r#type.scalar);
            print!(", i1 }}");
        } else {
            self.print_ffi_scalar_type(r#type.scalar);
        }
    }

    /// Print the given scalar type for FFI.
    fn print_ffi_scalar_type(&mut self, r#type: ast::ScalarType) {
        match r#type {
            ast::ScalarType::Int(t) => self.print_ffi_int_type(t),
        }
    }

    /// Print the given integer type for FFI.
    fn print_ffi_int_type(&mut self, r#type: ast::IntType) {
        match r#type {
            ast::IntType::U(s) => print!("i{}", s),
            ast::IntType::S(s) => print!("i{}", s),
        }
    }
}

/// Consume the given function.
pub fn consume<'ast, 'tck>(
    ast: &'ast ast::Storage<'ast>,
    tck: &'tck mut tck::Storage<'ast>,
    r#fn: &'ast Stored<ast::Fn<'ast>>,
) {
    (Storage { _ast: ast, _tck: tck }).print_fn(r#fn)
}
