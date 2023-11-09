//! The Medium Intermediary Representation (MIR).

use std::num::NonZeroU32;

use crate::{ast, tck};

/// The iteration rate of a streaming instruction.
///
/// This represents the number of elements output by a streaming instruction at
/// every iteration.  It is the least common multiple of the consumption rates
/// of each of the instructions consuming this ones outputs.
#[derive(Copy, Clone)]
pub struct StreamRate {
    inner: NonZeroU32,
}

impl Default for StreamRate {
    fn default() -> Self {
        Self { inner: NonZeroU32::MIN }
    }
}

/// A singular (non-stream) instruction.
pub enum SingleInst {
    /// Collect a stream.
    Stream(SingleColOp, Rc<StreamInst>),

    /// A unary operation.
    Una(SingleUnaOp, Rc<Self>),

    /// A binary operation.
    Bin(SingleBinOp, Rc<Self>),

    /// An integer literal.
    Int(BigInt),

    /// A function argument.
    Arg(u32),
}

/// A streaming instruction.
pub enum StreamInst {
    /// A streamed single value.
    Single(Rc<SingleInst>),

    /// A unary operation.
    Una(StreamUnaOp, Rc<Self>),

    /// A binary operation.
    Bin(StreamBinOp, [Rc<Self>; 2]),
}

/// A collection operation on streams.
pub enum SingleColOp {
    /// Construct an array out of the stream.
    Arr,
}

/// A unary operation on singles.
pub enum SingleUnaOp {
    /// A unary operation on vectors.
    Vec(VectorUnaOp),
}

/// A binary operation on singles.
pub enum SingleBinOp {
    /// A binary operation on vectors.
    Vec(VectorBinOp),
}

/// A unary operation on streams.
pub enum StreamUnaOp {
    /// A mapped unary operation on vectors.
    Map(VectorUnaOp),

    /// A re-interpreting stream cast.
    Cast(VectorType),
}

/// A binary operation on streams.
pub enum StreamBinOp {
    /// A mapped binary operation on vectors.
    Map(VectorBinOp),

    /// Array expansion.
    Exp,

    /// Array reduction.
    Red,
}

/// A unary operation on vectors.
pub enum VectorUnaOp {
    /// A mapped unary operation on options.
    Map(OptionUnaOp),
}

/// A binary operation on vectors.
pub enum VectorBinOp {
    /// A mapped binary operation on options.
    Map(OptionBinOp),

    /// Vector concatenation.
    Cat,

    /// Vector indexing.
    Ind,
}

/// A unary operation on options.
pub enum OptionUnaOp {
    /// A mapped unary operation on scalars.
    Map(ScalarUnaOp),
}

/// A binary operation on options.
pub enum OptionBinOp {
    /// A mapped binary operation on scalars.
    Map(ScalarBinOp),

    /// Masking.
    Cond,

    /// Unmasking.
    Else,
}

/// A unary operation on scalars.
pub enum ScalarUnaOp {
    /// Negation.
    Neg,

    /// Inversion.
    Not,
}

/// A binary operation on scalars.
pub enum ScalarBinOp {
    /// Addition.
    Add,

    /// Subtraction.
    Sub,

    /// Multiplication.
    Mul,

    /// Division.
    Div,

    /// Remainder.
    Rem,

    /// Bitwise AND.
    And,

    /// Bitwise inclusive OR.
    IOr,

    /// Bitwise exclusive OR.
    XOr,

    /// Left-shift.
    ShL,

    /// Right-shift.
    ShR,

    /// Comparison.
    Cmp(ScalarCmpOp),
}

/// A comparison operation on scalars.
pub enum ScalarCmpOp {
    /// Equality.
    IsEq,

    /// Inequality.
    IsNE,

    /// Less-than.
    IsLT,

    /// Less-than or equal to.
    IsLE,

    /// Greater-than.
    IsGT,

    /// Greater-than or equal to.
    IsGE,
}

//impl<'ast, 'tck> Storage<'ast, 'tck> {
//    /// Print the given function.
//    fn print_fn(&mut self, r#fn: &'ast Stored<ast::Fn<'ast>>) {
//        print!("declare ");
//        self.print_ffi_type(r#fn.rett);
//        print!(" @{} (", r#fn.name);
//        for (i, arg) in r#fn.args.into_iter().enumerate() {
//            if i != 0 {
//                print!(", ");
//            }
//
//            self.print_ffi_type(arg.r#type);
//            print!(" %{}", usize::from(arg.expr.id()));
//        }
//        print!(")");
//
//        println!();
//    }
//
//    /// Print the given type for FFI.
//    fn print_ffi_type(&mut self, r#type: ast::Type) {
//        if r#type.stream {
//            if r#type.option {
//                // Track the mask separately.
//                print!("{{ i64, ptr, ptr }}");
//            } else {
//                // Just count the number of elements.
//                print!("{{ i64, ptr }}");
//            }
//        } else if let Some(size) = r#type.vector {
//            if r#type.option {
//                print!("{{ [{} x ", size);
//                self.print_ffi_scalar_type(r#type.scalar);
//                print!("], [{} x i1] }}", size);
//            } else {
//                print!("[{} x ", size);
//                self.print_ffi_scalar_type(r#type.scalar);
//                print!("]");
//            }
//        } else if r#type.option {
//            print!("{{ ");
//            self.print_ffi_scalar_type(r#type.scalar);
//            print!(", i1 }}");
//        } else {
//            self.print_ffi_scalar_type(r#type.scalar);
//        }
//    }
//
//    /// Print the given scalar type for FFI.
//    fn print_ffi_scalar_type(&mut self, r#type: ast::ScalarType) {
//        match r#type {
//            ast::ScalarType::Int(t) => self.print_ffi_int_type(t),
//        }
//    }
//
//    /// Print the given integer type for FFI.
//    fn print_ffi_int_type(&mut self, r#type: ast::IntType) {
//        match r#type {
//            ast::IntType::U(s) => print!("i{}", s),
//            ast::IntType::S(s) => print!("i{}", s),
//        }
//    }
//}
