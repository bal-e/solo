use inkwell::{
    self,
    types::BasicType,
};

use crate::mir;

use super::{*, val::*};

impl<'ctx> Function<'ctx> {
    /// Parse a function from MIR.
    pub fn parse(
        context: &'ctx Context,
        module: &Module<'ctx>,
        mir: &mir::Storage,
        mir_fn: &mir::Function,
    ) -> Result<Self, Error> {
        // Create the function.
        let linkage = Some(inkwell::module::Linkage::External);
        let rett = mir.singles.get(mir_fn.body).dstt;
        let rett = StreamValue::llvm_ty(context, rett);
        let args = mir_fn.args.iter()
            .map(|t| StreamValue::llvm_ty(context, *t).into())
            .collect::<Vec<_>>();
        let fn_ty = rett.fn_type(&args, false);
        let value = module.add_function(&mir_fn.name, fn_ty, linkage);
        let entry = context.append_basic_block(value, "entry");

        // Create a parser for adding to the function.
        let builder = context.create_builder();
        builder.position_at_end(entry);
        let mut parser = Parser { context, builder, mir, mir_fn };

        // TODO: Iterate over loops and singles.

        todo!()
    }
}

/// A parser for an LIR function.
struct Parser<'ctx, 'mir> {
    context: &'ctx Context,
    builder: inkwell::builder::Builder<'ctx>,
    mir: &'mir mir::Storage,
    mir_fn: &'mir mir::Function,
}
