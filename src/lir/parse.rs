use core::assert_matches::assert_matches;
use core::ops::Range;

use inkwell::{
    self,
    types::BasicType,
};

use crate::mir;
use crate::ops::*;
use crate::soa::{ID, SeqID};
use crate::tys::fix::*;

use super::{*, build::*};

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
        let lir_fn = module.add_function(&mir_fn.name, fn_ty, linkage);
        let entry = context.append_basic_block(lir_fn, "entry");

        // Create a parser for adding to the function.
        let builder = context.create_builder();
        builder.position_at_end(entry);
        let mut parser = Parser {
            context,
            builder,
            lir_fn,
            mir,
            mir_fn,
            singles: Vec::new(),
            streams: Vec::new(),
        };

        // Iterate over loops and singles.
        let mut loops = mir.loops.iter();
        let mut single_cur = 0;
        loop {
            let r#loop = loops.next();

            // Iterate over any singles preceding the next loop (if any).
            let single_end = r#loop
                .map(|l| l.singles)
                .map(<Range<usize>>::from)
                .map_or(mir.singles.num(), |r| r.start);
            let singles = SeqID::from(single_cur .. single_end);
            for single_id in singles.iter() {
                parser.parse_next_single(single_id)?;
            }
            single_cur = single_end;

            // Process the next loop.
            let Some(r#loop) = r#loop else { break };

            todo!()
        }

        // Find the final value.
        let result = parser.singles[usize::from(mir_fn.body)];
        parser.builder.build_return(Some(&result.id))?;

        Ok(Self { inner: parser.lir_fn })
    }
}

/// A parser for an LIR function.
struct Parser<'ctx, 'mir> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    lir_fn: inkwell::values::FunctionValue<'ctx>,
    mir: &'mir mir::Storage,
    mir_fn: &'mir mir::Function,
    singles: Vec<StreamValue<'ctx>>,
    streams: Vec<VectorValue<'ctx>>,
}

impl<'ctx, 'mir> Parser<'ctx, 'mir> {
    /// Parse the next singular instruction.
    fn parse_next_single(
        &mut self,
        id: ID<mir::TypedSingleInst>,
    ) -> Result<(), Error> {
        let b = &self.builder;
        let single = self.mir.singles.get(id);
        let ty = single.dstt;

        let inst = match single.inst {
            mir::SingleInst::Bin(bop, src) => {
                let src = src.map(|v| self.singles[usize::from(v)]);
                self.parse_single_bop(ty, bop, src)?
            },

            mir::SingleInst::Una(uop, src) => {
                let src = src.map(|v| self.singles[usize::from(v)]);
                self.parse_single_uop(ty, uop, src)?
            },

            mir::SingleInst::BitCast(..) => todo!(),

            mir::SingleInst::MapCast(cop, src) => {
                let src = self.singles[usize::from(src)];
                match cop {
                    SingleCastOp::Scalar => unreachable!(),
                    SingleCastOp::Option =>
                        Self::use_option_op(b, ty, [src], |[src]| {
                            let src = src.try_into().unwrap();
                            OptionValue::build_new(b, ty.option, src)
                        })?,
                    SingleCastOp::Vector =>
                        Self::use_vector_op(b, ty, [src], |[src]| {
                            let src = src.try_into().unwrap();
                            VectorValue::build_new(b, ty.vector, src)
                        })?,
                }
            },

            // Collection operations are handled with streams.
            mir::SingleInst::Col(..) => unreachable!(),

            mir::SingleInst::Arg(num) => {
                let id = self.lir_fn.get_nth_param(num).unwrap();
                StreamValue { ty, id }
            },

            mir::SingleInst::Int(ref val) => {
                assert_matches!(ty.stream, StreamPart::None);
                assert_matches!(ty.vector, VectorPart::None);
                assert_matches!(ty.option, OptionPart::None);

                let ScalarType::Int(int_ty) = ty.into();
                let id = IntValue::build_const(b, int_ty, val)?.id;
                StreamValue { ty, id }
            },
        };

        assert_eq!(self.singles.len(), id.into());
        self.singles.push(inst);

        Ok(())
    }

    /// Parse a singular binary operation.
    fn parse_single_bop(
        &mut self,
        ty: MappedType,
        bop: SingleBinOp,
        src: [StreamValue<'ctx>; 2],
    ) -> Result<StreamValue<'ctx>, Error> {
        let b = &self.builder;
        match bop {
            SingleBinOp::Cat => Self::use_vector_op(b, ty, src, |src| {
                VectorValue::build_cat_bop(b, ty.into(), src)
            }),

            SingleBinOp::Ind => Self::use_vector_op(b, ty, src, |src| {
                VectorValue::build_cat_bop(b, ty.into(), src)
            }),

            SingleBinOp::Cond => Self::use_option_op(b, ty, src, |src| {
                OptionValue::build_cond_bop(b, ty.into(), src)
            }),

            SingleBinOp::Else => Self::use_option_op(b, ty, src, |src| {
                OptionValue::build_else_bop(b, ty.into(), src)
            }),

            SingleBinOp::Int(o) => Self::use_scalar_op(b, ty, src, |src| {
                ScalarValue::build_int_bop(b, ty.into(), o, src)
            }),

            SingleBinOp::Cmp(o) => Self::use_scalar_op(b, ty, src, |src| {
                ScalarValue::build_cmp_bop(b, ty.into(), o, src)
            }),
        }
    }

    /// Parse a singular unary operation.
    fn parse_single_uop(
        &mut self,
        ty: MappedType,
        uop: SingleUnaOp,
        src: [StreamValue<'ctx>; 1],
    ) -> Result<StreamValue<'ctx>, Error> {
        let b = &self.builder;
        match uop {
            SingleUnaOp::Int(o) => Self::use_scalar_op(b, ty, src, |src| {
                ScalarValue::build_int_uop(b, ty.into(), o, src)
            }),
        }
    }

    // Utilize an operation on vectors.
    fn use_vector_op<F, const N: usize>(
        _: &Builder<'ctx>,
        ty: StreamType,
        src: [StreamValue<'ctx>; N],
        f: F,
    ) -> Result<StreamValue<'ctx>, Error>
    where F: FnOnce(
              [VectorValue<'ctx>; N],
          ) -> Result<VectorValue<'ctx>, Error>
    {
        assert_matches!(ty.stream, StreamPart::None);
        (f)(src.map(StreamValue::into_vector)).map(From::from)
    }

    // Utilize an operation on options.
    fn use_option_op<F, const N: usize>(
        b: &Builder<'ctx>,
        ty: StreamType,
        src: [StreamValue<'ctx>; N],
        f: F,
    ) -> Result<StreamValue<'ctx>, Error>
    where F: FnOnce(
              [OptionValue<'ctx>; N],
          ) -> Result<OptionValue<'ctx>, Error>
    {
        Self::use_vector_op(b, ty, src, |src| {
            VectorValue::build_fold(b, ty.into(), |iter| {
                (f)(src.try_map(|v| v.build_iter_get(b, iter))?)
            })
        })
    }

    // Utilize an operation on scalars.
    fn use_scalar_op<F, const N: usize>(
        b: &Builder<'ctx>,
        ty: StreamType,
        src: [StreamValue<'ctx>; N],
        f: F,
    ) -> Result<StreamValue<'ctx>, Error>
    where F: FnOnce(
              [ScalarValue<'ctx>; N],
          ) -> Result<ScalarValue<'ctx>, Error>
    {
        Self::use_option_op(b, ty, src, |src| {
            OptionValue::build_fold(b, ty.into(), |iter| {
                (f)(src.try_map(|v| v.build_iter_get(b, iter))?)
            })
        })
    }
}
