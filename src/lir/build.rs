//! Building LIR values.

use core::assert_matches::assert_matches;

use num_bigint::{BigInt, Sign};

use inkwell::{
    self,
    AddressSpace,
    basic_block::BasicBlock,
    types::{BasicTypeEnum, BasicType},
    values::BasicValueEnum,
};

use crate::ops::*;
use crate::tys::fix::*;

use super::*;

/// A built stream (array) value.
#[derive(Copy, Clone, Debug)]
pub struct StreamValue<'ctx> {
    pub ty: StreamType,
    pub id: BasicValueEnum<'ctx>,
}

impl<'ctx> StreamValue<'ctx> {
    /// Translate a type to LLVM.
    pub fn llvm_ty(
        c: &'ctx Context,
        ty: StreamType,
    ) -> BasicTypeEnum<'ctx> {
        let mut data = ScalarValue::llvm_ty(c, ty.into());
        let mut mask = c.custom_width_int_type(1).as_basic_type_enum();

        if let VectorPart::Some { size } = ty.vector {
            data = data.array_type(size).into();
            mask = mask.array_type(size).into();
        }

        if let StreamPart::Some {} = ty.stream {
            let addr = AddressSpace::default();
            data = data.ptr_type(addr).into();
            mask = mask.ptr_type(addr).into();
        }

        let size = c.i64_type().into();
        match (ty.option, ty.stream) {
            (OptionPart::Some {}, StreamPart::Some {}) =>
                c.struct_type(&[data, mask, size], false).into(),
            (OptionPart::Some {}, StreamPart::None) =>
                c.struct_type(&[data, mask], false).into(),
            (OptionPart::None, StreamPart::Some {}) =>
                c.struct_type(&[data, size], false).into(),
            (OptionPart::None, StreamPart::None) =>
                data,
        }
    }

    /// Convert this to a vector value type.
    pub fn into_vector(self) -> VectorValue<'ctx> {
        assert_matches!(self.ty.stream, StreamPart::None);
        VectorValue { ty: self.ty.into(), id: self.id }
    }
}

impl<'ctx> From<VectorValue<'ctx>> for StreamValue<'ctx> {
    fn from(value: VectorValue<'ctx>) -> Self {
        Self { ty: value.ty.with_part(StreamPart::None), id: value.id }
    }
}

impl<'ctx> TryFrom<StreamValue<'ctx>> for VectorValue<'ctx> {
    type Error = ();

    fn try_from(value: StreamValue<'ctx>) -> Result<Self, Self::Error> {
        if let StreamPart::None = value.ty.stream {
            Ok(Self { ty: value.ty.into(), id: value.id })
        } else {
            Err(())
        }
    }
}

/// A built optional value.
#[derive(Copy, Clone, Debug)]
pub struct VectorValue<'ctx> {
    pub ty: VectorType,
    pub id: BasicValueEnum<'ctx>,
}

impl<'ctx> VectorValue<'ctx> {
    /// Build a concatenation operation.
    pub fn build_cat_bop(
        b: &Builder<'ctx>,
        ty: VectorType,
        src: [Self; 2],
    ) -> Result<Self, Error> {
        let [lhs, rhs] = src;
        let VectorPart::Some { size: lhs_size } = lhs.ty.vector else {
            unreachable!();
        };
        let VectorPart::Some { .. } = rhs.ty.vector else {
            unreachable!();
        };

        let val = Self::build_poison(b, ty)?;

        // Append LHS elements.
        let val = Self::build_iter(b, lhs.ty.vector, val.id, |iter, val| {
            let ins = lhs.build_iter_get(b, iter)?;

            let val = Self { ty, id: val };
            let val = val.build_iter_put(b, ins, iter)?;

            Ok(val.id)
        })?;

        let off = b.ctx.i32_type().const_int(lhs_size.into(), false);

        // Append RHS elements.
        let val = Self::build_iter(b, rhs.ty.vector, val, |iter, val| {
            let ins = rhs.build_iter_get(b, iter)?;

            // Adjust the iteration index for the destination type.
            let index = iter.unwrap().index;
            let index = b.build_int_add(index, off, "")?;
            let iter = Some(VectorIter { index });

            let val = Self { ty, id: val };
            let val = val.build_iter_put(b, ins, iter)?;

            Ok(val.id)
        })?;

        Ok(Self { ty, id: val })
    }

    /// Build a indexing operation.
    pub fn build_ind_bop(
        b: &Builder<'ctx>,
        ty: VectorType,
        src: [Self; 2],
    ) -> Result<Self, Error> {
        let [lhs, rhs] = src;
        assert_matches!(lhs.ty.vector, VectorPart::Some { .. });
        assert_matches!(rhs.ty.scalar, ScalarType::Int(_));
        assert_eq!(ty.vector, rhs.ty.vector);
        assert_eq!(ty.option, lhs.ty.option);
        assert_eq!(ty.option, rhs.ty.option);

        Self::build_fold(b, ty, |vec_iter| {
            let index = rhs.build_iter_get(b, vec_iter)?;
            OptionValue::build_fold(b, ty.into(), |opt_iter| {
                let index = index.build_iter_get(b, opt_iter)?;
                let index = index.id.into_int_value();
                let ind_iter = Some(VectorIter { index });
                let val = lhs.build_iter_get(b, ind_iter)?;
                val.build_iter_get(b, opt_iter)
            })
        })
    }

    /// Build an iteration loop that builds a single [`VectorValue`].
    pub fn build_fold<F>(
        b: &Builder<'ctx>,
        ty: VectorType,
        f: F,
    ) -> Result<Self, Error>
    where F: FnOnce(
              Option<VectorIter<'ctx>>,
          ) -> Result<OptionValue<'ctx>, Error>
    {
        let val = Self::build_undef(b, ty)?;
        let val = Self::build_iter(b, ty.vector, val.id, |iter, val| {
            let new = (f)(iter)?;
            let val = Self { ty, id: val };
            let val = val.build_iter_put(b, new, iter)?;
            Ok(val.id)
        })?;

        Ok(Self { ty, id: val })
    }

    /// Build a general iteration loop.
    pub fn build_iter<F>(
        b: &Builder<'ctx>,
        part: VectorPart,
        val: BasicValueEnum<'ctx>,
        f: F,
    ) -> Result<BasicValueEnum<'ctx>, Error>
    where F: FnOnce(
              Option<VectorIter<'ctx>>,
              BasicValueEnum<'ctx>,
          ) -> Result<BasicValueEnum<'ctx>, Error>
    {
        if let VectorPart::Some { size } = part {
            let curr = b.get_insert_block().unwrap();
            let test = b.ctx.insert_basic_block_after(curr, "");
            let body = b.ctx.insert_basic_block_after(test, "");
            let post = b.ctx.insert_basic_block_after(body, "");

            let idx_ty = b.ctx.i32_type();

            // Prepare for iteration.
            let idx_beg = idx_ty.const_zero();
            let val_beg = val;
            b.build_unconditional_branch(test)?;

            // Test that the current index is in bounds.
            b.position_at_end(test);
            let idx_phi = b.build_phi(idx_ty, "")?;
            let idx_old = idx_phi.as_basic_value().into_int_value();
            let val_phi = b.build_phi(val.get_type(), "")?;
            let val_old = val_phi.as_basic_value();
            let total = idx_ty.const_int(size.into(), false);
            let pred = inkwell::IntPredicate::ULT;
            let cont = b.build_int_compare(pred, idx_old, total, "")?;
            b.build_conditional_branch(cont, body, post)?;

            // Perform iteration and collect the values.
            b.position_at_end(body);
            let iter = VectorIter { index: idx_old };
            let val_new = (f)(Some(iter), val_old)?;
            let idx_add = idx_ty.const_int(1, false);
            let idx_new = b.build_int_add(idx_old, idx_add, "")?;
            b.build_unconditional_branch(test)?;

            // Regular control flow resumes.
            b.position_at_end(post);

            idx_phi.add_incoming(&[
                (&idx_beg, curr),
                (&idx_new, body),
            ]);

            val_phi.add_incoming(&[
                (&val_beg, curr),
                (&val_new, body),
            ]);

            Ok(val_old)
        } else {
            (f)(None, val)
        }
    }

    /// Retrieve a value during iteration.
    pub fn build_iter_get(
        &self,
        b: &Builder<'ctx>,
        iter: Option<VectorIter<'ctx>>,
    ) -> Result<OptionValue<'ctx>, Error> {
        if let VectorPart::Some { .. } = self.ty.vector {
            // Extract the current element via pointer arithmetic.

            let ty = Self::llvm_ty(b.ctx, self.ty);
            let elem_ty = OptionValue::llvm_ty(b.ctx, self.ty.into());
            let index = iter.unwrap().index;
            let tmp = b.build_alloca(ty, "")?;
            b.build_store(tmp, self.id)?;
            let ptr = unsafe {
                let idx = &[b.ctx.i32_type().const_zero(), index];
                b.build_in_bounds_gep(ty, tmp, idx, "")?
            };
            let val = b.build_load(elem_ty, ptr, "")?;

            Ok(OptionValue { ty: self.ty.into(), id: val })
        } else {
            assert_matches!(iter, None);
            Ok(OptionValue { ty: self.ty.into(), id: self.id })
        }
    }

    /// Insert a value during iteration.
    pub fn build_iter_put(
        self,
        b: &Builder<'ctx>,
        ins: OptionValue<'ctx>,
        iter: Option<VectorIter<'ctx>>,
    ) -> Result<Self, Error> {
        assert_eq!(ins.ty, self.ty.into());

        if let VectorPart::Some { .. } = self.ty.vector {
            let ty = Self::llvm_ty(b.ctx, self.ty);
            let index = iter.unwrap().index;
            let tmp = b.build_alloca(ty, "")?;
            b.build_store(tmp, self.id)?;
            let ptr = unsafe {
                let idx = &[b.ctx.i32_type().const_zero(), index];
                b.build_in_bounds_gep(ty, tmp, idx, "")?
            };
            b.build_store(ptr, ins.id)?;
            let val = b.build_load(ty, tmp, "")?;

            Ok(Self { ty: self.ty, id: val })
        } else {
            assert_matches!(iter, None);
            Ok(Self { ty: self.ty, id: ins.id })
        }
    }

    /// Broadcast a value into a [`VectorValue`].
    pub fn build_new(
        b: &Builder<'ctx>,
        part: VectorPart,
        src: OptionValue<'ctx>,
    ) -> Result<Self, Error> {
        let ty = src.ty.with_part(part);
        Self::build_fold(b, ty, |_| Ok(src))
    }

    /// Build a poison value.
    pub fn build_poison(
        b: &Builder<'ctx>,
        ty: VectorType,
    ) -> Result<Self, Error> {
        let id = get_poison(Self::llvm_ty(b.ctx, ty));
        Ok(Self { ty, id })
    }

    /// Build an undefined value.
    pub fn build_undef(
        b: &Builder<'ctx>,
        ty: VectorType,
    ) -> Result<Self, Error> {
        let id = get_undef(Self::llvm_ty(b.ctx, ty));
        Ok(Self { ty, id })
    }

    /// Translate a type to LLVM.
    pub fn llvm_ty(
        c: &'ctx Context,
        ty: VectorType,
    ) -> BasicTypeEnum<'ctx> {
        let option = OptionValue::llvm_ty(c, ty.into());
        if let VectorPart::Some { size } = ty.vector {
            option.array_type(size).into()
        } else {
            option
        }
    }
}

impl<'ctx> From<OptionValue<'ctx>> for VectorValue<'ctx> {
    fn from(value: OptionValue<'ctx>) -> Self {
        Self { ty: value.ty.with_part(VectorPart::None), id: value.id }
    }
}

impl<'ctx> TryFrom<VectorValue<'ctx>> for OptionValue<'ctx> {
    type Error = ();

    fn try_from(value: VectorValue<'ctx>) -> Result<Self, Self::Error> {
        if let VectorPart::None = value.ty.vector {
            Ok(Self { ty: value.ty.into(), id: value.id })
        } else {
            Err(())
        }
    }
}

/// An iteration over vectors.
#[derive(Copy, Clone, Debug)]
pub struct VectorIter<'ctx> {
    /// The current vector index.
    index: inkwell::values::IntValue<'ctx>,
}

/// A built optional value.
#[derive(Copy, Clone, Debug)]
pub struct OptionValue<'ctx> {
    pub ty: OptionType,
    pub id: BasicValueEnum<'ctx>,
}

impl<'ctx> OptionValue<'ctx> {
    /// Build an option condition operation.
    pub fn build_cond_bop(
        b: &Builder<'ctx>,
        ty: OptionType,
        src: [Self; 2],
    ) -> Result<Self, Error> {
        assert_matches!(ty.option, OptionPart::Some {});
        assert_matches!(src[0].ty.option, OptionPart::None);
        assert_matches!(src[0].ty.scalar, ScalarType::Int(_));
        assert_matches!(src[1].ty.option, OptionPart::Some {});

        let cond = src[0].id.into_int_value();
        let empty = Self::build_empty(b, ty)?;
        let res = b.build_select(cond, src[1].id, empty.id, "")?;

        Ok(Self { ty, id: res })
    }

    /// Build an option defaulting operation.
    pub fn build_else_bop(
        b: &Builder<'ctx>,
        ty: OptionType,
        src: [Self; 2],
    ) -> Result<Self, Error> {
        assert_matches!(src[0].ty.option, OptionPart::Some {});

        // If the RHS is optional, the result must be optional too.
        let lhs = src[0].id.into_struct_value();
        let data = b.build_extract_value(lhs, 0, "")?;
        let data = ScalarValue { ty: src[0].ty.into(), id: data };
        let data = Self::build_new(b, src[1].ty.option, data)?;

        let cond = b.build_extract_value(lhs, 1, "")?;
        let cond = cond.into_int_value();
        let res = b.build_select(cond, data.id, src[1].id, "")?;

        Ok(Self { ty, id: res })
    }

    /// Build an iteration loop that builds a single [`VectorValue`].
    pub fn build_fold<F>(
        b: &Builder<'ctx>,
        ty: OptionType,
        f: F,
    ) -> Result<Self, Error>
    where F: FnOnce(
              Option<OptionIter<'ctx>>,
          ) -> Result<ScalarValue<'ctx>, Error>
    {
        let val = Self::build_empty(b, ty)?;
        let val = Self::build_iter(b, ty, val.id, |iter, val| {
            let new = (f)(iter)?;
            let val = Self { ty, id: val };
            let val = val.build_iter_put(b, new, iter)?;
            Ok(val.id)
        })?;

        Ok(Self { ty, id: val })
    }

    /// Build an iteration loop.
    pub fn build_iter<F>(
        b: &Builder<'ctx>,
        ty: OptionType,
        val: BasicValueEnum<'ctx>,
        f: F,
    ) -> Result<BasicValueEnum<'ctx>, Error>
    where F: FnOnce(
              Option<OptionIter<'ctx>>,
              BasicValueEnum<'ctx>,
          ) -> Result<BasicValueEnum<'ctx>, Error>
    {
        if let OptionPart::Some {} = ty.option {
            let curr = b.get_insert_block().unwrap();
            let body = b.ctx.insert_basic_block_after(curr, "");
            let fail = b.ctx.insert_basic_block_after(body, "");
            let post = b.ctx.insert_basic_block_after(fail, "");

            // Prepare for iteration.
            let val_old = val;
            b.build_unconditional_branch(body)?;

            // Fill 'body' with the given function.
            b.position_at_end(body);
            let iter = OptionIter { escape_block: fail };
            let val_new = (f)(Some(iter), val_old)?;
            b.build_unconditional_branch(post)?;

            // In case of failure, `none` is returned.
            b.position_at_end(fail);
            b.build_unconditional_branch(post)?;

            b.position_at_end(post);
            let val_phi = b.build_phi(val.get_type(), "")?;
            let val_fin = val_phi.as_basic_value();

            val_phi.add_incoming(&[
                (&val_new, body),
                (&val_old, fail),
            ]);

            Ok(val_fin)
        } else {
            (f)(None, val)
        }
    }

    /// Retrieve a value during iteration.
    pub fn build_iter_get(
        &self,
        b: &Builder<'ctx>,
        iter: Option<OptionIter<'ctx>>,
    ) -> Result<ScalarValue<'ctx>, Error> {
        if let OptionPart::Some {} = self.ty.option {
            // Unwrap the 'option' and escape on failure.

            let curr = b.get_insert_block().unwrap();
            let next = b.ctx.insert_basic_block_after(curr, "");
            let exit = iter.unwrap().escape_block;

            let src = self.id.into_struct_value();
            let data = b.build_extract_value(src, 0, "")?;
            let mask = b.build_extract_value(src, 1, "")?;
            let mask = mask.into_int_value();

            b.build_conditional_branch(mask, next, exit)?;
            b.position_at_end(next);

            Ok(ScalarValue { ty: self.ty.into(), id: data })
        } else {
            assert!(iter.is_none());
            Ok(ScalarValue { ty: self.ty.into(), id: self.id })
        }
    }

    /// Insert a value during iteration.
    pub fn build_iter_put(
        self,
        b: &Builder<'ctx>,
        ins: ScalarValue<'ctx>,
        iter: Option<OptionIter<'ctx>>,
    ) -> Result<Self, Error> {
        assert_eq!(self.ty.scalar, ins.ty);

        if let OptionPart::Some {} = self.ty.option {
            let _ = iter.unwrap();
            let (data, dst) = (ins.id, self.id.into_struct_value());
            let mask = b.ctx.custom_width_int_type(1).const_int(1, false);
            let dst = b.build_insert_value(dst, data, 0, "")?;
            let dst = b.build_insert_value(dst, mask, 1, "")?;
            let dst = dst.into_struct_value().into();
            Ok(Self { ty: self.ty, id: dst })
        } else {
            assert_matches!(iter, None);
            Ok(Self { ty: self.ty, id: ins.id })
        }
    }

    /// Broadcast a value into an [`OptionValue`].
    pub fn build_new(
        b: &Builder<'ctx>,
        part: OptionPart,
        src: ScalarValue<'ctx>,
    ) -> Result<Self, Error> {
        let ty = src.ty.with_part(part);
        if let OptionPart::Some {} = part {
            let mask = b.ctx.custom_width_int_type(1).const_int(1, false);
            let id = b.ctx.const_struct(&[src.id, mask.into()], false).into();
            Ok(Self { ty, id })
        } else {
            Ok(Self { ty, id: src.id })
        }
    }

    /// Build an empty optional.
    pub fn build_empty(
        b: &Builder<'ctx>,
        ty: OptionType,
    ) -> Result<Self, Error> {
        let data = ScalarValue::build_poison(b, ty.into())?;
        if let OptionPart::Some {} = ty.option {
            let mask = b.ctx.custom_width_int_type(1).const_int(0, false);
            let id = b.ctx.const_struct(&[data.id, mask.into()], false).into();
            Ok(Self { ty, id })
        } else {
            Ok(Self { ty, id: data.id })
        }
    }

    /// Build a poison value.
    pub fn build_poison(
        b: &Builder<'ctx>,
        ty: OptionType,
    ) -> Result<Self, Error> {
        let id = get_poison(Self::llvm_ty(b.ctx, ty));
        Ok(Self { ty, id })
    }

    /// Build an undefined value.
    pub fn build_undef(
        b: &Builder<'ctx>,
        ty: OptionType,
    ) -> Result<Self, Error> {
        let id = get_undef(Self::llvm_ty(b.ctx, ty));
        Ok(Self { ty, id })
    }

    /// Translate a type to LLVM.
    pub fn llvm_ty(
        c: &'ctx Context,
        ty: OptionType,
    ) -> BasicTypeEnum<'ctx> {
        let scalar = ScalarValue::llvm_ty(c, ty.scalar);
        match ty.option {
            OptionPart::Some {} => {
                let mask = c.custom_width_int_type(1).into();
                c.struct_type(&[scalar, mask], false).into()
            },
            OptionPart::None => scalar,
        }
    }
}

impl<'ctx> From<ScalarValue<'ctx>> for OptionValue<'ctx> {
    fn from(value: ScalarValue<'ctx>) -> Self {
        Self { ty: value.ty.with_part(OptionPart::None), id: value.id }
    }
}

impl<'ctx> TryFrom<OptionValue<'ctx>> for ScalarValue<'ctx> {
    type Error = ();

    fn try_from(value: OptionValue<'ctx>) -> Result<Self, Self::Error> {
        if let OptionPart::None = value.ty.option {
            Ok(Self { ty: value.ty.into(), id: value.id })
        } else {
            Err(())
        }
    }
}

/// An iteration over optionals.
#[derive(Copy, Clone, Debug)]
pub struct OptionIter<'ctx> {
    /// The block to escape to when `none` is encountered.
    escape_block: BasicBlock<'ctx>,
}

/// A built scalar value.
#[derive(Copy, Clone, Debug)]
pub struct ScalarValue<'ctx> {
    pub ty: ScalarType,
    pub id: BasicValueEnum<'ctx>,
}

impl<'ctx> ScalarValue<'ctx> {
    /// Build the given integer binary operation.
    pub fn build_int_bop(
        b: &Builder<'ctx>,
        ty: ScalarType,
        bop: IntBinOp,
        src: [Self; 2],
    ) -> Result<Self, Error> {
        let ScalarType::Int(ty) = ty;
        let src = src.map(Self::into_int);
        IntValue::build_int_bop(b, ty, bop, src)
            .map(Self::from)
    }

    /// Build the given comparative binary operation.
    pub fn build_cmp_bop(
        b: &Builder<'ctx>,
        ty: ScalarType,
        cop: CmpBinOp,
        src: [Self; 2],
    ) -> Result<Self, Error> {
        match src[0].ty {
            ScalarType::Int(_) => {
                let ScalarType::Int(ty) = ty;
                let src = src.map(Self::into_int);
                IntValue::build_cmp_bop(b, ty, cop, src)
                    .map(Self::from)
            },
        }
    }

    /// Build the given integer unary operation.
    pub fn build_int_uop(
        b: &Builder<'ctx>,
        ty: ScalarType,
        uop: IntUnaOp,
        src: [Self; 1],
    ) -> Result<Self, Error> {
        let ScalarType::Int(ty) = ty;
        let src = src.map(Self::into_int);
        IntValue::build_int_uop(b, ty, uop, src)
            .map(Self::from)
    }

    /// Build a poison value.
    pub fn build_poison(
        b: &Builder<'ctx>,
        ty: ScalarType,
    ) -> Result<Self, Error> {
        let id = get_poison(Self::llvm_ty(b.ctx, ty));
        Ok(Self { ty, id })
    }

    /// Build an undefined value.
    pub fn build_undef(
        b: &Builder<'ctx>,
        ty: ScalarType,
    ) -> Result<Self, Error> {
        let id = get_undef(Self::llvm_ty(b.ctx, ty));
        Ok(Self { ty, id })
    }

    /// Translate a type to LLVM.
    pub fn llvm_ty(
        c: &'ctx Context,
        ty: ScalarType,
    ) -> BasicTypeEnum<'ctx> {
        match ty {
            ScalarType::Int(ty) => IntValue::llvm_ty(c, ty),
        }
    }

    /// Convert this into an integer value type.
    pub fn into_int(self) -> IntValue<'ctx> {
        let ScalarType::Int(ty) = self.ty;
        IntValue { ty, id: self.id }
    }
}

impl<'ctx> From<IntValue<'ctx>> for ScalarValue<'ctx> {
    fn from(value: IntValue<'ctx>) -> Self {
        Self {
            ty: ScalarType::Int(value.ty),
            id: value.id.into(),
        }
    }
}

/// A built integer value.
#[derive(Copy, Clone, Debug)]
pub struct IntValue<'ctx> {
    pub ty: IntType,
    pub id: BasicValueEnum<'ctx>,
}

impl<'ctx> IntValue<'ctx> {
    /// Build the given integer binary operation.
    pub fn build_int_bop(
        b: &Builder<'ctx>,
        ty: IntType,
        bop: IntBinOp,
        src: [IntValue<'ctx>; 2],
    ) -> Result<Self, Error> {
        assert_eq!(ty, src[0].ty);
        assert_eq!(ty, src[1].ty);

        let [lhs, rhs] = src.map(|v| v.id.into_int_value());
        let id = match (bop, ty.sign) {
            (IntBinOp::Add, _) =>
                b.build_int_add(lhs, rhs, "")?,
            (IntBinOp::Sub, _) =>
                b.build_int_add(lhs, rhs, "")?,
            (IntBinOp::Mul, _) =>
                b.build_int_add(lhs, rhs, "")?,

            (IntBinOp::Div, IntSign::U) =>
                b.build_int_unsigned_div(lhs, rhs, "")?,
            (IntBinOp::Div, IntSign::S) =>
                b.build_int_signed_div(lhs, rhs, "")?,
            (IntBinOp::Rem, IntSign::U) =>
                b.build_int_unsigned_rem(lhs, rhs, "")?,
            (IntBinOp::Rem, IntSign::S) =>
                b.build_int_signed_rem(lhs, rhs, "")?,

            (IntBinOp::And, _) =>
                b.build_and(lhs, rhs, "")?,
            (IntBinOp::IOr, _) =>
                b.build_or(lhs, rhs, "")?,
            (IntBinOp::XOr, _) =>
                b.build_xor(lhs, rhs, "")?,

            (IntBinOp::ShL, _) =>
                b.build_left_shift(lhs, rhs, "")?,
            (IntBinOp::ShR, IntSign::U) =>
                b.build_right_shift(lhs, rhs, false, "")?,
            (IntBinOp::ShR, IntSign::S) =>
                b.build_right_shift(lhs, rhs, true, "")?,
        }.into();

        Ok(Self { ty, id })
    }

    /// Build the given comparative binary operation.
    pub fn build_cmp_bop(
        b: &Builder<'ctx>,
        ty: IntType,
        cop: CmpBinOp,
        src: [Self; 2],
    ) -> Result<Self, Error> {
        use inkwell::IntPredicate;

        assert_eq!(src[0].ty, src[1].ty);

        let sign = src[0].ty.sign;
        let [lhs, rhs] = src.map(|v| v.id.into_int_value());
        let pred = match (cop, sign) {
            (CmpBinOp::IsEq, _) => IntPredicate::EQ,
            (CmpBinOp::IsNE, _) => IntPredicate::NE,
            (CmpBinOp::IsLT, IntSign::U) => IntPredicate::ULT,
            (CmpBinOp::IsLT, IntSign::S) => IntPredicate::SLT,
            (CmpBinOp::IsLE, IntSign::U) => IntPredicate::ULE,
            (CmpBinOp::IsLE, IntSign::S) => IntPredicate::SLE,
            (CmpBinOp::IsGT, IntSign::U) => IntPredicate::UGT,
            (CmpBinOp::IsGT, IntSign::S) => IntPredicate::SGT,
            (CmpBinOp::IsGE, IntSign::U) => IntPredicate::UGE,
            (CmpBinOp::IsGE, IntSign::S) => IntPredicate::SGE,
        };
        let id = b.build_int_compare(pred, lhs, rhs, "")?.into();
        Ok(Self { ty, id })
    }

    /// Build the given integer unary operation.
    pub fn build_int_uop(
        b: &Builder<'ctx>,
        ty: IntType,
        uop: IntUnaOp,
        src: [IntValue<'ctx>; 1],
    ) -> Result<Self, Error> {
        assert_eq!(ty, src[0].ty);

        let [src] = src.map(|v| v.id.into_int_value());
        let id = match uop {
            IntUnaOp::Neg => b.build_int_neg(src, "")?,
            IntUnaOp::Not => b.build_not(src, "")?,
        }.into();

        Ok(Self { ty, id })
    }

    /// Build a constant arbitrary-precision signed integer.
    pub fn build_const(
        b: &Builder<'ctx>,
        ty: IntType,
        val: &BigInt,
    ) -> Result<Self, Error> {
        let bits = val.bits().try_into().unwrap();
        let (sign, digits) = val.to_u64_digits();
        let mut val = b.ctx.custom_width_int_type(bits)
            .const_int_arbitrary_precision(&digits);
        if sign == Sign::Minus { val = val.const_neg(); }
        let int_ty = b.ctx.custom_width_int_type(ty.bits.get());
        val = val.const_cast(int_ty, ty.sign == IntSign::S);
        Ok(Self { ty, id: val.into() })
    }

    /// Build a poison value.
    pub fn build_poison(
        b: &Builder<'ctx>,
        ty: IntType,
    ) -> Result<Self, Error> {
        let id = get_poison(Self::llvm_ty(b.ctx, ty));
        Ok(Self { ty, id })
    }

    /// Build an undefined value.
    pub fn build_undef(
        b: &Builder<'ctx>,
        ty: IntType,
    ) -> Result<Self, Error> {
        let id = get_undef(Self::llvm_ty(b.ctx, ty));
        Ok(Self { ty, id })
    }

    /// Translate a type to LLVM.
    pub fn llvm_ty(
        c: &'ctx Context,
        ty: IntType,
    ) -> BasicTypeEnum<'ctx> {
        c.custom_width_int_type(ty.bits.get()).into()
    }
}

/// Get the `poison` value for a basic LLVM type.
fn get_poison<'ctx>(ty: BasicTypeEnum<'ctx>) -> BasicValueEnum<'ctx> {
    match ty {
        BasicTypeEnum::ArrayType(ty) => ty.get_poison().into(),
        BasicTypeEnum::FloatType(ty) => ty.get_poison().into(),
        BasicTypeEnum::IntType(ty) => ty.get_poison().into(),
        BasicTypeEnum::PointerType(ty) => ty.get_poison().into(),
        BasicTypeEnum::StructType(ty) => ty.get_poison().into(),
        BasicTypeEnum::VectorType(ty) => ty.get_poison().into(),
    }
}

/// Get the `undef` value for a basic LLVM type.
fn get_undef<'ctx>(ty: BasicTypeEnum<'ctx>) -> BasicValueEnum<'ctx> {
    match ty {
        BasicTypeEnum::ArrayType(ty) => ty.get_undef().into(),
        BasicTypeEnum::FloatType(ty) => ty.get_undef().into(),
        BasicTypeEnum::IntType(ty) => ty.get_undef().into(),
        BasicTypeEnum::PointerType(ty) => ty.get_undef().into(),
        BasicTypeEnum::StructType(ty) => ty.get_undef().into(),
        BasicTypeEnum::VectorType(ty) => ty.get_undef().into(),
    }
}
