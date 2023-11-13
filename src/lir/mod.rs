//! The Lower Intermediary Representation (LIR).

use core::cmp::Ord;

use thiserror::Error;

use inkwell::{
    self,
    builder::Builder,
    context::Context,
    values::{BasicValue, BasicValueEnum},
    types::{BasicType, BasicTypeEnum},
};

use crate::tck;
use crate::mir::ops;

/// Parse an MIR vector binary operation into LIR.
fn parse_vector_bin_op<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    bop: ops::VectorBinOp,
    lhs: BasicValueEnum<'ctx>,
    rhs: BasicValueEnum<'ctx>,
) -> Result<BasicValueEnum<'ctx>, Error> {
    Ok(match bop {

    })
}

/// Parse an MIR vector unary operation into LIR.
fn parse_vector_una_op<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    uop: ops::VectorUnaOp,
    src: BasicValueEnum<'ctx>,
) -> Result<BasicValueEnum<'ctx>, Error> {
    let dst_type = uop.dst_type();

    Ok(match uop {
        ops::VectorUnaOp::Map { uop, map: Some(map_type) } => {
            let dst_type = parse_vector_type(context, dst_type);
            build_vector_loop(
                context, builder, map_type.size,
                [dst_type.get_undef()], |index, [dst]| {

                let src_elem = build_extract_vector(
                    context, builder,
                    (map_type, uop.src_type()),
                    src, index)?;

                let dst_elem = parse_option_bin_op(context, builder, uop, src_elem)?;

                let dst = build_insert_vector(
                    context, builder,
                    (map_type, uop.dst_type()),
                    dst, index, dst_elem)?;

                Ok([dst])
            })?[0]
        },

        ops::VectorUnaOp::Map { uop, map: None } =>
            parse_option_una_op(context, builder, uop, src)?,

        ops::VectorUnaOp::New { src: src_type, map: map_type } => {
            let dst_type = parse_vector_type(context, dst_type);
            build_vector_loop(
                context, builder, map_type.size,
                [dst_type.get_undef()], |index, [dst]| {

                let dst = build_insert_vector(
                    context, builder,
                    (map_type, src_type),
                    dst, index, src)?;

                Ok([dst])
            })?[0]
        },
    })
}

/// Construct a vector iteration loop.
fn build_vector_loop<'ctx, F, const N: usize>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    size: u32,
    params: [BasicValueEnum<'ctx>; N],
    f: F,
) -> Result<[BasicValueEnum<'ctx>; N], Error>
where F: FnOnce(
          IntValue<'ctx>,
          [BasicValueEnum<'ctx>; N],
      ) -> Result<[BasicValueEnum<'ctx>; N], Error>
{
    let curr = builder.get_insert_block().unwrap();
    let test = context.insert_basic_block_after(curr, "");
    let body = context.insert_basic_block_after(test, "");
    let post = context.insert_basic_block_after(body, "");

    builder.build_unconditional_branch(test)?;

    builder.position_at_end(test);
    let count_type = context.i32_type();
    let count_phi = builder.build_phi(count_type, "")?;
    let count = count_phi.as_basic_value().into_int_value();
    let param_phis = params
        .try_map(|param| builder.build_phi(param.get_type(), ""))?;
    let param_vars = param_phis.map(|phi| phi.as_basic_value());
    let total = count_type.const_int(size, false);
    builder.build_switch(count, body, &[(total, post)])?;

    builder.position_at_end(body);
    let count_step = count_type.const_int(1);
    let count_next = builder.build_int_add(count, count_step, "")?;
    let param_news = (f)(count, param_vars)?;
    builder.build_unconditional_branch(test)?;

    builder.position_at_end(post);

    count.add_incoming(&[
        (&count_type.const_zero(), curr),
        (&count_next, test),
    ]);

    for i in 0 .. N {
        param_phis[i].add_incoming(&[
            (&params[i], curr),
            (&param_news[i], test),
        ]);
    }

    Ok(param_vars)
}

/// Index into a vector and load an element.
fn build_extract_vector<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    r#type: (VectorPart, OptionType),
    vector: ArrayValue<'ctx>,
    index: IntValue<'ctx>,
) -> Result<BasicValueEnum<'ctx>, Error> {
    let r#type = VectorType::new(r#type.1, Some(r#type.0));
    let lir_type = parse_vector_type(context, r#type);
    let ptr = builder.build_alloca(lir_type, "")?;
    builder.build_store(ptr, vector)?;

    let index_type = context.i32_type();
    let data_type = parse_scalar_type(context, r#type.option.scalar);
    let mask_type = context.custom_width_int_type(1);

    if let Some(OptionPart {}) = r#type.option.part {
        let data_ptr = unsafe {
            builder.build_in_bounds_gep(
                lir_type, ptr, &[
                    index_type.const_int(0),
                    index_type.const_int(0),
                    index,
                ], "")
        }?;

        let mask_ptr = unsafe {
            builder.build_in_bounds_gep(
                lir_type, ptr, &[
                    index_type.const_int(0),
                    index_type.const_int(1),
                    index,
                ], "")
        }?;

        let data = builder.build_load(data_type, data_ptr, "")?;
        let mask = builder.build_load(mask_type, mask_ptr, "")?;

        let res = iter_src_type.get_undef();
        let res = builder.build_insert_value(res, data, 0, "")?;
        let res = builder.build_insert_value(res, mask, 1, "")?;

        res.as_basic_value_enum()
    } else {
        let ptr = unsafe {
            builder.build_in_bounds_gep(
                lir_type, ptr, &[
                    index_type.const_int(0),
                    index,
                ], "")
        }?;

        builder
            .build_load(data_type, ptr, "")?
            .as_basic_value_enum()
    }
}

/// Index into a vector and store an element.
fn build_insert_vector<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    r#type: (VectorPart, OptionType),
    vector: ArrayValue<'ctx>,
    index: IntValue<'ctx>,
    elem: BasicValueEnum<'ctx>,
) -> Result<BasicValueEnum<'ctx>, Error> {
    let r#type = VectorType::new(r#type.1, Some(r#type.0));
    let lir_type = parse_vector_type(context, r#type);
    let ptr = builder.build_alloca(lir_type, "")?;
    builder.build_store(ptr, vector)?;

    let index_type = context.i32_type();

    if let Some(OptionPart {}) = r#type.option.part {
        let BasicValueEnum::StructValue(elem) = elem else { unreachable!() };

        let data_ptr = unsafe {
            builder.build_in_bounds_gep(
                lir_type, ptr, &[
                    index_type.const_int(0),
                    index_type.const_int(0),
                    index,
                ], "")
        }?;

        let mask_ptr = unsafe {
            builder.build_in_bounds_gep(
                lir_type, ptr, &[
                    index_type.const_int(0),
                    index_type.const_int(1),
                    index,
                ], "")
        }?;

        let data = builder.build_extract_value(elem, 0, "")?;
        let mask = builder.build_extract_value(elem, 1, "")?;

        builder.build_store(data_ptr, data)?;
        builder.build_store(mask_ptr, mask)?;
    } else {
        let ptr = unsafe {
            builder.build_in_bounds_gep(
                lir_type, ptr, &[
                    index_type.const_int(0),
                    index,
                ], "")
        }?;

        builder.build_store(ptr, elem)?;
    }

    builder.build_load(lir_type, ptr)?
}

/// Parse an MIR option binary operation into LIR.
fn parse_option_bin_op<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    bop: ops::OptionBinOp,
    lhs: BasicValueEnum<'ctx>,
    rhs: BasicValueEnum<'ctx>,
) -> Result<BasicValueEnum<'ctx>, Error> {
    let dst_type = bop.dst_type();

    Ok(match bop {
        ops::OptionBinOp::Map { bop, map: Some(map_type) } => {
            let tck::OptionPart {} = map_type;
            let BasicValueEnum::StructValue(lhs) = lhs else { unreachable!() };
            let BasicValueEnum::StructValue(rhs) = rhs else { unreachable!() };

            let lhs_data = builder.build_extract_value(lhs, 0, "")?;
            let lhs_mask = builder.build_extract_value(lhs, 1, "")?;
            let rhs_data = builder.build_extract_value(rhs, 0, "")?;
            let rhs_mask = builder.build_extract_value(rhs, 1, "")?;

            let dst_data = parse_scalar_bin_op(context, builder, bop, lhs_data, rhs_data)?;
            let dst_mask = builder.build_and(lhs_mask, rhs_mask, "");

            let dst = parse_option_type(context, dst_type).get_undef();
            let dst = builder.build_insert_value(dst, dst_data, 0, "")?;
            let dst = builder.build_insert_value(dst, dst_mask, 1, "")?;

            dst.as_basic_value_enum()
        },

        ops::OptionBinOp::Map { bop, map: None } =>
            parse_scalar_bin_op(context, builder, bop, lhs, rhs)?,

        ops::OptionBinOp::Cond { rhs: rhs_type } => {
            let BasicValueEnum::IntValue(lhs) = lhs else { unreachable!() };
            let BasicValueEnum::StructValue(rhs) = rhs else { unreachable!() };

            let rhs_data = builder.build_extract_value(rhs, 0, "")?;
            let rhs_mask = builder.build_extract_value(rhs, 1, "")?;

            let dst_data = rhs_data;
            let dst_mask = builder.build_and(lhs, rhs_mask, "");

            let dst = parse_option_type(context, dst_type).get_undef();
            let dst = builder.build_insert_value(dst, dst_data, 0, "")?;
            let dst = builder.build_insert_value(dst, dst_mask, 1, "")?;

            dst.as_basic_value_enum()
        },

        ops::OptionBinOp::Else { src: src_type, rhs: Some(OptionPart {}) } => {
            let BasicValueEnum::StructValue(lhs) = lhs else { unreachable!() };
            let BasicValueEnum::StructValue(rhs) = rhs else { unreachable!() };

            let lhs_data = builder.build_extract_value(lhs, 0, "")?;
            let lhs_mask = builder.build_extract_value(lhs, 1, "")?;

            let rhs_data = builder.build_extract_value(rhs, 0, "")?;
            let rhs_mask = builder.build_extract_value(rhs, 1, "")?;

            let dst_data = builder.build_select(lhs_mask, lhs_data, rhs_data, "")?;
            let dst_mask = builder.build_or(lhs_mask, rhs_mask, "");

            let dst = parse_option_type(context, dst_type).get_undef();
            let dst = builder.build_insert_value(dst, dst_data, 0, "")?;
            let dst = builder.build_insert_value(dst, dst_mask, 1, "")?;

            dst.as_basic_value_enum()
        },

        ops::OptionBinOp::Else { src: src_type, rhs: None } => {
            let BasicValueEnum::StructValue(lhs) = lhs else { unreachable!() };

            let lhs_data = builder.build_extract_value(lhs, 0, "")?;
            let lhs_mask = builder.build_extract_value(lhs, 1, "")?;

            builder.build_select(lhs_mask, lhs_data, rhs, "")?
        },
    })
}

/// Parse an MIR option unary operation into LIR.
fn parse_option_una_op<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    uop: ops::OptionUnaOp,
    src: BasicValueEnum<'ctx>,
) -> Result<BasicValueEnum<'ctx>, Error> {
    let dst_type = uop.dst_type();

    Ok(match uop {
        ops::OptionUnaOp::Map { uop, map: Some(map_type) } => {
            let tck::OptionPart {} = map_type;
            let BasicValueEnum::StructValue(src) = src else { unreachable!() };

            let src_data = builder.build_extract_value(src, 0, "")?;
            let src_mask = builder.build_extract_value(src, 1, "")?;

            let dst_data = parse_scalar_una_op(context, builder, uop, src_data)?;
            let dst_mask = src_mask;

            let dst = parse_option_type(context, dst_type).get_undef();
            let dst = builder.build_insert_value(dst, dst_data, 0, "")?;
            let dst = builder.build_insert_value(dst, dst_mask, 1, "")?;

            dst.as_basic_value_enum()
        },

        ops::OptionUnaOp::Map { uop, map: None } =>
            parse_scalar_una_op(context, builder, uop, src)?,

        ops::OptionUnaOp::New { src: _, map: map_type } => {
            let tck::OptionPart {} = map_type;

            let dst_data = src;
            let dst_mask = context
                .custom_width_int_type(1)
                .const_int(1, false);

            let dst = parse_option_type(context, dst_type).get_undef();
            let dst = builder.build_insert_value(dst, dst_data, 0, "")?;
            let dst = builder.build_insert_value(dst, dst_mask, 1, "")?;

            dst.as_basic_value_enum()
        },
    })
}

/// Parse an MIR scalar binary operation into LIR.
fn parse_scalar_bin_op<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    bop: ops::ScalarBinOp,
    lhs: BasicValueEnum<'ctx>,
    rhs: BasicValueEnum<'ctx>,
) -> Result<BasicValueEnum<'ctx>, Error> {
    Ok(match bop {
        ops::ScalarBinOp::Map { bop, map: tck::ScalarType::Int(map_type) } => {
            let BasicValueEnum::IntValue(lhs) = lhs else { unreachable!() };
            let BasicValueEnum::IntValue(rhs) = rhs else { unreachable!() };
            match (bop, map_type.sign) {
                (ops::ScalarMapBinOp::Add, _) =>
                    builder.build_int_add(lhs, rhs, "")?,
                (ops::ScalarMapBinOp::Sub, _) =>
                    builder.build_int_add(lhs, rhs, "")?,
                (ops::ScalarMapBinOp::Mul, _) =>
                    builder.build_int_add(lhs, rhs, "")?,

                (ops::ScalarMapBinOp::Div, tck::IntSign::U) =>
                    builder.build_int_unsigned_div(lhs, rhs, "")?,
                (ops::ScalarMapBinOp::Div, tck::IntSign::S) =>
                    builder.build_int_signed_div(lhs, rhs, "")?,
                (ops::ScalarMapBinOp::Rem, tck::IntSign::U) =>
                    builder.build_int_unsigned_rem(lhs, rhs, "")?,
                (ops::ScalarMapBinOp::Rem, tck::IntSign::S) =>
                    builder.build_int_signed_rem(lhs, rhs, "")?,

                ops::ScalarMapBinOp::And =>
                    builder.build_and(lhs, rhs, "")?,
                ops::ScalarMapBinOp::IOr =>
                    builder.build_or(lhs, rhs, "")?,
                ops::ScalarMapBinOp::XOr =>
                    builder.build_xor(lhs, rhs, "")?,
            }.as_basic_value_enum()
        },

        ops::ScalarBinOp::Sep {
            bop,
            lhs: tck::ScalarType::Int(lhs_type),
            rhs: tck::ScalarType::Int(rhs_type),
        } => {
            let lhs_lir_type = context.custom_width_int_type(lhs_type.bits);
            let BasicValueEnum::IntValue(lhs) = lhs else { unreachable!() };
            let BasicValueEnum::IntValue(rhs) = rhs else { unreachable!() };

            match bop {
                ops::ScalarSepBinOp::ShL | ops::ScalarSepBinOp::ShR => {
                    let rhs = match Ord::cmp(lhs_type.size, rhs_type.size) {
                        Ordering::Less => builder
                            .build_int_truncate(rhs, lhs_lir_type, "")?,
                        Ordering::Greater => builder
                            .build_int_z_extend(rhs, lhs_lir_type, "")?,
                        Ordering::Equal => rhs,
                    };

                    match (bop, lhs_type.sign) {
                        (ops::ScalarSepBinOp::ShL, _) =>
                            builder.build_left_shift(lhs, rhs, "")?,
                        (ops::ScalarSepBinOp::ShR, tck::IntSign::U) =>
                            builder.build_right_shift(lhs, rhs, false, "")?,
                        (ops::ScalarSepBinOp::ShR, tck::IntSign::S) =>
                            builder.build_right_shift(lhs, rhs, true, "")?,
                    }
                },
            }.as_basic_value_enum()
        },

        ops::ScalarBinOp::Cmp { bop, map: tck::ScalarType::Int(map_type) } => {
            let BasicValueEnum::IntValue(lhs) = lhs else { unreachable!() };
            let BasicValueEnum::IntValue(rhs) = rhs else { unreachable!() };

            let predicate = match (bop, map_type.sign) {
                (ops::ScalarCmpBinOp::IsEq, _) =>
                    inkwell::IntPredicate::EQ,
                (ops::ScalarCmpBinOp::IsNE, _) =>
                    inkwell::IntPredicate::NE,
                (ops::ScalarCmpBinOp::IsLT, tck::IntSign::U) =>
                    inkwell::IntPredicate::ULT,
                (ops::ScalarCmpBinOp::IsLT, tck::IntSign::S) =>
                    inkwell::IntPredicate::SLT,
                (ops::ScalarCmpBinOp::IsLE, tck::IntSign::U) =>
                    inkwell::IntPredicate::ULE,
                (ops::ScalarCmpBinOp::IsLE, tck::IntSign::S) =>
                    inkwell::IntPredicate::SLE,
                (ops::ScalarCmpBinOp::IsGT, tck::IntSign::U) =>
                    inkwell::IntPredicate::UGT,
                (ops::ScalarCmpBinOp::IsGT, tck::IntSign::S) =>
                    inkwell::IntPredicate::SGT,
                (ops::ScalarCmpBinOp::IsGE, tck::IntSign::U) =>
                    inkwell::IntPredicate::UGE,
                (ops::ScalarCmpBinOp::IsGE, tck::IntSign::S) =>
                    inkwell::IntPredicate::SGE,
            };

            builder
                .build_int_compare(predicate, lhs, rhs, "")?
                .as_basic_value_enum()
        },
    })
}

/// Parse an MIR scalar unary operation into LIR.
fn parse_scalar_una_op<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    uop: ops::ScalarUnaOp,
    src: BasicValueEnum<'ctx>,
) -> Result<BasicValueEnum<'ctx>, Error> {
    Ok(match uop {
        ops::ScalarUnaOp::Map { uop, map: tck::ScalarType::Int(_) } => {
            let BasicValueEnum::IntValue(src) = src else { unreachable!() };
            match uop {
                ops::ScalarMapUnaOp::Neg =>
                    builder.build_int_neg(src, "")?,
                ops::ScalarMapUnaOp::Not =>
                    builder.build_not(src, "")?,
            }.as_basic_value_enum()
        },
    })
}

/// Parse an MIR vector type into LIR.
fn parse_vector_type<'ctx>(
    context: &'ctx Context,
    r#type: tck::VectorType,
) -> BasicTypeEnum<'ctx> {
    let Some(vector) = r#type.part else {
        return parse_option_type(r#type.option);
    }

    let scalar = parse_scalar_type(context, r#type.option.scalar);
    if let Some(OptionPart {}) = r#type.option.part {
        context.struct_type(&[
            scalar.array_type(vector.size),
            context.custom_width_int_type(1).array_type(vector.size),
        ], false)
    } else {
        scalar.array_type(vector.size)
    }
}

/// Parse an MIR option type into LIR.
fn parse_option_type<'ctx>(
    context: &'ctx Context,
    r#type: tck::OptionType,
) -> BasicTypeEnum<'ctx> {
    let scalar = parse_scalar_type(context, r#type.scalar);
    if let Some(part) = r#type.part {
        context.struct_type(&[
            scalar,
            context.custom_width_int_type(1),
        ], false)
    } else { scalar }
}

/// Parse an MIR scalar type into LIR.
fn parse_scalar_type<'ctx>(
    context: &'ctx Context,
    r#type: tck::ScalarType,
) -> BasicTypeEnum<'ctx> {
    match r#type {
        tck::ScalarType::Int(r#type) =>
            context.custom_width_int_type(r#type.size.into()),
    }.as_basic_type_enum()
}

/// An error involving the LIR.
pub enum Error {
    #[error("An error occurred when building the LIR: {0}")]
    Builder(#[from] inkwell::builder::BuilderError),
}
