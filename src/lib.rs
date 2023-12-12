#![feature(assert_matches)]
#![feature(array_try_map)]
#![feature(try_trait_v2)]
#![feature(try_trait_v2_residual)]

pub mod src;
pub mod ast;
pub mod tck;
pub mod hir;
pub mod mir;
pub mod lir;

pub mod ops;
pub mod prec;
pub mod tys;

pub mod soa;
