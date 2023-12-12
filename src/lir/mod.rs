//! The Lower Intermediary Representation (LIR).

use core::ops::Deref;

use thiserror::Error;

use inkwell;

pub mod build;
pub mod parse;

pub struct Builder<'ctx> {
    ctx: &'ctx Context,
    inner: inkwell::builder::Builder<'ctx>,
}

impl<'ctx> Deref for Builder<'ctx> {
    type Target = inkwell::builder::Builder<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// LIR context.
pub struct Context {
    inner: inkwell::context::Context,
}

impl Context {
    /// Construct a new [`Context`].
    pub fn new() -> Self {
        Self { inner: inkwell::context::Context::create() }
    }

    /// Create a new [`Builder`].
    pub fn create_builder(&self) -> Builder<'_> {
        Builder { ctx: self, inner: self.inner.create_builder() }
    }
}

impl Deref for Context {
    type Target = inkwell::context::Context;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// The LIR of a module.
pub struct Module<'ctx> {
    inner: inkwell::module::Module<'ctx>,
}

impl<'ctx> Module<'ctx> {
    /// Construct a new [`Module`] of the given name.
    pub fn new(ctx: &'ctx Context, name: &str) -> Self {
        Self {
            inner: ctx.inner.create_module(name),
        }
    }

    /// Render the module as textual LLVM IR.
    pub fn as_text(&self) -> String {
        self.inner.to_string()
    }
}

impl<'ctx> Deref for Module<'ctx> {
    type Target = inkwell::module::Module<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// The LIR of a function.
pub struct Function<'ctx> {
    inner: inkwell::values::FunctionValue<'ctx>,
}

impl<'ctx> Deref for Function<'ctx> {
    type Target = inkwell::values::FunctionValue<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// An error involving the LIR.
#[derive(Debug, Error)]
pub enum Error {
    #[error("An error occurred when building the LIR: {0}")]
    Builder(#[from] inkwell::builder::BuilderError),
}
