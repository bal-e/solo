//! The Lower Intermediary Representation (LIR).

use core::ops::Deref;

use thiserror::Error;

use inkwell::{
    self,
    passes::PassBuilderOptions,
    targets::{self, Target, TargetMachine},
};

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

    /// Verify the consistency of the module.
    pub fn verify(&self) {
        self.inner.verify().unwrap();
    }

    /// Optimize the module.
    pub fn optimize(&self) {
        let config = targets::InitializationConfig::default();
        Target::initialize_all(&config);
        let target_triple = TargetMachine::get_default_triple();
        let cpu = TargetMachine::get_host_cpu_name();
        let cpu = cpu.to_str().unwrap();
        let cpu_features = TargetMachine::get_host_cpu_features();
        let cpu_features = cpu_features.to_str().unwrap();
        let opt_level = inkwell::OptimizationLevel::Aggressive;
        let reloc_mode = targets::RelocMode::Default;
        let code_model = targets::CodeModel::Default;
        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target.create_target_machine(
            &target_triple,
            cpu,
            cpu_features,
            opt_level,
            reloc_mode,
            code_model,
        ).unwrap();
        let pass_opts = PassBuilderOptions::create();
        pass_opts.set_loop_interleaving(true);
        pass_opts.set_loop_vectorization(true);
        pass_opts.set_loop_slp_vectorization(true);
        pass_opts.set_loop_unrolling(true);
        self.run_passes("default<O3>", &target_machine, pass_opts)
            .expect("LLVM optimizations should not fail");
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
