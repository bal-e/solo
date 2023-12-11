use core::convert::AsMut;

use crate::hir;

use super::*;

impl Function {
    /// Parse the HIR of a function.
    pub fn parse(i: &hir::Function) -> Self {
        let mut parser = Parser {
            storage: StorageMut::default(),
            hir: &i.body,
        };

        let name = i.name.clone();
        let body: &[_] = i.body.as_ref();
        let last = body.last().unwrap();
        let body = FunctionBody::parse(last, &mut parser);

        Self { name, body }
    }
}

impl FunctionBody {
    /// Parse the HIR of an expression.
    pub fn parse<'hir>(
        i: &'hir hir::TypedNode,
        p: &mut Parser<'hir>,
    ) -> Self {
        todo!()
    }
}

impl TypedStreamInst {
    /// Parse the HIR of an expression.
    pub fn parse<'hir>(
        i: &'hir hir::TypedNode,
        p: &mut Parser<'hir>,
    ) -> Self {
        todo!()
    }
}

impl StreamInst {
    /// Parse the HIR of an expression.
    pub fn parse<'hir>(
        i: &'hir hir::TypedNode,
        p: &mut Parser<'hir>,
    ) -> Self {
        todo!()
    }
}

impl TypedSingleInst {
    /// Parse the HIR of an expression.
    pub fn parse<'hir>(
        i: &'hir hir::TypedNode,
        p: &mut Parser<'hir>,
    ) -> Self {
        todo!()
    }
}

impl SingleInst {
    /// Parse the HIR of an expression.
    pub fn parse<'hir>(
        i: &'hir hir::TypedNode,
        p: &mut Parser<'hir>,
    ) -> Self {
        todo!()
    }
}

/// A parser for Solo HIR code.
pub struct Parser<'hir> {
    /// Storage for the MIR being constructed.
    pub storage: StorageMut,

    /// The HIR data being parsed from.
    pub hir: &'hir hir::RecExpr<hir::TypedNode>,
}

impl<'hir, T> AsMut<T> for Parser<'hir>
where StorageMut: AsMut<T> {
    fn as_mut(&mut self) -> &mut T {
        self.storage.as_mut()
    }
}
