use core::convert::AsMut;

use rustc_hash::FxHashMap;

use crate::hir;

use super::*;

impl Function {
    /// Parse the HIR of a function.
    pub fn parse(i: &hir::Function) -> (Self, Storage) {
        let mut parser = Parser {
            storage: StorageMut::default(),
            hir: &i.body,
            map: FxHashMap::default(),
        };

        let name = i.name.clone();
        let body: &[_] = i.body.as_ref();
        let last = (body.len() - 1).into();
        let body = TypedSingleInst::parse(last, &mut parser);

        (Self { name, body }, parser.storage.into())
    }
}

impl TypedStreamInst {
    /// Parse the HIR of an expression.
    pub fn parse<'hir>(
        i: hir::Id,
        p: &mut Parser<'hir>,
    ) -> ID<Self> {
        // At this stage, it is not possible to chain streams - we have no
        // information about how many times any individual stream is used.
        // Instead, we collect every stream into an intermediary array.

        // Note that 'TypedSingleInst::parse()' may load a cached instruction,
        // but 'TypedStreamInst::parse()' does not - this ensures that the maps
        // of singular values within a loop are correctly independent.

        let dstt = p.hir[i].dstt;
        let src = TypedSingleInst::parse(i, p);
        let inst = StreamInst::Map(src);
        p.storage.streams.put(Self { dstt, inst })
    }
}

impl StreamInst {
    /// Parse the HIR of an expression.
    pub fn parse<'hir>(
        i: &'hir hir::TypedNode,
        p: &mut Parser<'hir>,
    ) -> Self {
        match i.node {
            hir::Node::Bin(bop, [lhs, rhs]) => {
                let lhs = TypedStreamInst::parse(lhs, p);
                let rhs = TypedStreamInst::parse(rhs, p);
                Self::Bin(bop, [lhs, rhs])
            },

            hir::Node::Una(uop, [src]) => {
                let src = TypedStreamInst::parse(src, p);
                Self::Una(uop, [src])
            },

            hir::Node::BitCast(cop, src) => {
                let src = TypedStreamInst::parse(src, p);
                Self::BitCast(cop, src)
            },

            hir::Node::MapCast(cop, src) => {
                if cop == CastOp::Stream {
                    let src = TypedSingleInst::parse(src, p);
                    Self::Map(src)
                } else {
                    let src = TypedStreamInst::parse(src, p);
                    Self::MapCast(cop, src)
                }
            },

            // Stream arguments are singles that get mapped.
            hir::Node::Arg(num) => {
                let src = TypedSingleInst {
                    dstt: i.dstt,
                    inst: SingleInst::Arg(num),
                };

                Self::Map(p.storage.singles.put(src))
            },

            // Integer literals are not streams.
            hir::Node::Int(_) => unreachable!(),
        }
    }
}

impl TypedSingleInst {
    /// Parse the HIR of an expression.
    pub fn parse<'hir>(
        i: hir::Id,
        p: &mut Parser<'hir>,
    ) -> ID<Self> {
        // If this node has already been parsed, return it as-is.
        if let Some(inst) = p.map.get(&i).copied() {
            return inst;
        }

        let dstt = p.hir[i].dstt;
        let inst = match dstt.stream {
            StreamPart::Some {} => {
                // Begin a new loop.
                let stream_beg = p.storage.streams.num();
                let single_beg = p.storage.singles.num();

                // Parse the streaming instruction.
                let inst = StreamInst::parse(&p.hir[i], p);
                let inst = TypedStreamInst { dstt, inst };
                let src = p.storage.streams.put(inst);

                // Collect the stream into an array.
                let inst = SingleInst::Col(SingleColOp::Arr, src);
                let inst = TypedSingleInst { dstt, inst };
                let inst = p.storage.singles.put(inst);

                // Mark the completion of the loop.
                let stream_end = p.storage.streams.num();
                let single_end = p.storage.singles.num();

                let streams = (stream_beg .. stream_end).into();
                let singles = (single_beg .. single_end).into();
                p.storage.loops.put(Loop { streams, singles });

                inst
            },

            StreamPart::None => {
                let inst = SingleInst::parse(&p.hir[i], p);
                p.storage.singles.put(Self { dstt, inst })
            },
        };

        // Cache the parsed node for future re-use.
        p.map.insert(i, inst);
        inst
    }
}

impl SingleInst {
    /// Parse the HIR of an expression.
    pub fn parse<'hir>(
        i: &'hir hir::TypedNode,
        p: &mut Parser<'hir>,
    ) -> Self {
        match i.node {
            hir::Node::Bin(bop, [lhs, rhs]) => {
                let lhs = TypedSingleInst::parse(lhs, p);
                let rhs = TypedSingleInst::parse(rhs, p);
                let bop = bop.try_into().unwrap();
                Self::Bin(bop, [lhs, rhs])
            },

            hir::Node::Una(uop, [src]) => {
                let src = TypedSingleInst::parse(src, p);
                let uop = uop.try_into().unwrap();
                Self::Una(uop, [src])
            },

            hir::Node::BitCast(cop, src) => {
                let src = TypedSingleInst::parse(src, p);
                let cop = cop.try_into().unwrap();
                Self::BitCast(cop, src)
            },

            hir::Node::MapCast(cop, src) => {
                let src = TypedSingleInst::parse(src, p);
                let cop = cop.try_into().unwrap();
                Self::MapCast(cop, src)
            },

            hir::Node::Arg(num) => Self::Arg(num),
            hir::Node::Int(ref val) => Self::Int(val.clone()),
        }
    }
}

/// A parser for Solo HIR code.
pub struct Parser<'hir> {
    /// Storage for the MIR being constructed.
    pub storage: StorageMut,

    /// The HIR data being parsed from.
    pub hir: &'hir hir::RecExpr<hir::TypedNode>,

    /// A mapping from HIR nodes to MIR nodes.
    pub map: FxHashMap<hir::Id, ID<TypedSingleInst>>,
}

impl<'hir, T> AsMut<T> for Parser<'hir>
where StorageMut: AsMut<T> {
    fn as_mut(&mut self) -> &mut T {
        self.storage.as_mut()
    }
}
