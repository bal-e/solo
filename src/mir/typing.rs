//! MIR types.

use std::iter;

use rustc_hash::FxHashMap;

/// A collection of MIR types.
#[derive(Debug, Default)]
pub struct TypeSet {
    /// A collection of mapping types.
    map_set: Vec<MapType>,
    /// A collection of fixed types.
    fix_set: Vec<FixType>,

    /// A lookup map for mapping types.
    map_map: FxHashMap<MapType, MapTypeID>,
    /// A lookup map for fixed types.
    fix_map: FxHashMap<FixType, FixTypeID>,
}

impl TypeSet {
    /// Construct a new [`TypeSet`].
    pub fn new() -> Self {
        Self::default()
    }

    /// Lookup an ID for the given [`MapType`].
    ///
    /// If the [`MapType`] was not already in the collection, it is added, and
    /// assigned a new ID.
    pub fn add_map(&mut self, t: MapType) -> MapTypeID {
        self.map_map.entry(t).or_insert_with(|| {
            let idx = self.map_set.len();
            self.map_set.push(t);
            MapTypeID(idx.try_into().unwrap())
        }).clone()
    }

    /// Lookup an ID for the given [`FixType`].
    ///
    /// If the [`FixType`] was not already in the collection, it is added, and
    /// assigned a new ID.
    pub fn add_fix(&mut self, t: FixType) -> FixTypeID {
        self.fix_map.entry(t).or_insert_with(|| {
            let idx = self.fix_set.len();
            self.fix_set.push(t);
            FixTypeID(idx.try_into().unwrap())
        }).clone()
    }

    /// Lookup a [`MapType`] from the given ID.
    ///
    /// Panics if the type could not be found.
    pub fn get_map(&self, id: MapTypeID) -> MapType {
        self.map_set[id.0 as usize]
    }

    /// Lookup a [`FixType`] from the given ID.
    ///
    /// Panics if the type could not be found.
    pub fn get_fix(&self, id: FixTypeID) -> FixType {
        self.fix_set[id.0 as usize]
    }

    /// Pair two [`MapType`]s.
    pub fn pair_maps(&mut self, mids: [MapTypeID; 2]) -> [MapTypeID; 2] {
        // Probe the LHS for the RHS.
        if iter::successors(Some(mids[0]), |n| Some(match self.get_map(*n) {
            MapType::Fix => return None,
            MapType::Opt(id) => id,
            MapType::Arr(id) => id,
        })).any(|n| n == mids[1]) {
            // The LHS contains the RHS; return only the LHS.
            return [mids[0], mids[0]];
        }

        // Probe the RHS for the LHS.
        if iter::successors(Some(mids[1]), |n| Some(match self.get_map(*n) {
            MapType::Fix => return None,
            MapType::Opt(id) => id,
            MapType::Arr(id) => id,
        })).any(|n| n == mids[0]) {
            // The RHS contains the LHS; return only the RHS.
            return [mids[1], mids[1]];
        }

        // The types are incompatible.
        panic!("Cannot pair the given types!")
    }
}

/// A mapping MIR type.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum MapType {
    /// A fixed type.
    Fix,
    /// An optional type.
    Opt(MapTypeID),
    /// A mapping array type.
    Arr(MapTypeID),
}

/// A reference to an MIR type through [`TypeSet`].
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MapTypeID(u32);

/// A fixed MIR type.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum FixType {
    /// An integer type.
    Int,
    /// An optional type.
    Opt(FixTypeID),
    /// An array type.
    Arr(FixTypeID),
}

/// A reference to a fixed MIR type through [`TypeSet`].
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FixTypeID(u32);
