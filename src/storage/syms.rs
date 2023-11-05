//! Storage for [`Symbol`]s.
//!
//! The code for this module was inspired by [`string-interner`], but seems to
//! be much simpler, uses [`rustc_hash`], and eliminates the requirement that
//! symbol IDs are convertable to/from [`usize`].
//!
//! [`string-interner`]: https://github.com/robbepop/string-interner

use core::fmt;
use core::hash::{Hash, Hasher};
use core::ops::Deref;

use hashbrown::hash_table::{self, HashTable};
use rustc_hash::FxHasher;

use super::*;

/// A symbol.
///
/// This is simply a newtype of [`str`], used to identify strings representing
/// identifiers that can be uniquely stored.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Symbol {
    inner: str,
}

impl Symbol {
    /// Treat a string as a [`Symbol`].
    pub fn new(inner: &str) -> &Self {
        // SAFETY: 'Self' is 'repr(transparent)'.
        unsafe { core::mem::transmute(inner) }
    }
}

impl Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <str as fmt::Debug>::fmt(self.inner, f)
    }
}

impl<C: Collector> Object<C> for Symbol
where Symbols<C::Collection<u8>>: Collection<Symbol> {
    type Collection = Symbols<C::Collection<u8>>;
}

/// The identifier for a symbol.
#[repr(transparent)]
pub struct SymbolID<C: SeriesCollection<u8>> {
    /// The ID of the underlying byte series.
    inner: C::SeriesID,
}

impl<C: SeriesCollection<u8>> Identifier for SymbolID<C> {
    type Object = Symbol;
}

/// A collection of [`Symbol`]s.
pub struct Symbols<C: SeriesCollection<u8>> {
    /// A hash table for deduplicating strings.
    hashes: HashTable<SymbolID<C>>,

    /// The contents of every contained string.
    contents: C,
}

impl<C: SeriesCollection<u8>> Symbols<C> {
    /// Retrieved an interned symbol.
    unsafe fn get(contents: &C, id: SymbolID<C>) -> &Symbol {
        // SAFETY:
        // - The caller guarantees that 'id' belongs to this collection.
        // - Thus the 'id' must have been created by 'insert_ref()'.
        // - 'insert_ref()' only takes valid strings as input.
        // - Thus the extracted item is also a valid string.
        let raw = unsafe { contents.get_series_ref(id.inner) };
        Symbol::new(unsafe { core::str::from_utf8_unchecked(raw) })
    }
}

impl<C: SeriesCollection<u8> + Default> Default for Symbols<C> {
    fn default() -> Self {
        Self {
            hashes: HashTable::default(),
            contents: C::default(),
        }
    }
}

impl<C: SeriesCollection<u8>> Collection<Symbol> for Symbols<C> {
    type ID = SymbolID<C>;

    unsafe fn get(&self, id: Self::ID) -> Symbol
    where Symbol: Sized + Copy {
        unreachable!()
    }
}

impl<C: SeriesCollection<u8>> CollectionRef<Symbol> for Symbols<C>
where C: SeriesCollectionRef<u8> {
    unsafe fn get_ref(&self, id: Self::ID) -> &Symbol {
        Self::get(&self.contents, id)
    }
}

impl<C: SeriesCollection<u8>> CollectionInsertRef<Symbol> for Symbols<C>
where C: SeriesCollectionRef<u8> + CollectionExtend<u8> {
    fn insert_ref(&mut self, object: &Symbol) -> Self::ID {
        /// Convenience function for hashing a [`Symbol`].
        fn hasher(input: &Symbol) -> u64 {
            let mut state = FxHasher::default();
            input.hash(&mut state);
            state.finish()
        }

        let hash = hasher(object);
        let iseq = |id| unsafe { Self::get(&self.contents, id) == object };
        match self.hashes.entry(hash, iseq, hasher) {
            hash_table::Entry::Occupied(entry) => {
                // The symbol is already loaded; finish.
                entry.get().clone()
            },
            hash_table::Entry::Vacant(entry) => {
                // The symbol didn't exist; add it.
                entry.insert(hash);
                SymbolID { inner: self.contents.extend(object.inner.bytes()) }
            },
        }
    }
}
