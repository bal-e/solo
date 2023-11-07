//! Storage for [`Symbol`]s.
//!
//! The code for this module was inspired by [`string-interner`], but seems to
//! be much simpler, uses [`rustc_hash`], and eliminates the requirement that
//! symbol IDs are convertable to/from [`usize`].
//!
//! [`string-interner`]: https://github.com/robbepop/string-interner

use core::cmp::Ordering;
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

impl Object for Symbol {
    type ID = SymbolID;
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <str as fmt::Debug>::fmt(&self.inner, f)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <str as fmt::Display>::fmt(&self.inner, f)
    }
}

/// The identifier for a symbol.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct SymbolID {
    /// The ID of the underlying byte series.
    inner: ident::SeqID32<u8>,
}

impl PartialOrd for SymbolID {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let [lhs, rhs] = [*self, *other]
            .map(|x| x.inner)
            .map(Range::<ident::ID32<u8>>::from)
            .map(|x| x.start);
        lhs.partial_cmp(&rhs)
    }
}

impl Ord for SymbolID {
    fn cmp(&self, other: &Self) -> Ordering {
        let [lhs, rhs] = [*self, *other]
            .map(|x| x.inner)
            .map(Range::<ident::ID32<u8>>::from)
            .map(|x| x.start);
        lhs.cmp(&rhs)
    }
}

impl Ident for SymbolID {
    type Object = Symbol;
}

/// A collection of [`Symbol`]s.
pub struct Symbols<'s, D: Disposition<'s>> {
    /// A hash table for deduplicating strings.
    hashes: HashTable<SymbolID>,

    /// The contents of every contained string.
    contents: D::SeqStorage<u8>,
}

impl<'s, D: Disposition<'s>> Symbols<'s, D> {
    /// Retrieved an interned symbol temporarily.
    unsafe fn get_sym_tmp<R, F>(
        contents: &D::SeqStorage<u8>,
        id: SymbolID,
        func: F,
    ) -> R
    where D::SeqStorage<u8>: SeqStorageGetTmp<'s, u8>,
          F: FnOnce(&Symbol) -> R {
        let range = Range::<u32>::from(id.inner);
        let range = [range.start, range.end]
            .map(usize::try_from)
            .map(Result::unwrap)
            .map(ident::IDLen::<u8>::from);
        let seqid = ident::SeqIDLen::from(range[0] .. range[1]);

        // SAFETY:
        // - The caller guarantees that 'id' belongs to this collection.
        // - Thus the 'id' must have been created by 'insert_ref()'.
        // - 'insert_ref()' only takes valid strings as input.
        // - Thus the extracted item is also a valid string.
        unsafe { contents.get_seq_tmp(seqid, |raw| {
            let str = core::str::from_utf8_unchecked(raw);
            let sym = Symbol::new(str);
            (func)(sym)
        }) }
    }
}

impl<'s, D: Disposition<'s>> Default for Symbols<'s, D>
where D::SeqStorage<u8>: Default {
    fn default() -> Self {
        Self {
            hashes: HashTable::default(),
            contents: Default::default(),
        }
    }
}

impl<'s, D: Disposition<'s>> Storage<'s, Symbol> for Symbols<'s, D> {
    type ID = SymbolID;
    type Disposition = D;
}

impl<'s, D: Disposition<'s>> StorageGetTmp<'s, Symbol> for Symbols<'s, D>
where D::SeqStorage<u8>: SeqStorageGetTmp<'s, u8> {
    unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
    where F: FnOnce(&Symbol) -> R {
        Self::get_sym_tmp(&self.contents, id, func)
    }
}

impl<'s, D: Disposition<'s>> StorageGetRef<'s, Symbol> for Symbols<'s, D>
where D::SeqStorage<u8>: SeqStorageGetRef<'s, u8> {
    unsafe fn get_ref(&self, id: Self::ID) -> &Symbol {
        use core::str::from_utf8_unchecked;

        let range = Range::<u32>::from(id.inner);
        let range = [range.start, range.end]
            .map(usize::try_from)
            .map(Result::unwrap)
            .map(ident::IDLen::<u8>::from);
        let seqid = ident::SeqIDLen::from(range[0] .. range[1]);

        let raw = unsafe { self.contents.get_seq_ref(seqid) };
        let str = unsafe { from_utf8_unchecked(raw) };
        Symbol::new(str)
    }
}

impl<'s, D: Disposition<'s>> StoragePutTmp<'s, Symbol> for Symbols<'s, D>
where D::SeqStorage<u8>: SeqStorageGetTmp<'s, u8> + SeqStoragePutTmp<'s, u8> {
    fn put_tmp(&mut self, object: &Symbol) -> Self::ID {
        /// Convenience function for hashing a [`Symbol`].
        fn hasher(input: &Symbol) -> u64 {
            let mut state = FxHasher::default();
            input.hash(&mut state);
            state.finish()
        }

        let key = hasher(object);
        let hash = |&id: &_| unsafe {
            Self::get_sym_tmp(&self.contents, id, hasher)
        };
        let iseq = |&id: &_| unsafe {
            Self::get_sym_tmp(&self.contents, id, |symbol| object == symbol)
        };

        match self.hashes.entry(key, iseq, hash) {
            hash_table::Entry::Occupied(entry) => {
                // The symbol is already loaded; finish.
                entry.get().clone()
            },
            hash_table::Entry::Vacant(entry) => {
                // The symbol didn't exist; add it.
                let bytes = object.inner.as_bytes();
                let seqid = self.contents.put_seq_tmp(bytes);
                let range = Range::<usize>::from(seqid);
                let range = [range.start, range.end]
                    .map(ident::ID32::<u8>::try_from)
                    .map(Result::unwrap);
                let seqid = ident::SeqID32::from(range[0] .. range[1]);
                let symid = SymbolID { inner: seqid };
                entry.insert(symid);
                symid
            },
        }
    }
}
