//! An ID-providing arena allocator.
//!
//! This is a wrapper around [`typed_arena`] which annotates each object with a
//! [`u32`] identifier.  This is very useful for annotating the objects with an
//! external [`Vec`] of information.

use std::cell::Cell;
use std::ops::{Bound, Deref, Range, RangeBounds};
use std::slice::SliceIndex;

use typed_arena;

/// An ID-providing arena allocator.
pub struct Arena<T> {
    /// The underlying arena.
    inner: typed_arena::Arena<T>,
    /// The ID of the next-allocated object.
    counter: Cell<u32>,
}

impl<T> Arena<T> {
    /// Construct a new [`Arena`].
    pub fn new() -> Self {
        Self::default()
    }

    /// Allocate a new object.
    pub fn alloc(&self, object: T) -> Ref<'_, T> {
        let inner = self.inner.alloc(object);
        let id = self.counter.get();
        self.counter.set(id
            .checked_add(1)
            .expect("Arena IDs overflowed!"));
        Ref { inner, id }
    }

    /// Allocate a new series of objects.
    pub fn alloc_many<I>(&self, objects: I) -> RefMany<'_, T>
    where I: IntoIterator<Item = T> {
        let inner = self.inner.alloc_extend(objects);
        let id = self.counter.get();
        self.counter.set(inner.len()
            .try_into().ok()
            .and_then(|len| id.checked_add(len))
            .expect("Arena IDs overflowed!"));
        RefMany { inner, id }
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self {
            inner: typed_arena::Arena::new(),
            counter: Cell::new(0),
        }
    }
}

/// A reference to an object in an arena.
pub struct Ref<'a, T> {
    /// The objects location in the arena.
    inner: &'a T,
    /// The ID of the object.
    id: u32,
}

impl<'a, T> Ref<'a, T> {
    /// Get the ID of the referenced object.
    pub fn id(&self) -> u32 {
        self.id
    }
}

impl<'a, T> Deref for Ref<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner
    }
}

/// A reference to a series of objects in an arena.
pub struct RefMany<'a, T> {
    /// The objects' location in the arena.
    inner: &'a [T],
    /// The ID of the first object, if any.
    id: u32,
}

impl<'a, T> RefMany<'a, T> {
    /// Get the IDs of the referenced objects.
    pub fn ids(&self) -> Range<u32> {
        self.id .. self.id + self.inner.len() as u32
    }

    /// Index a certain object for a [`Ref`].
    pub fn index(&self, index: usize) -> Ref<'a, T> {
        let inner = &self.inner[index];
        let id = self.id + index as u32;
        Ref { inner, id }
    }

    /// Index a certain range of objects for a [`RefMany`].
    pub fn index_many<R>(&self, index: R) -> RefMany<'a, T>
    where R: RangeBounds<usize> + SliceIndex<[T], Output = [T]> {
        let id = self.id + match index.start_bound() {
            Bound::Included(&x) => x as u32,
            Bound::Excluded(&x) => 1 + x as u32,
            Bound::Unbounded => 0,
        };
        let inner = &self.inner[index];
        RefMany { inner, id }
    }
}

impl<'a, T> Deref for RefMany<'a, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.inner
    }
}
