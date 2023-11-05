//! Identifiers for objects in storage.
//!
//! In general, 32-bit identifiers are used for efficiency.  It is very unlikely
//! that more than 4 billion objects of any kind will exist simultaneously.  For
//! niche optimization, [`NonZeroU32`] is used in particular.

use core::fmt;
use core::iter::Step;
use core::marker::PhantomData;
use core::num::NonZeroU32;
use core::ops::Range;

use super::{Identifier, SeriesIdentifier};

/// A 32-bit numeric identifier for an [`Object`].
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ID32<T: ?Sized> {
    inner: NonZeroU32,
    _data: PhantomData<T>,
}

impl<T: ?Sized> ID32<T> {
    /// Construct a new [`ID32`] from the given index.
    ///
    /// This function will panic if given [`u32::MAX`] or greater.
    pub fn new(index: usize) -> Self {
        // Increment the index to avoid the niche slot.
        let index = u32::try_from(index)
            .and_then(|index| index.checked_add(1))
            .expect("Only indices smaller than `u32::MAX` are allowed!");

        // SAFETY: We checked that 'index + 1' did not overflow, so it must be
        // strictly greater than zero.
        let index = unsafe { NonZeroU32::new_unchecked(index) };

        Self { inner: index, _data: PhantomData }
    }
}

impl<T: ?Sized> Identifier for ID32<T> {
    type Object = T;
}

impl<T: ?Sized> fmt::Debug for ID32<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <u32 as fmt::Debug>::fmt(&(self.inner.get() - 1), f)
    }
}

impl<T: ?Sized> Step for ID32<T> {
    fn steps_between(beg: &Self, end: &Self) -> Option<usize> {
        let [beg, end] = [*beg, *end].map(usize::from);
        end.checked_sub(beg)
    }

    fn forward_checked(beg: Self, num: usize) -> Option<Self> {
        u32::try_from(num)
            .and_then(|num| beg.inner.checked_add(num))
            .map(|index| Self { inner: index, _data: PhantomData })
    }

    fn backward_checked(beg: Self, num: usize) -> Option<Self> {
        u32::try_from(num)
            .and_then(|num| u32::from(beg).checked_sub(num))
            .and_then(NonZeroU32::new)
            .map(|index| Self { inner: index, _data: PhantomData })
    }
}

impl<T: ?Sized> From<ID32<T>> for u32 {
    fn from(value: ID32<T>) -> Self {
        value.inner.get() - 1
    }
}

impl<T: ?Sized> From<ID32<T>> for usize {
    fn from(value: ID32<T>) -> Self {
        (value.inner.get() - 1).try_into().unwrap()
    }
}

/// A 32-bit numeric identifier for a series of [`Object`]s.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SeriesID32<T: ?Sized> {
    beg: ID32<T>,
    len: u32,
}

impl<T: ?Sized> SeriesID32<T> {
    /// Construct a new [`SeriesID32`].
    pub fn new(beg: ID32<T>, len: u32) -> Self {
        Self { beg, len }
    }
}

impl<T: ?Sized> SeriesIdentifier for SeriesID32<T> {
    type Single = ID32<T>;
    type Object = T;
}

impl<T: ?Sized> From<Range<ID32<T>>> for SeriesID32<T> {
    fn from(value: Range<ID32<T>>) -> Self {
        let [beg, end] = [value.start, value.end].map(u32::from);
        let len = end.checked_sub(beg).unwrap_or(0);
        Self { beg: value.start, len }
    }
}

impl<T: ?Sized> From<SeriesID32<T>> for Range<ID32<T>> {
    fn from(value: SeriesID32<T>) -> Self {
        let end = value.beg.inner.checked_add(value.len).unwrap();
        Range {
            start: value.beg,
            end: ID32 { inner: end, _data: PhantomData },
        }
    }
}
