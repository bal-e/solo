//! Storage.

use core::convert::AsMut;
use core::ops::{ControlFlow, FromResidual, Try, Range, Residual};

mod ids;
pub use ids::*;

/// Immutable storage for objects.
#[derive(Clone, Debug)]
pub struct Storage<T> {
    inner: Box<[T]>,
}

impl<T> Storage<T> {
    /// Construct a new [`Storage`] from the given slice.
    pub fn new(inner: Box<[T]>) -> Self {
        Self { inner }
    }

    /// Access an object given its ID.
    pub fn get(&self, id: ID<T>) -> &T {
        &self.inner[<usize>::from(id)]
    }

    /// Access a sequence of objects given their ID.
    pub fn get_seq(&self, id: SeqID<T>) -> &[T] {
        &self.inner[<Range<usize>>::from(id)]
    }
}

/// Mutable storage for objects.
#[derive(Clone, Debug)]
pub struct StorageMut<T> {
    /// The primary list of objects.
    inner: Vec<T>,

    /// A temporary stack for fallible sequences.
    stack: Vec<T>,
}

impl<T> StorageMut<T> {
    /// Construct a new, empty [`StorageMut`].
    pub fn new(inner: Vec<T>) -> Self {
        Self { inner, stack: Vec::new() }
    }

    /// Access an object given its ID.
    pub fn get(&self, id: ID<T>) -> &T {
        &self.inner[<usize>::from(id)]
    }

    /// Access a sequence of objects given their ID.
    pub fn get_seq(&self, id: SeqID<T>) -> &[T] {
        &self.inner[<Range<usize>>::from(id)]
    }

    /// Insert an object and get its ID.
    pub fn put(&mut self, obj: T) -> ID<T> {
        let len = self.inner.len();
        self.inner.push(obj);
        len.into()
    }

    /// Insert a sequence of objects and get their ID.
    pub fn put_seq<I>(&mut self, seq: I) -> SeqID<T>
    where I: IntoIterator<Item = T> {
        let beg = self.inner.len();
        self.inner.extend(seq);
        let end = self.inner.len();
        (beg .. end).into()
    }

    /// Insert a fallible sequence of objects and get their ID.
    pub fn try_put_seq<I, E, R>(&mut self, seq: I) -> R::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = T, Residual = R>,
          R: Residual<SeqID<T>> {
        let beg = self.inner.len();
        for res in seq {
            match res.branch() {
                ControlFlow::Continue(obj) =>
                    self.inner.push(obj),
                ControlFlow::Break(res) =>
                    return FromResidual::from_residual(res),
            }
        }
        let end = self.inner.len();
        Try::from_output((beg .. end).into())
    }

    /// Insert a sequence of objects using the storage itself.
    pub fn put_seq_rec<This, F>(this: &mut This, seq: F) -> SeqID<T>
    where This: AsMut<Self>, F: FnMut(&mut This) -> Option<T> {
        let mut seq = seq;

        let beg = this.as_mut().stack.len();
        while let Some(obj) = (seq)(this) {
            this.as_mut().stack.push(obj);
        }

        let this = this.as_mut();
        let set = this.stack.drain(beg ..);
        let beg = this.inner.len();
        this.inner.extend(set);
        let end = this.inner.len();

        (beg .. end).into()
    }

    /// Insert a fallible sequence of objects using the storage itself.
    pub fn try_put_seq_rec<This, F, E, R>(this: &mut This, seq: F) -> R::TryType
    where This: AsMut<Self>,
          F: FnMut(&mut This) -> E,
          E: Try<Output = Option<T>, Residual = R>,
          R: Residual<SeqID<T>> {
        let mut seq = seq;

        let beg = this.as_mut().stack.len();
        loop {
            match (seq)(this).branch() {
                ControlFlow::Continue(obj) => {
                    let Some(obj) = obj else { break };
                    this.as_mut().stack.push(obj);
                },
                ControlFlow::Break(res) => {
                    this.as_mut().stack.truncate(beg);
                    return FromResidual::from_residual(res);
                }
            }
        }

        let this = this.as_mut();
        let set = this.stack.drain(beg ..);
        let beg = this.inner.len();
        this.inner.extend(set);
        let end = this.inner.len();

        Try::from_output((beg .. end).into())
    }
}

impl<T> Default for StorageMut<T> {
    fn default() -> Self {
        Self {
            inner: Vec::default(),
            stack: Vec::default(),
        }
    }
}
