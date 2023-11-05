//! Safely sharing collections thread-locally.

use core::cell::Cell;
use core::ops::{ControlFlow, FromResidual, Residual, Try};

use super::*;

/// A sharing wrapper around a [`Collection`].
pub struct Share<C> {
    inner: Cell<Option<C>>,
}

impl<C> Share<C> {
    /// Wrap the given collection in a [`Share`].
    pub fn new(inner: C) -> Self {
        Self { inner: Cell::new(Some(inner)) }
    }

    /// Destroy the [`Share`] and return the collection.
    pub fn into_inner(self) -> C {
        self.inner.into_inner()
            .expect("A shared collection tried to access itself!")
    }

    /// Get a new [`ShareStack`] for the given element type.
    pub fn stack<T>(&self) -> ShareStack<'_, C, T>
    where C: Collection<T> {
        ShareStack {
            share: self,
            stack: Cell::new(Some(Vec::new())),
        }
    }
}

impl<C: Collection<T>, T: ?Sized> Collection<T> for Share<C> {
    type ID = <C as Collection<T>>::ID;

    fn get(&self, id: Self::ID) -> T
    where T: Sized + Copy {
        let inner = self.inner.take()
            .expect("A shared collection tried to access itself!");
        let result = inner.get(id);
        self.inner.set(Some(inner));
        result
    }
}

impl<C: CollectionInsert<T>, T> CollectionInsert<T> for Share<C> {
    fn insert(&mut self, object: T) -> Self::ID {
        self.inner.get_mut().as_mut()
            .expect("A shared collection tried to access itself!")
            .insert(object)
    }
}

impl<C: CollectionExtend<T>, T> CollectionExtend<T> for Share<C> {
    fn extend<I>(&mut self, series: I) -> Self::SeriesID
    where I: IntoIterator<Item = T> {
        self.inner.get_mut().as_mut()
            .expect("A shared collection tried to access itself!")
            .extend(series)
    }

    fn try_extend<E, F, I>(
        &mut self,
        series: I,
    ) -> <F as Residual<Self::SeriesID>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = T, Residual = F>,
          F: Residual<Self::SeriesID> {
        self.inner.get_mut().as_mut()
            .expect("A shared collection tried to access itself!")
            .try_extend(series)
    }
}

impl<C: SeriesCollection<T>, T> SeriesCollection<T> for Share<C> {
    type SeriesID = <C as SeriesCollection<T>>::SeriesID;
}

/// A stack providing [`CollectionExtend`] on a [`Share`].
#[derive(Clone)]
pub struct ShareStack<'a, C: Collection<T>, T> {
    /// The underlying share.
    share: &'a Share<C>,

    /// A stack of items in the process of being added.
    stack: Cell<Option<Vec<T>>>,
}

impl<'a, C: Collection<T>, T> ShareStack<'a, C, T> {
    /// Get the underlying [`Share`].
    pub fn share(&self) -> &'a Share<C> {
        self.share
    }
}

impl<C: Collection<U>, T, U: ?Sized> Collection<U> for ShareStack<'_, C, T> {
    type ID = <C as Collection<T>>::ID;

    fn get(&self, id: Self::ID) -> T
    where T: Sized + Copy {
        self.share.get(id)
    }
}

impl<C: CollectionInsert<U>, T, U> CollectionInsert<U> for ShareStack<'_, C, T> {
    fn insert(&mut self, object: T) -> Self::ID {
        let inner = self.share.inner.take()
            .expect("A shared collection tried to access itself!");
        let result = inner.insert(object);
        self.share.inner.set(Some(inner));
        result
    }
}

impl<C: CollectionExtend<T>, T> CollectionExtend<T> for ShareStack<'_, C, T> {
    fn extend<I>(&mut self, series: I) -> Self::SeriesID
    where I: IntoIterator<Item = T> {
        // We load the series into the local stack first, so that the iterator
        // can use the collection however it wants.  Once that's done, we pass
        // the elements to the collection.

        let stack = self.stack.take()
            .expect("A shared collection tried to access itself!");
        let beg = stack.len();
        self.stack.set(Some(stack));

        for item in series {
            let mut stack = self.stack.take()
                .expect("A shared collection tried to access itself!");
            stack.push(item);
            self.stack.set(Some(stack));
        }

        let mut stack = self.stack.take()
            .expect("A shared collection tried to access itself!");
        let mut inner = self.share.inner.take()
            .expect("A shared collection tried to access itself!");

        let result = inner.extend(stack.drain(beg ..));

        self.share.inner.set(Some(inner));
        self.stack.set(Some(stack));

        result
    }

    fn try_extend<E, F, I>(
        &mut self,
        series: I,
    ) -> <F as Residual<Self::SeriesID>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = T, Residual = F>,
          F: Residual<Self::SeriesID> {
        // We load the series into the local stack first, so that the iterator
        // can use the collection however it wants.  Once that's done, we pass
        // the elements to the collection.

        let stack = self.stack.take()
            .expect("A shared collection tried to access itself!");
        let beg = stack.len();
        self.stack.set(Some(stack));

        for item in series {
            match item.branch() {
                ControlFlow::Continue(item) => {
                    let mut stack = self.stack.take()
                        .expect("A shared collection tried to access itself!");
                    stack.push(item);
                    self.stack.set(Some(stack));
                },
                ControlFlow::Break(residual) => {
                    let mut stack = self.stack.take()
                        .expect("A shared collection tried to access itself!");
                    stack.truncate(beg);
                    self.stack.set(Some(stack));

                    return FromResidual::from_residual(residual);
                },
            }
        }

        let mut stack = self.stack.take()
            .expect("A shared collection tried to access itself!");
        let mut inner = self.share.inner.take()
            .expect("A shared collection tried to access itself!");

        let result = inner.extend(stack.drain(beg ..));

        self.share.inner.set(Some(inner));
        self.stack.set(Some(stack));

        result
    }
}
