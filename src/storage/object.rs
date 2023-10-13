use std::cell::Cell;
use std::num::NonZeroU32;
use std::ops::{ControlFlow, FromResidual, Residual, Try};

use typed_arena::Arena;

use super::*;

/// Storage for objects of a certain type.
pub struct ObjectStorage<T> {
    /// The underlying arena.
    inner: Arena<Stored<T>>,

    /// A stack of objects that might be added.
    stack: Cell<Option<Vec<T>>>,

    /// The ID of the next-allocated object.
    next_id: Cell<NonZeroU32>,
}

impl<T> ObjectStorage<T> {
    /// Construct a new [`ObjectStorage`].
    pub fn new() -> Self {
        Self::default()
    }

    /// Allocate a new ID number.
    fn alloc_idnum(&self) -> NonZeroU32 {
        let idnum = self.next_id.get();
        self.next_id.set(idnum
            .checked_add(1)
            .expect("Arena IDs overflowed!"));
        idnum
    }
}

impl<T> Store<T> for ObjectStorage<T> {
    type Stored<'a> = &'a Stored<T> where Self: 'a;

    fn store(&self, object: T) -> Self::Stored<'_> {
        let idnum = self.alloc_idnum();
        let object = Stored { inner: object, idnum };
        self.inner.alloc(object)
    }
}

impl<T> StoreMany<T> for ObjectStorage<T> {
    type Stored<'a> = &'a [Stored<T>] where Self: 'a;

    fn store_many<I>(&self, objects: I) -> Self::Stored<'_>
    where I: IntoIterator<Item = T> {
        self.inner.alloc_extend(objects.into_iter()
            .map(|inner| Stored { inner, idnum: self.alloc_idnum() }))
    }
}

impl<T> StoreTryMany<T> for ObjectStorage<T> {
    type Stored<'a> = &'a [Stored<T>] where Self: 'a;

    fn store_try_many<'a, E, F, I>(&'a self, objects: I)
        -> <F as Residual<Self::Stored<'_>>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = T, Residual = F>,
          F: Residual<Self::Stored<'a>> {
        // Get the original size of the stack.
        let stack = self.stack.take().unwrap();
        let orig_size = stack.len();
        self.stack.set(Some(stack));

        // Evaluate the iterator and add items to the stack.
        for object in objects {
            match object.branch() {
                ControlFlow::Continue(object) => {
                    let mut stack = self.stack.take().unwrap();
                    stack.push(object);
                    self.stack.set(Some(stack));
                },
                ControlFlow::Break(failure) => {
                    return FromResidual::from_residual(failure);
                },
            }
        }

        // Drain the stack and save the items.
        let mut stack = self.stack.take().unwrap();
        let result = self.store_many(stack.drain(orig_size ..));
        self.stack.set(Some(stack));

        Try::from_output(result)
    }
}

impl<T> Default for ObjectStorage<T> {
    fn default() -> Self {
        Self {
            inner: Arena::new(),
            stack: Cell::new(Some(Vec::new())),
            next_id: Cell::new(NonZeroU32::MIN),
        }
    }
}
