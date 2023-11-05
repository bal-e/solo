//! Storage for arbitrary-width integers.

use core::fmt;
use core::num::NonZeroI32;
use core::ops::{Deref, DerefMut};

use num_bigint::{BigInt, Sign};

use super::*;
use super::ident::*;

/// An integer.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Integer {
    inner: BigInt,
}

impl Deref for Integer {
    type Target = BigInt;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for Integer {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<C: Collector> Object<C> for Integer
where C::Collection<u32>: SeriesCollection<u32, SeriesID = SeriesID32<u32>> {
    type Collection = Integers<C::Collection<u32>>;
}

impl From<BigInt> for Integer {
    fn from(value: BigInt) -> Self {
        Self { inner: value }
    }
}

impl From<Integer> for BigInt {
    fn from(value: Integer) -> Self {
        value.inner
    }
}

impl fmt::Debug for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <BigInt as fmt::Debug>::fmt(self.inner, f)
    }
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <BigInt as fmt::Display>::fmt(self.inner, f)
    }
}

/// The identifier for an integer.
pub enum IntegerID {
    /// An inline integer.
    ///
    /// This is used for all integers that fit in 32 bits.
    Inline(i32),

    /// An externally-stored integer.
    ///
    /// The sign of the integer is encoded in the length.
    Extern {
        beg: ID32<u32>,
        len: NonZeroI32,
    }
}

impl IntegerID {
    /// Retrieve the ID of an externally-stored integer, or its value.
    fn extern_id(&self) -> Result<(SeriesID32<u32>, Sign), i32> {
        let (beg, len) = match self {
            Self::Inline(num) => return Err(num),
            Self::Extern { beg, len } => (beg, len),
        };

    }
}

impl Identifier for IntegerID {
    type Object = Integer;
}

/// A collection of [`Integer`]s.
pub struct Integers<C: SeriesCollection<u32>> {
    inner: C,
}

impl<C: SeriesCollection<u32> + Default> Default for Integers<C> {
    fn default() -> Self {
        Self { inner: C::default() }
    }
}

impl<C: SeriesCollection<u32>> Collection<Integer> for Integers<C>
where C: SeriesCollectionRef<u32> {
    type ID = IntegerID;

    unsafe fn get(&self, id: Self::ID) -> Integer
    where Integer: Sized + Clone {
        match id {
            IntegerID::Inline(num) => num.into(),
            IntegerID::Extern { beg, len } => {
                let (sign, len) = (if len.is_positive() {
                    Sign::Plus
                } else {
                    Sign::Minus
                }, len.get().unsigned_abs());

                let digits_id = SeriesID32::new(beg, len);
                let digits = unsafe { self.inner.get_series_ref(digits_id) };
                BigInt::from_slice(sign, digits);
            },
        }
    }
}

impl<C: SeriesCollection<u32>> CollectionInsertRef<Integer> for Integers<C>
where C: CollectionExtend<u32> {
    fn insert_ref(&mut self, object: &Integer) -> Self::ID {
        // If the number would fit inline, return it as-is.
        if let Ok(num) = i32::try_from(&*object) {
            return IntegerID::Inline(num);
        }

        // Add the digits to the collection.
        let id = self.inner.extend(object.inner.iter_u32_digits());
        let (beg, len) = id.into_beg_len();
        let len = i32::try_from(len).unwrap();

        // Prepare the resulting ID.
        let len = if object.inner.sign() == Sign::Plus { len } else { -len };
        IntegerID::Extern { beg, len }
    }
}
