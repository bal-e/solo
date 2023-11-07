//! Storage for arbitrary-width integers.

use core::fmt;
use core::num::NonZeroI32;
use core::ops::{Deref, DerefMut};

use num_bigint::{BigInt, Sign};

use super::*;

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

impl Object for Integer {
    type ID = IntegerID;
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
        <BigInt as fmt::Debug>::fmt(&self.inner, f)
    }
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <BigInt as fmt::Display>::fmt(&self.inner, f)
    }
}

/// The identifier for an [`Integer`].
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum IntegerID {
    /// An inline integer.
    ///
    /// This is used for all integers that fit in 32 bits.
    Inline(i32),

    /// An externally-stored integer.
    ///
    /// The sign of the integer is encoded in the length.
    Extern {
        beg: ident::ID32<u32>,
        len: NonZeroI32,
    }
}

impl Ident for IntegerID {
    type Object = Integer;
}

impl fmt::Display for IntegerID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Inline(n) => write!(f, "= {}", n),
            Self::Extern { beg, len } => {
                if len.is_positive() {
                    f.write_str("(+) ")?;
                } else {
                    f.write_str("(-) ")?;
                }

                let len = len.get().unsigned_abs();
                let id = ident::SeqID32::try_from((*beg, len)).unwrap();
                <ident::SeqID32<_> as fmt::Display>::fmt(&id, f)
            },
        }
    }
}

/// A collection of [`Integer`]s.
pub struct Integers<'s, D: Disposition<'s>> {
    inner: D::SeqStorage<u32>,
}

impl<'s, D: Disposition<'s>> Default for Integers<'s, D>
where D::SeqStorage<u32>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

impl<'s, D: Disposition<'s>> Storage<'s, Integer> for Integers<'s, D> {
    type ID = IntegerID;
    type Disposition = D;
}

impl<'s, D: Disposition<'s>> StorageGet<'s, Integer> for Integers<'s, D>
where D::SeqStorage<u32>: SeqStorageGetTmp<'s, u32> {
    unsafe fn get(&self, id: Self::ID) -> Integer {
        match id {
            IntegerID::Inline(num) => BigInt::from(num).into(),
            IntegerID::Extern { beg, len } => {
                let (sign, len) = (if len.is_positive() {
                    Sign::Plus
                } else {
                    Sign::Minus
                }, len.get().unsigned_abs());

                let beg = ident::IDLen::from(usize::try_from(beg).unwrap());
                let len = usize::try_from(len).unwrap();
                let digits_id = ident::SeqIDLen::try_from((beg, len)).unwrap();
                unsafe { self.inner.get_seq_tmp(digits_id, |digits| {
                    BigInt::from_slice(sign, digits)
                }) }.into()
            },
        }
    }
}

impl<'s, D: Disposition<'s>> StoragePutTmp<'s, Integer> for Integers<'s, D>
where D::SeqStorage<u32>: SeqStoragePut<'s, u32> {
    fn put_tmp(&mut self, object: &Integer) -> Self::ID {
        // If the number would fit inline, return it as-is.
        if let Ok(num) = i32::try_from(object.deref()) {
            return IntegerID::Inline(num);
        }

        // Add the digits to the collection.
        let id = self.inner.put_seq(object.inner.iter_u32_digits());
        let (beg, len) = id.into();
        let beg = ident::ID32::try_from(usize::from(beg)).unwrap();
        let len = NonZeroI32::try_from(i32::try_from(len).unwrap()).unwrap();

        // Prepare the resulting ID.
        let len = if object.inner.sign() == Sign::Plus { len } else { -len };
        IntegerID::Extern { beg, len }
    }
}
