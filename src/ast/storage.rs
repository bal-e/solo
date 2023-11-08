//! Storage for the AST.

use core::ops::{Residual, Try};

use crate::storage::{self, *};
use crate::storage::ints::{Integer, Integers};
use crate::storage::syms::{Symbol, Symbols};

use super::*;

/// AST storage.
pub struct Storage<D: Disposition> {
    modules: Modules<D>,
    functions: Functions<D>,
    arguments: Arguments<D>,
    types: Types<D>,
    stmts: Stmts<D>,
    exprs: Exprs<D>,
    integers: Integers<D>,
    symbols: Symbols<D>,
}

impl<D: Disposition> Default for Storage<D>
where Modules<D>: Default,
      Functions<D>: Default,
      Arguments<D>: Default,
      Types<D>: Default,
      Stmts<D>: Default,
      Exprs<D>: Default,
      Integers<D>: Default,
      Symbols<D>: Default {

    fn default() -> Self {
        Self {
            modules: Default::default(),
            functions: Default::default(),
            arguments: Default::default(),
            types: Default::default(),
            stmts: Default::default(),
            exprs: Default::default(),
            integers: Default::default(),
            symbols: Default::default(),
        }
    }
}

macro_rules! fwd_storage {
    ($os:ty, $f:ident, $is:ty, $t:ty) => {
        impl<D> crate::storage::Storage<$t> for $os
        where D: crate::storage::Disposition,
              $is: crate::storage::Storage<$t> {
            type ID = <$is as crate::storage::Storage<$t>>::ID;
            type Disposition = D;
        }

        impl<'s, D> crate::storage::Storage<$t> for &'s $os
        where D: crate::storage::Disposition,
              &'s $is: crate::storage::Storage<$t> {
            type ID = <&'s $is as crate::storage::Storage<$t>>::ID;
            type Disposition = D;
        }

        impl<'s, D> crate::storage::Storage<$t> for &'s mut $os
        where D: crate::storage::Disposition,
              &'s mut $is: crate::storage::Storage<$t> {
            type ID = <&'s mut $is as crate::storage::Storage<$t>>::ID;
            type Disposition = D;
        }

        impl<D> crate::storage::StorageGet<$t> for $os
        where D: crate::storage::Disposition,
              $is: crate::storage::StorageGet<$t> {
            unsafe fn get(&self, id: Self::ID) -> $t {
                (&self.$f).get(id)
            }
        }

        impl<'s, D> crate::storage::StorageGet<$t> for &'s $os
        where D: crate::storage::Disposition,
              &'s $is: crate::storage::StorageGet<$t> {
            unsafe fn get(&self, id: Self::ID) -> $t {
                (&&self.$f).get(id)
            }
        }

        impl<'s, D> crate::storage::StorageGet<$t> for &'s mut $os
        where D: crate::storage::Disposition,
              &'s mut $is: crate::storage::StorageGet<$t> {
            unsafe fn get(&self, id: Self::ID) -> $t {
                (&&mut self.$f).get(id)
            }
        }

        impl<D> crate::storage::StoragePut<$t> for $os
        where D: crate::storage::Disposition,
              $is: crate::storage::StoragePut<$t> {
            fn put(&mut self, object: $t) -> Self::ID {
                (&mut self.$f).put(object)
            }
        }

        impl<'s, D> crate::storage::StoragePut<$t> for &'s $os
        where D: crate::storage::Disposition,
              &'s $is: crate::storage::StoragePut<$t> {
            fn put(&mut self, object: $t) -> Self::ID {
                (&mut &self.$f).put(object)
            }
        }

        impl<'s, D> crate::storage::StoragePut<$t> for &'s mut $os
        where D: crate::storage::Disposition,
              &'s mut $is: crate::storage::StoragePut<$t> {
            fn put(&mut self, object: $t) -> Self::ID {
                (&mut &mut self.$f).put(object)
            }
        }
    }
}

macro_rules! fwd_storage_ref {
    ($os:ty, $f:ident, $is:ty, $t:ty) => {
        impl<D> crate::storage::Storage<$t> for $os
        where D: crate::storage::Disposition,
              $is: crate::storage::Storage<$t> {
            type ID = <$is as crate::storage::Storage<$t>>::ID;
            type Disposition = D;
        }

        impl<'s, D> crate::storage::Storage<$t> for &'s $os
        where D: crate::storage::Disposition,
              &'s $is: crate::storage::Storage<$t> {
            type ID = <&'s $is as crate::storage::Storage<$t>>::ID;
            type Disposition = D;
        }

        impl<'s, D> crate::storage::Storage<$t> for &'s mut $os
        where D: crate::storage::Disposition,
              &'s mut $is: crate::storage::Storage<$t> {
            type ID = <&'s mut $is as crate::storage::Storage<$t>>::ID;
            type Disposition = D;
        }

        impl<D> crate::storage::StorageGetTmp<$t> for $os
        where D: crate::storage::Disposition,
              $is: crate::storage::StorageGetTmp<$t> {
            unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
            where F: FnOnce(&$t) -> R {
                (&self.$f).get_tmp(id, func)
            }
        }

        impl<'s, D> crate::storage::StorageGetTmp<$t> for &'s Storage<D>
        where D: crate::storage::Disposition,
              &'s $is: crate::storage::StorageGetTmp<$t> {
            unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
            where F: FnOnce(&$t) -> R {
                (&&self.$f).get_tmp(id, func)
            }
        }

        impl<'s, D> crate::storage::StorageGetTmp<$t> for &'s mut Storage<D>
        where D: crate::storage::Disposition,
              &'s mut $is: crate::storage::StorageGetTmp<$t> {
            unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
            where F: FnOnce(&$t) -> R {
                (&&mut self.$f).get_tmp(id, func)
            }
        }

        impl<D> crate::storage::StorageGetRef<$t> for $os
        where D: crate::storage::Disposition,
              $is: crate::storage::StorageGetRef<$t> {
            unsafe fn get_ref(&self, id: Self::ID) -> &$t {
                (&self.$f).get_ref(id)
            }
        }

        impl<'s, D> crate::storage::StorageGetRef<$t> for &'s $os
        where D: crate::storage::Disposition,
              &'s $is: crate::storage::StorageGetRef<$t> {
            unsafe fn get_ref(&self, id: Self::ID) -> &$t {
                (&&self.$f).get_ref(id)
            }
        }

        impl<'s, D> crate::storage::StorageGetRef<$t> for &'s mut $os
        where D: crate::storage::Disposition,
              &'s mut $is: crate::storage::StorageGetRef<$t> {
            unsafe fn get_ref(&self, id: Self::ID) -> &$t {
                (&&mut self.$f).get_ref(id)
            }
        }

        impl<D> crate::storage::StoragePutTmp<$t> for $os
        where D: crate::storage::Disposition,
              $is: crate::storage::StoragePutTmp<$t> {
            fn put_tmp(&mut self, object: &$t) -> Self::ID {
                (&mut self.$f).put_tmp(object)
            }
        }

        impl<'s, D> crate::storage::StoragePutTmp<$t> for &'s $os
        where D: crate::storage::Disposition,
              &'s $is: crate::storage::StoragePutTmp<$t> {
            fn put_tmp(&mut self, object: &$t) -> Self::ID {
                (&mut &self.$f).put_tmp(object)
            }
        }

        impl<'s, D> crate::storage::StoragePutTmp<$t> for &'s mut $os
        where D: crate::storage::Disposition,
              &'s mut $is: crate::storage::StoragePutTmp<$t> {
            fn put_tmp(&mut self, object: &$t) -> Self::ID {
                (&mut &mut self.$f).put_tmp(object)
            }
        }
    }
}

macro_rules! fwd_seq_storage {
    ($os:ty, $f:ident, $is:ty, $t:ty) => {
        fwd_storage!($os, $f, $is, $t);

        impl<D> crate::storage::SeqStorage<$t> for $os
        where D: crate::storage::Disposition,
              $is: crate::storage::SeqStorage<$t> {
            type SeqID = <$is as crate::storage::SeqStorage<$t>>::SeqID;
        }

        impl<'s, D> crate::storage::SeqStorage<$t> for &'s $os
        where D: crate::storage::Disposition,
              &'s $is: crate::storage::SeqStorage<$t> {
            type SeqID = <&'s $is as crate::storage::SeqStorage<$t>>::SeqID;
        }

        impl<'s, D> crate::storage::SeqStorage<$t> for &'s mut $os
        where D: crate::storage::Disposition,
              &'s mut $is: crate::storage::SeqStorage<$t> {
            type SeqID = <&'s mut $is as crate::storage::SeqStorage<$t>>::SeqID;
        }

        impl<D> crate::storage::SeqStorageGet<$t> for $os
        where D: crate::storage::Disposition,
              $is: crate::storage::SeqStorageGet<$t> {
            type Seq<'t>
                = <$is as crate::storage::SeqStorageGet<$t>>::Seq<'t>
                where Self: 't, $t: 't;

            unsafe fn get_seq<'t>(&'t self, id: Self::SeqID) -> Self::Seq<'t>
            where $t: 't {
                (&self.$f).get_seq(id)
            }
        }

        impl<'s, D> crate::storage::SeqStorageGet<$t> for &'s $os
        where D: crate::storage::Disposition,
              &'s $is: crate::storage::SeqStorageGet<$t> {
            type Seq<'t>
                = <&'s $is as crate::storage::SeqStorageGet<$t>>::Seq<'t>
                where Self: 't, $t: 't;

            unsafe fn get_seq<'t>(&'t self, id: Self::SeqID) -> Self::Seq<'t>
            where $t: 't {
                (&self.$f).get_seq(id)
            }
        }

        impl<'s, D> crate::storage::SeqStorageGet<$t> for &'s mut $os
        where D: crate::storage::Disposition,
              &'s mut $is: crate::storage::SeqStorageGet<$t> {
            type Seq<'t>
                = <&'s mut $is as crate::storage::SeqStorageGet<$t>>::Seq<'t>
                where Self: 't, $t: 't;

            unsafe fn get_seq<'t>(&'t self, id: Self::SeqID) -> Self::Seq<'t>
            where $t: 't {
                (&&mut self.$f).get_seq(id)
            }
        }

        impl<D> crate::storage::SeqStoragePut<$t> for $os
        where D: crate::storage::Disposition,
              $is: crate::storage::SeqStoragePut<$t> {
            fn put_seq<I>(&mut self, series: I) -> Self::SeqID
            where I: IntoIterator<Item = $t> {
                self.$f.put_seq(series)
            }

            fn try_put_seq<E, F, I>(
                &mut self,
                series: I,
            ) -> <F as Residual<Self::SeqID>>::TryType
            where I: IntoIterator<Item = E>,
                  E: Try<Output = $t, Residual = F>,
                  F: Residual<Self::SeqID> {
                self.$f.try_put_seq(series)
            }
        }

        impl<'s, D> crate::storage::SeqStoragePut<$t> for &'s $os
        where D: crate::storage::Disposition,
              &'s $is: crate::storage::SeqStoragePut<$t> {
            fn put_seq<I>(&mut self, series: I) -> Self::SeqID
            where I: IntoIterator<Item = $t> {
                (&mut &self.$f).put_seq(series)
            }

            fn try_put_seq<E, F, I>(
                &mut self,
                series: I,
            ) -> <F as Residual<Self::SeqID>>::TryType
            where I: IntoIterator<Item = E>,
                  E: Try<Output = $t, Residual = F>,
                  F: Residual<Self::SeqID> {
                (&mut &self.$f).try_put_seq(series)
            }
        }

        impl<'s, D> crate::storage::SeqStoragePut<$t> for &'s mut $os
        where D: crate::storage::Disposition,
              &'s mut $is: crate::storage::SeqStoragePut<$t> {
            fn put_seq<I>(&mut self, series: I) -> Self::SeqID
            where I: IntoIterator<Item = $t> {
                (&mut &mut self.$f).put_seq(series)
            }

            fn try_put_seq<E, F, I>(
                &mut self,
                series: I,
            ) -> <F as Residual<Self::SeqID>>::TryType
            where I: IntoIterator<Item = E>,
                  E: Try<Output = $t, Residual = F>,
                  F: Residual<Self::SeqID> {
                (&mut &mut self.$f).try_put_seq(series)
            }
        }
    }
}

fwd_seq_storage!(Storage<D>, modules, Modules<D>, Module);
fwd_seq_storage!(Storage<D>, functions, Functions<D>, Function);
fwd_seq_storage!(Storage<D>, arguments, Arguments<D>, Argument);
fwd_seq_storage!(Storage<D>, types, Types<D>, Type);
fwd_seq_storage!(Storage<D>, stmts, Stmts<D>, Stmt);
fwd_seq_storage!(Storage<D>, exprs, Exprs<D>, Expr);
fwd_storage!(Storage<D>, integers, Integers<D>, Integer);
fwd_storage_ref!(Storage<D>, symbols, Symbols<D>, Symbol);

/// Storage for [`Module`]s.
pub struct Modules<D: Disposition> {
    inner: D::SeqStorage<Module>,
}

impl<D: Disposition> Default for Modules<D>
where D::SeqStorage<Module>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

fwd_seq_storage!(Modules<D>, inner, D::SeqStorage<Module>, Module);

impl Object for Module {
    type ID = ident::IDLen<Self>;
}

impl SeqObject for Module {
    type SeqID = ident::SeqIDLen<Self>;
}

/// Storage for [`Function`]s.
pub struct Functions<D: Disposition> {
    inner: D::SeqStorage<Function>,
}

impl<D: Disposition> Default for Functions<D>
where D::SeqStorage<Function>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

fwd_seq_storage!(Functions<D>, inner, D::SeqStorage<Function>, Function);

impl Object for Function {
    type ID = ident::IDLen<Self>;
}

impl SeqObject for Function {
    type SeqID = ident::SeqIDLen<Self>;
}

/// Storage for [`Argument`]s.
pub struct Arguments<D: Disposition> {
    inner: D::SeqStorage<Argument>,
}

impl<D: Disposition> Default for Arguments<D>
where D::SeqStorage<Argument>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

fwd_seq_storage!(Arguments<D>, inner, D::SeqStorage<Argument>, Argument);

impl Object for Argument {
    type ID = ident::IDLen<Self>;
}

impl SeqObject for Argument {
    type SeqID = ident::SeqIDLen<Self>;
}

/// Storage for [`Variable`]s.
pub struct Variables<D: Disposition> {
    inner: D::SeqStorage<Variable>,
}

impl<D: Disposition> Default for Variables<D>
where D::SeqStorage<Variable>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

fwd_seq_storage!(Variables<D>, inner, D::SeqStorage<Variable>, Variable);

impl Object for Variable {
    type ID = ident::IDLen<Self>;
}

impl SeqObject for Variable {
    type SeqID = ident::SeqIDLen<Self>;
}

/// Storage for [`Type`]s.
pub struct Types<D: Disposition> {
    inner: D::SeqStorage<Type>,
}

impl<D: Disposition> Default for Types<D>
where D::SeqStorage<Type>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

fwd_seq_storage!(Types<D>, inner, D::SeqStorage<Type>, Type);

impl Object for Type {
    type ID = ident::IDLen<Self>;
}

impl SeqObject for Type {
    type SeqID = ident::SeqIDLen<Self>;
}

/// Storage for [`Stmt`]s.
pub struct Stmts<D: Disposition> {
    inner: D::SeqStorage<Stmt>,
}

impl<D: Disposition> Default for Stmts<D>
where D::SeqStorage<Stmt>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

fwd_seq_storage!(Stmts<D>, inner, D::SeqStorage<Stmt>, Stmt);

impl Object for Stmt {
    type ID = ident::IDLen<Self>;
}

impl SeqObject for Stmt {
    type SeqID = ident::SeqIDLen<Self>;
}

/// Storage for [`Expr`]s.
pub struct Exprs<D: Disposition> {
    inner: D::SeqStorage<Expr>,
}

impl<D: Disposition> Default for Exprs<D>
where D::SeqStorage<Expr>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

fwd_seq_storage!(Exprs<D>, inner, D::SeqStorage<Expr>, Expr);

impl Object for Expr {
    type ID = ident::IDLen<Self>;
}

impl SeqObject for Expr {
    type SeqID = ident::SeqIDLen<Self>;
}
