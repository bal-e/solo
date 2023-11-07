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
    ($type:ident, $storage:ident, $field:ident) => {
        impl<D: Disposition> storage::Storage<$type> for Storage<D> {
            type ID = <$storage<D> as storage::Storage<$type>>::ID;
            type Disposition = D;
        }

        impl<D: Disposition> StorageGet<$type> for Storage<D>
        where $storage<D>: StorageGet<$type> {
            unsafe fn get(&self, id: Self::ID) -> $type {
                self.$field.get(id)
            }
        }

        impl<D: Disposition> StoragePut<$type> for Storage<D>
        where $storage<D>: StoragePut<$type> {
            fn put(&mut self, object: $type) -> Self::ID {
                self.$field.put(object)
            }
        }
    }
}

macro_rules! fwd_storage_ref {
    ($type:ident, $storage:ident, $field:ident) => {
        impl<D: Disposition> storage::Storage<$type> for Storage<D> {
            type ID = <$storage<D> as storage::Storage<$type>>::ID;
            type Disposition = D;
        }

        impl<D: Disposition> StorageGetTmp<$type> for Storage<D>
        where $storage<D>: StorageGetTmp<$type> {
            unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
            where F: FnOnce(&$type) -> R {
                self.$field.get_tmp(id, func)
            }
        }

        impl<D: Disposition> StorageGetRef<$type> for Storage<D>
        where $storage<D>: StorageGetRef<$type> {
            unsafe fn get_ref(&self, id: Self::ID) -> &$type {
                self.$field.get_ref(id)
            }
        }

        impl<D: Disposition> StoragePutTmp<$type> for Storage<D>
        where $storage<D>: StoragePutTmp<$type> {
            fn put_tmp(&mut self, object: &$type) -> Self::ID {
                self.$field.put_tmp(object)
            }
        }
    }
}

macro_rules! fwd_seq_storage {
    ($type:ident, $storage:ident, $field:ident) => {
        fwd_storage!($type, $storage, $field);
        impl<D: Disposition> SeqStorage<$type> for Storage<D> {
            type SeqID = <$storage<D> as SeqStorage<$type>>::SeqID;
        }

        impl<D: Disposition> SeqStorageGet<$type> for Storage<D>
        where $storage<D>: SeqStorageGet<$type> {
            type Seq<'a>
                = <$storage<D> as SeqStorageGet<$type>>::Seq<'a>
                where Self: 'a, $type: 'a;

            unsafe fn get_seq(&self, id: Self::SeqID) -> Self::Seq<'_> {
                self.$field.get_seq(id)
            }
        }

        impl<D: Disposition> SeqStoragePut<$type> for Storage<D>
        where $storage<D>: SeqStoragePut<$type> {
            fn put_seq<I>(&mut self, series: I) -> Self::SeqID
            where I: IntoIterator<Item = $type> {
                self.$field.put_seq(series)
            }

            fn try_put_seq<E, F, I>(
                &mut self,
                series: I,
            ) -> <F as Residual<Self::SeqID>>::TryType
            where I: IntoIterator<Item = E>,
                  E: Try<Output = $type, Residual = F>,
                  F: Residual<Self::SeqID> {
                self.$field.try_put_seq(series)
            }
        }
    }
}

fwd_seq_storage!(Module, Modules, modules);
fwd_seq_storage!(Function, Functions, functions);
fwd_seq_storage!(Argument, Arguments, arguments);
fwd_seq_storage!(Type, Types, types);
fwd_seq_storage!(Stmt, Stmts, stmts);
fwd_seq_storage!(Expr, Exprs, exprs);
fwd_storage!(Integer, Integers, integers);
fwd_storage_ref!(Symbol, Symbols, symbols);

/// Storage for [`Module`]s.
pub struct Modules<D: Disposition> {
    inner: D::SeqStorage<Module>,
}

impl<D: Disposition> Clone for Modules<D>
where D::SeqStorage<Module>: Clone {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<D: Disposition> Default for Modules<D>
where D::SeqStorage<Module>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

impl<D: Disposition> storage::Storage<Module> for Modules<D> {
    type ID = ident::IDLen<Module>;
    type Disposition = D;
}

impl<D: Disposition> StorageGet<Module> for Modules<D>
where D::SeqStorage<Module>: StorageGet<Module> {
    unsafe fn get(&self, id: Self::ID) -> Module {
        self.inner.get(id)
    }
}

impl<D: Disposition> StoragePut<Module> for Modules<D>
where D::SeqStorage<Module>: StoragePut<Module> {
    fn put(&mut self, object: Module) -> Self::ID {
        self.inner.put(object)
    }
}

impl<D: Disposition> SeqStorage<Module> for Modules<D> {
    type SeqID = ident::SeqIDLen<Module>;
}

impl<D: Disposition> SeqStorageGet<Module> for Modules<D>
where D::SeqStorage<Module>: SeqStorageGet<Module> {
    type Seq<'a>
        = <D::SeqStorage<Module> as SeqStorageGet<Module>>::Seq<'a>
        where Self: 'a, Module: 'a;

    unsafe fn get_seq(&self, id: Self::SeqID) -> Self::Seq<'_> {
        self.inner.get_seq(id)
    }
}

impl<D: Disposition> SeqStoragePut<Module> for Modules<D>
where D::SeqStorage<Module>: SeqStoragePut<Module> {
    fn put_seq<I>(&mut self, series: I) -> Self::SeqID
    where I: IntoIterator<Item = Module> {
        self.inner.put_seq(series)
    }

    fn try_put_seq<E, F, I>(
        &mut self,
        series: I,
    ) -> <F as Residual<Self::SeqID>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = Module, Residual = F>,
          F: Residual<Self::SeqID> {
        self.inner.try_put_seq(series)
    }
}

impl Object for Module {
    type ID = ident::IDLen<Self>;
    type Storage<D: Disposition> = Modules<D>;
}

impl SeqObject for Module {
    type SeqID = ident::SeqIDLen<Self>;
    type SeqStorage<D: Disposition> = Modules<D>;
}

/// Storage for [`Function`]s.
pub struct Functions<D: Disposition> {
    inner: D::SeqStorage<Function>,
}

impl<D: Disposition> Clone for Functions<D>
where D::SeqStorage<Function>: Clone {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<D: Disposition> Default for Functions<D>
where D::SeqStorage<Function>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

impl<D: Disposition> storage::Storage<Function> for Functions<D> {
    type ID = ident::IDLen<Function>;
    type Disposition = D;
}

impl<D: Disposition> StorageGet<Function> for Functions<D>
where D::SeqStorage<Function>: StorageGet<Function> {
    unsafe fn get(&self, id: Self::ID) -> Function {
        self.inner.get(id)
    }
}

impl<D: Disposition> StoragePut<Function> for Functions<D>
where D::SeqStorage<Function>: StoragePut<Function> {
    fn put(&mut self, object: Function) -> Self::ID {
        self.inner.put(object)
    }
}

impl<D: Disposition> SeqStorage<Function> for Functions<D> {
    type SeqID = ident::SeqIDLen<Function>;
}

impl<D: Disposition> SeqStorageGet<Function> for Functions<D>
where D::SeqStorage<Function>: SeqStorageGet<Function> {
    type Seq<'a>
        = <D::SeqStorage<Function> as SeqStorageGet<Function>>::Seq<'a>
        where Self: 'a, Function: 'a;

    unsafe fn get_seq(&self, id: Self::SeqID) -> Self::Seq<'_> {
        self.inner.get_seq(id)
    }
}

impl<D: Disposition> SeqStoragePut<Function> for Functions<D>
where D::SeqStorage<Function>: SeqStoragePut<Function> {
    fn put_seq<I>(&mut self, series: I) -> Self::SeqID
    where I: IntoIterator<Item = Function> {
        self.inner.put_seq(series)
    }

    fn try_put_seq<E, F, I>(
        &mut self,
        series: I,
    ) -> <F as Residual<Self::SeqID>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = Function, Residual = F>,
          F: Residual<Self::SeqID> {
        self.inner.try_put_seq(series)
    }
}

impl Object for Function {
    type ID = ident::IDLen<Self>;
    type Storage<D: Disposition> = Functions<D>;
}

impl SeqObject for Function {
    type SeqID = ident::SeqIDLen<Self>;
    type SeqStorage<D: Disposition> = Functions<D>;
}

/// Storage for [`Argument`]s.
pub struct Arguments<D: Disposition> {
    inner: D::SeqStorage<Argument>,
}

impl<D: Disposition> Clone for Arguments<D>
where D::SeqStorage<Argument>: Clone {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<D: Disposition> Default for Arguments<D>
where D::SeqStorage<Argument>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

impl<D: Disposition> storage::Storage<Argument> for Arguments<D> {
    type ID = ident::IDLen<Argument>;
    type Disposition = D;
}

impl<D: Disposition> StorageGet<Argument> for Arguments<D>
where D::SeqStorage<Argument>: StorageGet<Argument> {
    unsafe fn get(&self, id: Self::ID) -> Argument {
        self.inner.get(id)
    }
}

impl<D: Disposition> StoragePut<Argument> for Arguments<D>
where D::SeqStorage<Argument>: StoragePut<Argument> {
    fn put(&mut self, object: Argument) -> Self::ID {
        self.inner.put(object)
    }
}

impl<D: Disposition> SeqStorage<Argument> for Arguments<D> {
    type SeqID = ident::SeqIDLen<Argument>;
}

impl<D: Disposition> SeqStorageGet<Argument> for Arguments<D>
where D::SeqStorage<Argument>: SeqStorageGet<Argument> {
    type Seq<'a>
        = <D::SeqStorage<Argument> as SeqStorageGet<Argument>>::Seq<'a>
        where Self: 'a, Argument: 'a;

    unsafe fn get_seq(&self, id: Self::SeqID) -> Self::Seq<'_> {
        self.inner.get_seq(id)
    }
}

impl<D: Disposition> SeqStoragePut<Argument> for Arguments<D>
where D::SeqStorage<Argument>: SeqStoragePut<Argument> {
    fn put_seq<I>(&mut self, series: I) -> Self::SeqID
    where I: IntoIterator<Item = Argument> {
        self.inner.put_seq(series)
    }

    fn try_put_seq<E, F, I>(
        &mut self,
        series: I,
    ) -> <F as Residual<Self::SeqID>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = Argument, Residual = F>,
          F: Residual<Self::SeqID> {
        self.inner.try_put_seq(series)
    }
}

impl Object for Argument {
    type ID = ident::IDLen<Self>;
    type Storage<D: Disposition> = Arguments<D>;
}

impl SeqObject for Argument {
    type SeqID = ident::SeqIDLen<Self>;
    type SeqStorage<D: Disposition> = Arguments<D>;
}

/// Storage for [`Variable`]s.
pub struct Variables<D: Disposition> {
    inner: D::SeqStorage<Variable>,
}

impl<D: Disposition> Clone for Variables<D>
where D::SeqStorage<Variable>: Clone {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<D: Disposition> Default for Variables<D>
where D::SeqStorage<Variable>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

impl<D: Disposition> storage::Storage<Variable> for Variables<D> {
    type ID = ident::IDLen<Variable>;
    type Disposition = D;
}

impl<D: Disposition> StorageGet<Variable> for Variables<D>
where D::SeqStorage<Variable>: StorageGet<Variable> {
    unsafe fn get(&self, id: Self::ID) -> Variable {
        self.inner.get(id)
    }
}

impl<D: Disposition> StoragePut<Variable> for Variables<D>
where D::SeqStorage<Variable>: StoragePut<Variable> {
    fn put(&mut self, object: Variable) -> Self::ID {
        self.inner.put(object)
    }
}

impl<D: Disposition> SeqStorage<Variable> for Variables<D> {
    type SeqID = ident::SeqIDLen<Variable>;
}

impl<D: Disposition> SeqStorageGet<Variable> for Variables<D>
where D::SeqStorage<Variable>: SeqStorageGet<Variable> {
    type Seq<'a>
        = <D::SeqStorage<Variable> as SeqStorageGet<Variable>>::Seq<'a>
        where Self: 'a, Variable: 'a;

    unsafe fn get_seq(&self, id: Self::SeqID) -> Self::Seq<'_> {
        self.inner.get_seq(id)
    }
}

impl<D: Disposition> SeqStoragePut<Variable> for Variables<D>
where D::SeqStorage<Variable>: SeqStoragePut<Variable> {
    fn put_seq<I>(&mut self, series: I) -> Self::SeqID
    where I: IntoIterator<Item = Variable> {
        self.inner.put_seq(series)
    }

    fn try_put_seq<E, F, I>(
        &mut self,
        series: I,
    ) -> <F as Residual<Self::SeqID>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = Variable, Residual = F>,
          F: Residual<Self::SeqID> {
        self.inner.try_put_seq(series)
    }
}

impl Object for Variable {
    type ID = ident::IDLen<Self>;
    type Storage<D: Disposition> = Variables<D>;
}

impl SeqObject for Variable {
    type SeqID = ident::SeqIDLen<Self>;
    type SeqStorage<D: Disposition> = Variables<D>;
}

/// Storage for [`Type`]s.
pub struct Types<D: Disposition> {
    inner: D::SeqStorage<Type>,
}

impl<D: Disposition> Clone for Types<D>
where D::SeqStorage<Type>: Clone {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<D: Disposition> Default for Types<D>
where D::SeqStorage<Type>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

impl<D: Disposition> storage::Storage<Type> for Types<D> {
    type ID = ident::IDLen<Type>;
    type Disposition = D;
}

impl<D: Disposition> StorageGet<Type> for Types<D>
where D::SeqStorage<Type>: StorageGet<Type> {
    unsafe fn get(&self, id: Self::ID) -> Type {
        self.inner.get(id)
    }
}

impl<D: Disposition> StoragePut<Type> for Types<D>
where D::SeqStorage<Type>: StoragePut<Type> {
    fn put(&mut self, object: Type) -> Self::ID {
        self.inner.put(object)
    }
}

impl<D: Disposition> SeqStorage<Type> for Types<D> {
    type SeqID = ident::SeqIDLen<Type>;
}

impl<D: Disposition> SeqStorageGet<Type> for Types<D>
where D::SeqStorage<Type>: SeqStorageGet<Type> {
    type Seq<'a>
        = <D::SeqStorage<Type> as SeqStorageGet<Type>>::Seq<'a>
        where Self: 'a, Type: 'a;

    unsafe fn get_seq(&self, id: Self::SeqID) -> Self::Seq<'_> {
        self.inner.get_seq(id)
    }
}

impl<D: Disposition> SeqStoragePut<Type> for Types<D>
where D::SeqStorage<Type>: SeqStoragePut<Type> {
    fn put_seq<I>(&mut self, series: I) -> Self::SeqID
    where I: IntoIterator<Item = Type> {
        self.inner.put_seq(series)
    }

    fn try_put_seq<E, F, I>(
        &mut self,
        series: I,
    ) -> <F as Residual<Self::SeqID>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = Type, Residual = F>,
          F: Residual<Self::SeqID> {
        self.inner.try_put_seq(series)
    }
}

impl Object for Type {
    type ID = ident::IDLen<Self>;
    type Storage<D: Disposition> = Types<D>;
}

impl SeqObject for Type {
    type SeqID = ident::SeqIDLen<Self>;
    type SeqStorage<D: Disposition> = Types<D>;
}

/// Storage for [`Stmt`]s.
pub struct Stmts<D: Disposition> {
    inner: D::SeqStorage<Stmt>,
}

impl<D: Disposition> Clone for Stmts<D>
where D::SeqStorage<Stmt>: Clone {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<D: Disposition> Default for Stmts<D>
where D::SeqStorage<Stmt>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

impl<D: Disposition> storage::Storage<Stmt> for Stmts<D> {
    type ID = ident::IDLen<Stmt>;
    type Disposition = D;
}

impl<D: Disposition> StorageGet<Stmt> for Stmts<D>
where D::SeqStorage<Stmt>: StorageGet<Stmt> {
    unsafe fn get(&self, id: Self::ID) -> Stmt {
        self.inner.get(id)
    }
}

impl<D: Disposition> StoragePut<Stmt> for Stmts<D>
where D::SeqStorage<Stmt>: StoragePut<Stmt> {
    fn put(&mut self, object: Stmt) -> Self::ID {
        self.inner.put(object)
    }
}

impl<D: Disposition> SeqStorage<Stmt> for Stmts<D> {
    type SeqID = ident::SeqIDLen<Stmt>;
}

impl<D: Disposition> SeqStorageGet<Stmt> for Stmts<D>
where D::SeqStorage<Stmt>: SeqStorageGet<Stmt> {
    type Seq<'a>
        = <D::SeqStorage<Stmt> as SeqStorageGet<Stmt>>::Seq<'a>
        where Self: 'a, Stmt: 'a;

    unsafe fn get_seq(&self, id: Self::SeqID) -> Self::Seq<'_> {
        self.inner.get_seq(id)
    }
}

impl<D: Disposition> SeqStoragePut<Stmt> for Stmts<D>
where D::SeqStorage<Stmt>: SeqStoragePut<Stmt> {
    fn put_seq<I>(&mut self, series: I) -> Self::SeqID
    where I: IntoIterator<Item = Stmt> {
        self.inner.put_seq(series)
    }

    fn try_put_seq<E, F, I>(
        &mut self,
        series: I,
    ) -> <F as Residual<Self::SeqID>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = Stmt, Residual = F>,
          F: Residual<Self::SeqID> {
        self.inner.try_put_seq(series)
    }
}

impl Object for Stmt {
    type ID = ident::IDLen<Self>;
    type Storage<D: Disposition> = Stmts<D>;
}

impl SeqObject for Stmt {
    type SeqID = ident::SeqIDLen<Self>;
    type SeqStorage<D: Disposition> = Stmts<D>;
}

/// Storage for [`Expr`]s.
pub struct Exprs<D: Disposition> {
    inner: D::SeqStorage<Expr>,
}

impl<D: Disposition> Clone for Exprs<D>
where D::SeqStorage<Expr>: Clone {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<D: Disposition> Default for Exprs<D>
where D::SeqStorage<Expr>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

impl<D: Disposition> storage::Storage<Expr> for Exprs<D> {
    type ID = ident::IDLen<Expr>;
    type Disposition = D;
}

impl<D: Disposition> StorageGet<Expr> for Exprs<D>
where D::SeqStorage<Expr>: StorageGet<Expr> {
    unsafe fn get(&self, id: Self::ID) -> Expr {
        self.inner.get(id)
    }
}

impl<D: Disposition> StoragePut<Expr> for Exprs<D>
where D::SeqStorage<Expr>: StoragePut<Expr> {
    fn put(&mut self, object: Expr) -> Self::ID {
        self.inner.put(object)
    }
}

impl<D: Disposition> SeqStorage<Expr> for Exprs<D> {
    type SeqID = ident::SeqIDLen<Expr>;
}

impl<D: Disposition> SeqStorageGet<Expr> for Exprs<D>
where D::SeqStorage<Expr>: SeqStorageGet<Expr> {
    type Seq<'a>
        = <D::SeqStorage<Expr> as SeqStorageGet<Expr>>::Seq<'a>
        where Self: 'a, Expr: 'a;

    unsafe fn get_seq(&self, id: Self::SeqID) -> Self::Seq<'_> {
        self.inner.get_seq(id)
    }
}

impl<D: Disposition> SeqStoragePut<Expr> for Exprs<D>
where D::SeqStorage<Expr>: SeqStoragePut<Expr> {
    fn put_seq<I>(&mut self, series: I) -> Self::SeqID
    where I: IntoIterator<Item = Expr> {
        self.inner.put_seq(series)
    }

    fn try_put_seq<E, F, I>(
        &mut self,
        series: I,
    ) -> <F as Residual<Self::SeqID>>::TryType
    where I: IntoIterator<Item = E>,
          E: Try<Output = Expr, Residual = F>,
          F: Residual<Self::SeqID> {
        self.inner.try_put_seq(series)
    }
}

impl Object for Expr {
    type ID = ident::IDLen<Self>;
    type Storage<D: Disposition> = Exprs<D>;
}

impl SeqObject for Expr {
    type SeqID = ident::SeqIDLen<Self>;
    type SeqStorage<D: Disposition> = Exprs<D>;
}
