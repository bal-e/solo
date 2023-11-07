//! Storage for the AST.

use core::ops::{Residual, Try};

use crate::storage::{self, *};
use crate::storage::ints::{Integer, Integers};
use crate::storage::syms::{Symbol, Symbols};

use super::*;

/// AST storage.
pub struct Storage<'s, D: Disposition<'s>> {
    modules: Modules<'s, D>,
    functions: Functions<'s, D>,
    arguments: Arguments<'s, D>,
    types: Types<'s, D>,
    stmts: Stmts<'s, D>,
    exprs: Exprs<'s, D>,
    integers: Integers<'s, D>,
    symbols: Symbols<'s, D>,
}

impl<'s, D: Disposition<'s>> Default for Storage<'s, D>
where Modules<'s, D>: Default,
      Functions<'s, D>: Default,
      Arguments<'s, D>: Default,
      Types<'s, D>: Default,
      Stmts<'s, D>: Default,
      Exprs<'s, D>: Default,
      Integers<'s, D>: Default,
      Symbols<'s, D>: Default {

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
        impl<'s, D: Disposition<'s>> storage::Storage<'s, $type> for Storage<'s, D> {
            type ID = <$storage<'s, D> as storage::Storage<'s, $type>>::ID;
            type Disposition = D;
        }

        impl<'s, D: Disposition<'s>> StorageGet<'s, $type> for Storage<'s, D>
        where $storage<'s, D>: StorageGet<'s, $type> {
            unsafe fn get(&self, id: Self::ID) -> $type {
                self.$field.get(id)
            }
        }

        impl<'s, D: Disposition<'s>> StoragePut<'s, $type> for Storage<'s, D>
        where $storage<'s, D>: StoragePut<'s, $type> {
            fn put(&mut self, object: $type) -> Self::ID {
                self.$field.put(object)
            }
        }
    }
}

macro_rules! fwd_storage_ref {
    ($type:ident, $storage:ident, $field:ident) => {
        impl<'s, D: Disposition<'s>> storage::Storage<'s, $type> for Storage<'s, D> {
            type ID = <$storage<'s, D> as storage::Storage<'s, $type>>::ID;
            type Disposition = D;
        }

        impl<'s, D: Disposition<'s>> StorageGetTmp<'s, $type> for Storage<'s, D>
        where $storage<'s, D>: StorageGetTmp<'s, $type> {
            unsafe fn get_tmp<R, F>(&self, id: Self::ID, func: F) -> R
            where F: FnOnce(&$type) -> R {
                self.$field.get_tmp(id, func)
            }
        }

        impl<'s, D: Disposition<'s>> StorageGetRef<'s, $type> for Storage<'s, D>
        where $storage<'s, D>: StorageGetRef<'s, $type> {
            unsafe fn get_ref(&self, id: Self::ID) -> &$type {
                self.$field.get_ref(id)
            }
        }

        impl<'s, D: Disposition<'s>> StoragePutTmp<'s, $type> for Storage<'s, D>
        where $storage<'s, D>: StoragePutTmp<'s, $type> {
            fn put_tmp(&mut self, object: &$type) -> Self::ID {
                self.$field.put_tmp(object)
            }
        }
    }
}

macro_rules! fwd_seq_storage {
    ($type:ident, $storage:ident, $field:ident) => {
        fwd_storage!($type, $storage, $field);

        impl<'s, D: Disposition<'s>> SeqStorage<'s, $type> for Storage<'s, D> {
            type SeqID = <$storage<'s, D> as SeqStorage<'s, $type>>::SeqID;
        }

        impl<'s, D: Disposition<'s>> SeqStorageGet<'s, $type> for Storage<'s, D>
        where $storage<'s, D>: SeqStorageGet<'s, $type> {
            type Seq<'t>
                = <$storage<'s, D> as SeqStorageGet<'s, $type>>::Seq<'t>
                where 's: 't;

            unsafe fn get_seq<'t>(&'t self, id: Self::SeqID) -> Self::Seq<'t>
            where 's: 't {
                self.$field.get_seq(id)
            }
        }

        impl<'s, D: Disposition<'s>> SeqStoragePut<'s, $type> for Storage<'s, D>
        where $storage<'s, D>: SeqStoragePut<'s, $type> {
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
pub struct Modules<'s, D: Disposition<'s>> {
    inner: D::SeqStorage<Module>,
}

impl<'s, D: Disposition<'s>> Clone for Modules<'s, D>
where D::SeqStorage<Module>: Clone {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<'s, D: Disposition<'s>> Default for Modules<'s, D>
where D::SeqStorage<Module>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

impl<'s, D: Disposition<'s>> storage::Storage<'s, Module> for Modules<'s, D> {
    type ID = ident::IDLen<Module>;
    type Disposition = D;
}

impl<'s, D: Disposition<'s>> StorageGet<'s, Module> for Modules<'s, D>
where D::SeqStorage<Module>: StorageGet<'s, Module> {
    unsafe fn get(&self, id: Self::ID) -> Module {
        self.inner.get(id)
    }
}

impl<'s, D: Disposition<'s>> StoragePut<'s, Module> for Modules<'s, D>
where D::SeqStorage<Module>: StoragePut<'s, Module> {
    fn put(&mut self, object: Module) -> Self::ID {
        self.inner.put(object)
    }
}

impl<'s, D: Disposition<'s>> SeqStorage<'s, Module> for Modules<'s, D> {
    type SeqID = ident::SeqIDLen<Module>;
}

impl<'s, D: Disposition<'s>> SeqStorageGet<'s, Module> for Modules<'s, D>
where D::SeqStorage<Module>: SeqStorageGet<'s, Module> {
    type Seq<'t>
        = <D::SeqStorage<Module> as SeqStorageGet<'s, Module>>::Seq<'t>
        where 's: 't;

    unsafe fn get_seq<'t>(&'t self, id: Self::SeqID) -> Self::Seq<'t>
    where 's: 't {
        self.inner.get_seq(id)
    }
}

impl<'s, D: Disposition<'s>> SeqStoragePut<'s, Module> for Modules<'s, D>
where D::SeqStorage<Module>: SeqStoragePut<'s, Module> {
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
}

impl SeqObject for Module {
    type SeqID = ident::SeqIDLen<Self>;
}

/// Storage for [`Function`]s.
pub struct Functions<'s, D: Disposition<'s>> {
    inner: D::SeqStorage<Function>,
}

impl<'s, D: Disposition<'s>> Clone for Functions<'s, D>
where D::SeqStorage<Function>: Clone {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<'s, D: Disposition<'s>> Default for Functions<'s, D>
where D::SeqStorage<Function>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

impl<'s, D: Disposition<'s>> storage::Storage<'s, Function> for Functions<'s, D> {
    type ID = ident::IDLen<Function>;
    type Disposition = D;
}

impl<'s, D: Disposition<'s>> StorageGet<'s, Function> for Functions<'s, D>
where D::SeqStorage<Function>: StorageGet<'s, Function> {
    unsafe fn get(&self, id: Self::ID) -> Function {
        self.inner.get(id)
    }
}

impl<'s, D: Disposition<'s>> StoragePut<'s, Function> for Functions<'s, D>
where D::SeqStorage<Function>: StoragePut<'s, Function> {
    fn put(&mut self, object: Function) -> Self::ID {
        self.inner.put(object)
    }
}

impl<'s, D: Disposition<'s>> SeqStorage<'s, Function> for Functions<'s, D> {
    type SeqID = ident::SeqIDLen<Function>;
}

impl<'s, D: Disposition<'s>> SeqStorageGet<'s, Function> for Functions<'s, D>
where D::SeqStorage<Function>: SeqStorageGet<'s, Function> {
    type Seq<'t>
        = <D::SeqStorage<Function> as SeqStorageGet<'s, Function>>::Seq<'t>
        where 's: 't;

    unsafe fn get_seq<'t>(&'t self, id: Self::SeqID) -> Self::Seq<'t>
    where 's: 't {
        self.inner.get_seq(id)
    }
}

impl<'s, D: Disposition<'s>> SeqStoragePut<'s, Function> for Functions<'s, D>
where D::SeqStorage<Function>: SeqStoragePut<'s, Function> {
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
}

impl SeqObject for Function {
    type SeqID = ident::SeqIDLen<Self>;
}

/// Storage for [`Argument`]s.
pub struct Arguments<'s, D: Disposition<'s>> {
    inner: D::SeqStorage<Argument>,
}

impl<'s, D: Disposition<'s>> Clone for Arguments<'s, D>
where D::SeqStorage<Argument>: Clone {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<'s, D: Disposition<'s>> Default for Arguments<'s, D>
where D::SeqStorage<Argument>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

impl<'s, D: Disposition<'s>> storage::Storage<'s, Argument> for Arguments<'s, D> {
    type ID = ident::IDLen<Argument>;
    type Disposition = D;
}

impl<'s, D: Disposition<'s>> StorageGet<'s, Argument> for Arguments<'s, D>
where D::SeqStorage<Argument>: StorageGet<'s, Argument> {
    unsafe fn get(&self, id: Self::ID) -> Argument {
        self.inner.get(id)
    }
}

impl<'s, D: Disposition<'s>> StoragePut<'s, Argument> for Arguments<'s, D>
where D::SeqStorage<Argument>: StoragePut<'s, Argument> {
    fn put(&mut self, object: Argument) -> Self::ID {
        self.inner.put(object)
    }
}

impl<'s, D: Disposition<'s>> SeqStorage<'s, Argument> for Arguments<'s, D> {
    type SeqID = ident::SeqIDLen<Argument>;
}

impl<'s, D: Disposition<'s>> SeqStorageGet<'s, Argument> for Arguments<'s, D>
where D::SeqStorage<Argument>: SeqStorageGet<'s, Argument> {
    type Seq<'t>
        = <D::SeqStorage<Argument> as SeqStorageGet<'s, Argument>>::Seq<'t>
        where 's: 't;

    unsafe fn get_seq<'t>(&'t self, id: Self::SeqID) -> Self::Seq<'t>
    where 's: 't {
        self.inner.get_seq(id)
    }
}

impl<'s, D: Disposition<'s>> SeqStoragePut<'s, Argument> for Arguments<'s, D>
where D::SeqStorage<Argument>: SeqStoragePut<'s, Argument> {
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
}

impl SeqObject for Argument {
    type SeqID = ident::SeqIDLen<Self>;
}

/// Storage for [`Variable`]s.
pub struct Variables<'s, D: Disposition<'s>> {
    inner: D::SeqStorage<Variable>,
}

impl<'s, D: Disposition<'s>> Clone for Variables<'s, D>
where D::SeqStorage<Variable>: Clone {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<'s, D: Disposition<'s>> Default for Variables<'s, D>
where D::SeqStorage<Variable>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

impl<'s, D: Disposition<'s>> storage::Storage<'s, Variable> for Variables<'s, D> {
    type ID = ident::IDLen<Variable>;
    type Disposition = D;
}

impl<'s, D: Disposition<'s>> StorageGet<'s, Variable> for Variables<'s, D>
where D::SeqStorage<Variable>: StorageGet<'s, Variable> {
    unsafe fn get(&self, id: Self::ID) -> Variable {
        self.inner.get(id)
    }
}

impl<'s, D: Disposition<'s>> StoragePut<'s, Variable> for Variables<'s, D>
where D::SeqStorage<Variable>: StoragePut<'s, Variable> {
    fn put(&mut self, object: Variable) -> Self::ID {
        self.inner.put(object)
    }
}

impl<'s, D: Disposition<'s>> SeqStorage<'s, Variable> for Variables<'s, D> {
    type SeqID = ident::SeqIDLen<Variable>;
}

impl<'s, D: Disposition<'s>> SeqStorageGet<'s, Variable> for Variables<'s, D>
where D::SeqStorage<Variable>: SeqStorageGet<'s, Variable> {
    type Seq<'t>
        = <D::SeqStorage<Variable> as SeqStorageGet<'s, Variable>>::Seq<'t>
        where 's: 't;

    unsafe fn get_seq<'t>(&'t self, id: Self::SeqID) -> Self::Seq<'t>
    where 's: 't {
        self.inner.get_seq(id)
    }
}

impl<'s, D: Disposition<'s>> SeqStoragePut<'s, Variable> for Variables<'s, D>
where D::SeqStorage<Variable>: SeqStoragePut<'s, Variable> {
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
}

impl SeqObject for Variable {
    type SeqID = ident::SeqIDLen<Self>;
}

/// Storage for [`Type`]s.
pub struct Types<'s, D: Disposition<'s>> {
    inner: D::SeqStorage<Type>,
}

impl<'s, D: Disposition<'s>> Clone for Types<'s, D>
where D::SeqStorage<Type>: Clone {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<'s, D: Disposition<'s>> Default for Types<'s, D>
where D::SeqStorage<Type>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

impl<'s, D: Disposition<'s>> storage::Storage<'s, Type> for Types<'s, D> {
    type ID = ident::IDLen<Type>;
    type Disposition = D;
}

impl<'s, D: Disposition<'s>> StorageGet<'s, Type> for Types<'s, D>
where D::SeqStorage<Type>: StorageGet<'s, Type> {
    unsafe fn get(&self, id: Self::ID) -> Type {
        self.inner.get(id)
    }
}

impl<'s, D: Disposition<'s>> StoragePut<'s, Type> for Types<'s, D>
where D::SeqStorage<Type>: StoragePut<'s, Type> {
    fn put(&mut self, object: Type) -> Self::ID {
        self.inner.put(object)
    }
}

impl<'s, D: Disposition<'s>> SeqStorage<'s, Type> for Types<'s, D> {
    type SeqID = ident::SeqIDLen<Type>;
}

impl<'s, D: Disposition<'s>> SeqStorageGet<'s, Type> for Types<'s, D>
where D::SeqStorage<Type>: SeqStorageGet<'s, Type> {
    type Seq<'t>
        = <D::SeqStorage<Type> as SeqStorageGet<'s, Type>>::Seq<'t>
        where 's: 't;

    unsafe fn get_seq<'t>(&'t self, id: Self::SeqID) -> Self::Seq<'t>
    where 's: 't {
        self.inner.get_seq(id)
    }
}

impl<'s, D: Disposition<'s>> SeqStoragePut<'s, Type> for Types<'s, D>
where D::SeqStorage<Type>: SeqStoragePut<'s, Type> {
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
}

impl SeqObject for Type {
    type SeqID = ident::SeqIDLen<Self>;
}

/// Storage for [`Stmt`]s.
pub struct Stmts<'s, D: Disposition<'s>> {
    inner: D::SeqStorage<Stmt>,
}

impl<'s, D: Disposition<'s>> Clone for Stmts<'s, D>
where D::SeqStorage<Stmt>: Clone {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<'s, D: Disposition<'s>> Default for Stmts<'s, D>
where D::SeqStorage<Stmt>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

impl<'s, D: Disposition<'s>> storage::Storage<'s, Stmt> for Stmts<'s, D> {
    type ID = ident::IDLen<Stmt>;
    type Disposition = D;
}

impl<'s, D: Disposition<'s>> StorageGet<'s, Stmt> for Stmts<'s, D>
where D::SeqStorage<Stmt>: StorageGet<'s, Stmt> {
    unsafe fn get(&self, id: Self::ID) -> Stmt {
        self.inner.get(id)
    }
}

impl<'s, D: Disposition<'s>> StoragePut<'s, Stmt> for Stmts<'s, D>
where D::SeqStorage<Stmt>: StoragePut<'s, Stmt> {
    fn put(&mut self, object: Stmt) -> Self::ID {
        self.inner.put(object)
    }
}

impl<'s, D: Disposition<'s>> SeqStorage<'s, Stmt> for Stmts<'s, D> {
    type SeqID = ident::SeqIDLen<Stmt>;
}

impl<'s, D: Disposition<'s>> SeqStorageGet<'s, Stmt> for Stmts<'s, D>
where D::SeqStorage<Stmt>: SeqStorageGet<'s, Stmt> {
    type Seq<'t>
        = <D::SeqStorage<Stmt> as SeqStorageGet<'s, Stmt>>::Seq<'t>
        where 's: 't;

    unsafe fn get_seq<'t>(&'t self, id: Self::SeqID) -> Self::Seq<'t>
    where 's: 't {
        self.inner.get_seq(id)
    }
}

impl<'s, D: Disposition<'s>> SeqStoragePut<'s, Stmt> for Stmts<'s, D>
where D::SeqStorage<Stmt>: SeqStoragePut<'s, Stmt> {
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
}

impl SeqObject for Stmt {
    type SeqID = ident::SeqIDLen<Self>;
}

/// Storage for [`Expr`]s.
pub struct Exprs<'s, D: Disposition<'s>> {
    inner: D::SeqStorage<Expr>,
}

impl<'s, D: Disposition<'s>> Clone for Exprs<'s, D>
where D::SeqStorage<Expr>: Clone {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<'s, D: Disposition<'s>> Default for Exprs<'s, D>
where D::SeqStorage<Expr>: Default {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

impl<'s, D: Disposition<'s>> storage::Storage<'s, Expr> for Exprs<'s, D> {
    type ID = ident::IDLen<Expr>;
    type Disposition = D;
}

impl<'s, D: Disposition<'s>> StorageGet<'s, Expr> for Exprs<'s, D>
where D::SeqStorage<Expr>: StorageGet<'s, Expr> {
    unsafe fn get(&self, id: Self::ID) -> Expr {
        self.inner.get(id)
    }
}

impl<'s, D: Disposition<'s>> StoragePut<'s, Expr> for Exprs<'s, D>
where D::SeqStorage<Expr>: StoragePut<'s, Expr> {
    fn put(&mut self, object: Expr) -> Self::ID {
        self.inner.put(object)
    }
}

impl<'s, D: Disposition<'s>> SeqStorage<'s, Expr> for Exprs<'s, D> {
    type SeqID = ident::SeqIDLen<Expr>;
}

impl<'s, D: Disposition<'s>> SeqStorageGet<'s, Expr> for Exprs<'s, D>
where D::SeqStorage<Expr>: SeqStorageGet<'s, Expr> {
    type Seq<'t>
        = <D::SeqStorage<Expr> as SeqStorageGet<'s, Expr>>::Seq<'t>
        where 's: 't;

    unsafe fn get_seq<'t>(&'t self, id: Self::SeqID) -> Self::Seq<'t>
    where 's: 't {
        self.inner.get_seq(id)
    }
}

impl<'s, D: Disposition<'s>> SeqStoragePut<'s, Expr> for Exprs<'s, D>
where D::SeqStorage<Expr>: SeqStoragePut<'s, Expr> {
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
}

impl SeqObject for Expr {
    type SeqID = ident::SeqIDLen<Self>;
}
