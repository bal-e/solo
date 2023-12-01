use core::convert::{AsRef, AsMut};

use crate::soa;

use super::*;

macro_rules! def_storage {
    ($($field:ident : $type:ident { $storage:ident + $storage_mut:ident },)*) => {
        /// Immutable AST storage.
        #[derive(Clone, Debug)]
        pub struct Storage {
            $(pub $field : $storage),*
        }

        $(impl AsRef<$storage> for Storage {
            fn as_ref(&self) -> &$storage {
                &self.$field
            }
        })*

        /// Mutable AST storage.
        #[derive(Clone, Debug, Default)]
        pub struct StorageMut {
            $(pub $field : $storage_mut),*
        }

        $(
            impl AsRef<$storage_mut> for StorageMut {
                fn as_ref(&self) -> &$storage_mut {
                    &self.$field
                }
            }

            impl AsMut<$storage_mut> for StorageMut {
                fn as_mut(&mut self) -> &mut $storage_mut {
                    &mut self.$field
                }
            }
        )*
    };
}

def_storage!(
    modules: Module { Modules + ModulesMut },
    functions: Function { Functions + FunctionsMut },
    arguments: Argument { Arguments + ArgumentsMut },
    variables: Variable { Variables + VariablesMut },
    stmts: Stmt { Stmts + StmtsMut },
    exprs: Expr { Exprs + ExprsMut },
);

/// Immutable storage for [`Module`]s.
pub type Modules = soa::Storage<Module>;

/// Mutable storage for [`Module`]s.
pub type ModulesMut = soa::StorageMut<Module>;

/// Immutable storage for [`Function`]s.
pub type Functions = soa::Storage<Function>;

/// Mutable storage for [`Function`]s.
pub type FunctionsMut = soa::StorageMut<Function>;

/// Immutable storage for [`Argument`]s.
pub type Arguments = soa::Storage<Argument>;

/// Mutable storage for [`Argument`]s.
pub type ArgumentsMut = soa::StorageMut<Argument>;

/// Immutable storage for [`Variable`]s.
pub type Variables = soa::Storage<Variable>;

/// Mutable storage for [`Variable`]s.
pub type VariablesMut = soa::StorageMut<Variable>;

/// Immutable storage for [`Stmt`]s.
pub type Stmts = soa::Storage<Stmt>;

/// Mutable storage for [`Stmt`]s.
pub type StmtsMut = soa::StorageMut<Stmt>;

/// Immutable storage for [`Expr`]s.
pub type Exprs = soa::Storage<Expr>;

/// Mutable storage for [`Expr`]s.
pub type ExprsMut = soa::StorageMut<Expr>;
