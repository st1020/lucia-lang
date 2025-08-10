use lucia_lang::compiler::{
    ast::LitKind,
    value::{MetaMethod, MetaName},
};

use crate::{
    error::TypeError,
    typing::{FunctionType, TableType, Type},
};

pub(crate) struct MetaMethodType;

macro_rules! meta_operator_error {
    ($operator:expr, $arg1:expr) => {
        TypeError::MetaUnOperator {
            operator: $operator,
            operand: Box::new($arg1),
        }
    };
    ($operator:expr, $arg1:expr, $arg2:expr) => {
        TypeError::MetaBinOperator {
            operator: $operator,
            operand: (Box::new($arg1), Box::new($arg2)),
        }
    };
}

macro_rules! call_metamethod {
    ($ctx:ident, $meta_name:expr, $v1:ident $(,)? $($arg:ident),*) => {
        if let Some(metatable) = $v1.metatable() {
            let metamethod = metatable.get_str($meta_name);
            if !metamethod.is_null() {
                let function = $ctx.call(metamethod)?;
                function.check_args(&[$v1, $($arg),*])?;
                return Ok(function.returns.clone());
            }
        }
    };
}

impl MetaMethodType {
    pub fn arithmetic<S: AsRef<str> + Clone + Eq + Ord>(
        &self,
        meta_name: MetaName,
        lhs: Type<S>,
        rhs: Type<S>,
    ) -> Result<Type<S>, TypeError<S>> {
        call_metamethod!(self, meta_name, lhs, rhs);
        if lhs == Type::Any && rhs == Type::Any {
            Ok(Type::Any)
        } else if lhs.is_subtype_of(&Type::Int) && rhs.is_subtype_of(&Type::Int) {
            Ok(Type::Int)
        } else if lhs.is_subtype_of(&Type::Float) && rhs.is_subtype_of(&Type::Float) {
            Ok(Type::Float)
        } else {
            Err(meta_operator_error!(meta_name, lhs, rhs))
        }
    }
}

impl<S: AsRef<str> + Clone + Eq + Ord> MetaMethod<Type<S>> for MetaMethodType {
    type ResultCall = Result<Box<FunctionType<S>>, TypeError<S>>;
    type ResultIter = Result<Type<S>, TypeError<S>>;
    type Result1 = Result<Type<S>, TypeError<S>>;
    type Result2 = Self::Result1;
    type Result3 = Self::Result1;

    fn call(&self, value: Type<S>) -> Self::ResultCall {
        if value == Type::Any {
            return Ok(Box::new(FunctionType::ANY));
        }
        if let Type::Function(function) = value {
            return Ok(function);
        }
        let metatable = value
            .metatable()
            .ok_or_else(|| meta_operator_error!(MetaName::Call, value))?;
        match metatable.get_str(MetaName::Call) {
            Type::Function(f) => Ok(f),
            v @ Type::Table(_) => self.call(v),
            v => Err(meta_operator_error!(MetaName::Call, v)),
        }
    }

    fn iter(&self, value: Type<S>) -> Self::ResultIter {
        call_metamethod!(self, MetaName::Iter, value);
        match value {
            Type::Any => Ok(Type::Any),
            Type::Table(table) => {
                let mut key: Type<S> = table
                    .pairs
                    .iter()
                    .map(|(k, _)| Type::Literal(k.clone()))
                    .collect::<Vec<_>>()
                    .into();
                let mut value: Type<S> = table
                    .pairs
                    .iter()
                    .map(|(_, v)| v.clone())
                    .collect::<Vec<_>>()
                    .into();
                if let Some((k, v)) = &table.others {
                    key = key.union(k);
                    value = value.union(v);
                }
                Ok(TableType {
                    pairs: vec![(LitKind::Int(0), key), (LitKind::Int(1), value)],
                    others: None,
                    metatable: None,
                }
                .into())
            }
            Type::Function(function) => {
                function.check_args(&[])?;
                Ok(function.returns.clone())
            }
            _ => Err(meta_operator_error!(MetaName::Iter, value)),
        }
    }

    fn get_attr(&self, table: Type<S>, key: Type<S>) -> Self::Result2 {
        call_metamethod!(self, MetaName::GetAttr, table, key);
        match table {
            Type::Any => Ok(Type::Any),
            Type::Table(v) => Ok(v.get(key)),
            _ => Err(meta_operator_error!(MetaName::GetAttr, table, key)),
        }
    }

    fn get_item(&self, table: Type<S>, key: Type<S>) -> Self::Result2 {
        call_metamethod!(self, MetaName::GetItem, table, key);
        match table {
            Type::Any => Ok(Type::Any),
            Type::Table(v) => Ok(v.get(key)),
            _ => Err(meta_operator_error!(MetaName::GetItem, table, key)),
        }
    }

    fn set_attr(&self, table: Type<S>, key: Type<S>, value: Type<S>) -> Self::Result3 {
        call_metamethod!(self, MetaName::GetAttr, table, key, value);
        match table {
            Type::Any => Ok(Type::NULL),
            Type::Table(v) => {
                v.set(key, value)?;
                Ok(Type::NULL)
            }
            _ => Err(meta_operator_error!(MetaName::GetAttr, table, key)),
        }
    }

    fn set_item(&self, table: Type<S>, key: Type<S>, value: Type<S>) -> Self::Result3 {
        call_metamethod!(self, MetaName::SetItem, table, key, value);
        match table {
            Type::Any => Ok(Type::NULL),
            Type::Table(v) => {
                v.set(key, value)?;
                Ok(Type::NULL)
            }
            _ => Err(meta_operator_error!(MetaName::SetItem, table, key)),
        }
    }

    fn neg(&self, value: Type<S>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Neg, value);
        value
            .expect_is_subtype_of(&(Type::Int | Type::Float))
            .map(|_| value)
    }

    fn add(&self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        call_metamethod!(self, MetaName::Add, lhs, rhs);
        if lhs == Type::Any && rhs == Type::Any {
            Ok(Type::Any)
        } else if lhs.is_subtype_of(&Type::Int) && rhs.is_subtype_of(&Type::Int) {
            Ok(Type::Int)
        } else if lhs.is_subtype_of(&Type::Float) && rhs.is_subtype_of(&Type::Float) {
            Ok(Type::Float)
        } else if lhs.is_subtype_of(&Type::Str) && rhs.is_subtype_of(&Type::Str) {
            Ok(Type::Str)
        } else {
            Err(meta_operator_error!(MetaName::Add, lhs, rhs))
        }
    }

    fn sub(&self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        self.arithmetic(MetaName::Sub, lhs, rhs)
    }

    fn mul(&self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        self.arithmetic(MetaName::Mul, lhs, rhs)
    }

    fn div(&self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        self.arithmetic(MetaName::Div, lhs, rhs)
    }

    fn rem(&self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        self.arithmetic(MetaName::Rem, lhs, rhs)
    }

    fn eq(&self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        call_metamethod!(self, MetaName::Eq, lhs, rhs);
        Ok(Type::Bool)
    }

    fn ne(&self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        call_metamethod!(self, MetaName::Ne, lhs, rhs);
        Ok(Type::Bool)
    }

    fn gt(&self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        self.arithmetic(MetaName::Gt, lhs, rhs)
    }

    fn ge(&self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        self.arithmetic(MetaName::Ge, lhs, rhs)
    }

    fn lt(&self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        self.arithmetic(MetaName::Lt, lhs, rhs)
    }

    fn le(&self, lhs: Type<S>, rhs: Type<S>) -> Self::Result2 {
        self.arithmetic(MetaName::Le, lhs, rhs)
    }

    fn len(&self, value: Type<S>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Len, value);
        if !(value.is_subtype_of(&Type::Str) || matches!(value, Type::Table(_))) {
            return Err(meta_operator_error!(MetaName::Len, value));
        }
        Ok(Type::Int)
    }

    fn bool(&self, value: Type<S>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Bool, value);
        value.expect_is_subtype_of(&Type::Any)?;
        Ok(Type::Bool)
    }

    fn int(&self, value: Type<S>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Int, value);
        value.expect_is_subtype_of(
            &(Type::NULL | Type::Bool | Type::Int | Type::Float | Type::Str),
        )?;
        Ok(Type::Int)
    }

    fn float(&self, value: Type<S>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Float, value);
        value.expect_is_subtype_of(
            &(Type::NULL | Type::Bool | Type::Int | Type::Float | Type::Str),
        )?;
        Ok(Type::Float)
    }

    fn str(&self, value: Type<S>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Str, value);
        value.expect_is_subtype_of(&Type::Any)?;
        Ok(Type::Str)
    }

    fn repr(&self, value: Type<S>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Repr, value);
        value.expect_is_subtype_of(&Type::Any)?;
        Ok(Type::Str)
    }
}
