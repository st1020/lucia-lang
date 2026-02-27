use std::{cmp::Ordering, rc::Rc};

use compact_str::{CompactString, ToCompactString};
use derive_more::Display;
use ordermap::{OrderMap, OrderSet};

use crate::{
    Context,
    compiler::value::{MetaMethod, MetaName},
    objects::{
        ArgumentRange, BuiltinEffect, Callback, CallbackFn, CallbackReturn, Effect, Function,
        MetaResult, Value, call_meta_iter, call_metamethod, impl_metamethod,
    },
};

pub type RcTable = Rc<Table>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default, Display)]
#[display("<table {self:p}>")]
pub struct Table {
    pub entries: TableEntries,
    pub metatable: Option<RcTable>,
}

impl Table {
    pub fn new() -> Self {
        Table::default()
    }

    pub fn get<K: Into<Value>>(&self, key: K) -> Value {
        let key = key.into();
        if let Value::Int(key) = key
            && let Ok(key) = usize::try_from(key)
            && key < self.entries.array.len()
        {
            return self.entries.array[key].clone();
        }
        self.entries.map.get(&key).cloned().unwrap_or(Value::Null)
    }

    pub fn get_index(&self, index: usize) -> Option<(Value, Value)> {
        if index < self.entries.array.len() {
            self.entries
                .array
                .get(index)
                .map(|v| (index.into(), v.clone()))
        } else {
            self.entries
                .map
                .get_index(index - self.entries.array.len())
                .map(|(k, v)| (k.clone(), v.clone()))
        }
    }

    pub fn set<K: Into<Value>, V: Into<Value>>(&mut self, key: K, value: V) {
        let key = key.into();
        let value = value.into();
        if let Value::Int(k) = key
            && let Ok(k) = usize::try_from(k)
        {
            match k.cmp(&self.entries.array.len()) {
                Ordering::Less => return self.entries.array[k] = value,
                Ordering::Equal => return self.entries.array.push(value),
                Ordering::Greater => (),
            }
        }
        if value.is_null() {
            self.entries.map.remove(&key);
        } else {
            self.entries.map.insert(key, value);
        }
    }

    pub fn len(&self) -> usize {
        self.entries.array.len() + self.entries.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn metatable(&self) -> Option<RcTable> {
        self.metatable.clone()
    }

    pub fn set_metatable(&mut self, metatable: Option<RcTable>) {
        self.metatable = metatable;
    }

    pub fn iter(&self) -> TableIter<'_> {
        TableIter { table: self, i: 0 }
    }
}

#[derive(Debug, Clone)]
enum TableIterKind {
    Keys,
    Values,
    Items,
}

#[derive(Debug, Clone)]
pub struct TableIterCallback {
    kind: TableIterKind,
    table: RcTable,
    i: usize,
}

impl TableIterCallback {
    pub fn new(table: RcTable) -> Self {
        TableIterCallback {
            kind: TableIterKind::Items,
            table,
            i: 0,
        }
    }

    pub fn keys(table: RcTable) -> Self {
        TableIterCallback {
            kind: TableIterKind::Keys,
            table,
            i: 0,
        }
    }

    pub fn values(table: RcTable) -> Self {
        TableIterCallback {
            kind: TableIterKind::Values,
            table,
            i: 0,
        }
    }
}

impl CallbackFn for TableIterCallback {
    fn call(&mut self, _ctx: &Context, args: &[Value]) -> super::CallbackResult {
        ArgumentRange::check_iter_callback(args, self.i == 0)?;

        self.i += 1;
        if let Some((k, v)) = self.table.get_index(self.i - 1) {
            Ok(CallbackReturn::Perform {
                effect: Effect::Builtin(BuiltinEffect::Yield).into(),
                args: vec![match self.kind {
                    TableIterKind::Keys => k,
                    TableIterKind::Values => v,
                    TableIterKind::Items => Value::from(&[k, v]),
                }],
            })
        } else {
            Ok(CallbackReturn::ReturnValue { value: Value::Null })
        }
    }
}

impl<'a> IntoIterator for &'a Table {
    type Item = (Value, Value);
    type IntoIter = TableIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl MetaMethod<&Context> for RcTable {
    impl_metamethod!(Table);

    impl_metamethod!(Table, call);

    #[inline]
    fn meta_iter(self, ctx: &Context) -> Result<Self::ResultIter, Self::Error> {
        call_meta_iter!(Table, ctx, self);
        Function::Callback(Rc::new(Callback::new(TableIterCallback::new(self)))).meta_iter(ctx)
    }

    #[inline]
    fn meta_len(self, ctx: &Context) -> Result<Self::Result1, Self::Error> {
        call_metamethod!(ctx, MetaName::Len, self);
        Ok(self.len().into())
    }

    #[inline]
    fn meta_bool(self, ctx: &Context) -> Result<Self::Result1, Self::Error> {
        call_metamethod!(ctx, MetaName::Bool, self);
        Ok((!self.is_empty()).into())
    }

    impl_metamethod!(Table, metatable, Int, meta_int, 1);
    impl_metamethod!(Table, metatable, Float, meta_float, 1);

    #[inline]
    fn meta_str(self, ctx: &Context) -> Result<Self::Result1, Self::Error> {
        call_metamethod!(ctx, MetaName::Str, self);
        Ok(self.to_compact_string().into())
    }

    #[inline]
    fn meta_repr(self, ctx: &Context) -> Result<Self::Result1, Self::Error> {
        enum ValueOrStr {
            Value(Value),
            Str(&'static str),
        }

        call_metamethod!(ctx, MetaName::Repr, self);

        let mut result = CompactString::new("");
        let mut visited = OrderSet::new();
        let mut stack = vec![ValueOrStr::Value(Value::Table(self))];
        while let Some(current) = stack.pop() {
            match current {
                ValueOrStr::Value(Value::Table(t)) => {
                    let id = Rc::as_ptr(&t).cast::<()>();
                    if visited.contains(&id) {
                        result.push_str("<table>");
                        continue;
                    }
                    visited.insert(id);
                    stack.push(ValueOrStr::Str("}"));
                    for i in (0..t.len()).rev() {
                        let Some((k, v)) = t.get_index(i) else {
                            continue;
                        };
                        if i != t.len() - 1 {
                            stack.push(ValueOrStr::Str(", "));
                        }
                        stack.push(ValueOrStr::Value(v));
                        stack.push(ValueOrStr::Str(": "));
                        stack.push(ValueOrStr::Value(k));
                    }
                    stack.push(ValueOrStr::Str("{"));
                }
                ValueOrStr::Value(v) => result.push_str(&v.to_compact_string()),
                ValueOrStr::Str(s) => result.push_str(s),
            }
        }
        Ok(result.into())
    }

    impl_metamethod!(Table, metatable, Neg, meta_neg, 1);
    impl_metamethod!(Table, metatable, Add, meta_add, 2);
    impl_metamethod!(Table, metatable, Sub, meta_sub, 2);
    impl_metamethod!(Table, metatable, Mul, meta_mul, 2);
    impl_metamethod!(Table, metatable, Div, meta_div, 2);
    impl_metamethod!(Table, metatable, Rem, meta_rem, 2);

    fn meta_eq(self, ctx: &Context, other: Self::Value) -> Result<Self::Result2, Self::Error> {
        call_metamethod!(ctx, MetaName::Eq, self, other);
        if let Value::Table(other) = other {
            Ok((self == other).into())
        } else {
            Ok(false.into())
        }
    }

    fn meta_ne(self, ctx: &Context, other: Self::Value) -> Result<Self::Result2, Self::Error> {
        call_metamethod!(ctx, MetaName::Ne, self, other);
        if let Value::Table(other) = other {
            Ok((self != other).into())
        } else {
            Ok(true.into())
        }
    }

    impl_metamethod!(Table, metatable, Gt, meta_gt, 2);
    impl_metamethod!(Table, metatable, Ge, meta_ge, 2);
    impl_metamethod!(Table, metatable, Lt, meta_lt, 2);
    impl_metamethod!(Table, metatable, Le, meta_le, 2);

    #[inline]
    fn meta_get_attr(self, ctx: &Context, key: Self::Value) -> Result<Self::Result2, Self::Error> {
        call_metamethod!(ctx, MetaName::GetAttr, self, key);
        Ok(MetaResult::ReturnValue {
            value: self.get(key),
        })
    }

    #[inline]
    fn meta_get_item(self, ctx: &Context, key: Self::Value) -> Result<Self::Result2, Self::Error> {
        call_metamethod!(ctx, MetaName::GetItem, self, key);
        Ok(MetaResult::ReturnValue {
            value: self.get(key),
        })
    }

    #[inline]
    fn meta_set_attr(
        self,
        ctx: &Context,
        key: Self::Value,
        value: Self::Value,
    ) -> Result<Self::Result3, Self::Error> {
        call_metamethod!(ctx, MetaName::SetAttr, self, key, value);
        let mut table = Rc::unwrap_or_clone(self);
        table.set(key, value);
        Ok(table.into())
    }

    #[inline]
    fn meta_set_item(
        self,
        ctx: &Context,
        key: Self::Value,
        value: Self::Value,
    ) -> Result<Self::Result3, Self::Error> {
        call_metamethod!(ctx, MetaName::SetItem, self, key, value);
        let mut table = Rc::unwrap_or_clone(self);
        table.set(key, value);
        Ok(table.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct TableEntries {
    array: Vec<Value>,
    map: OrderMap<Value, Value>,
}

impl FromIterator<Value> for TableEntries {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        let array = Vec::from_iter(iter);
        TableEntries {
            array,
            map: OrderMap::new(),
        }
    }
}

impl FromIterator<(Value, Value)> for TableEntries {
    fn from_iter<T: IntoIterator<Item = (Value, Value)>>(iter: T) -> Self {
        let map = OrderMap::from_iter(iter);
        TableEntries {
            array: Vec::new(),
            map,
        }
    }
}

#[derive(Debug)]
pub struct TableIter<'a> {
    table: &'a Table,
    i: usize,
}

impl Iterator for TableIter<'_> {
    type Item = (Value, Value);

    fn next(&mut self) -> Option<Self::Item> {
        self.nth(0)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.table.len() - self.i, Some(self.table.len() - self.i))
    }

    fn count(self) -> usize {
        self.table.len() - self.i
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.i += n + 1;
        self.table.get_index(self.i - 1)
    }

    fn last(mut self) -> Option<Self::Item> {
        self.i = self.table.len();
        self.table.get_index(self.i - 1)
    }
}

impl ExactSizeIterator for TableIter<'_> {}

impl From<Table> for Value {
    fn from(value: Table) -> Self {
        Value::Table(Rc::new(value))
    }
}

impl<T: Into<TableEntries>> From<T> for Value {
    fn from(value: T) -> Self {
        Value::Table(Rc::new(Table {
            entries: value.into(),
            metatable: None,
        }))
    }
}

impl<T: Into<Value>> From<Vec<T>> for TableEntries {
    fn from(value: Vec<T>) -> Self {
        value.into_iter().map(Into::into).collect()
    }
}

impl<T: Into<Value> + Clone> From<&[T]> for TableEntries {
    fn from(value: &[T]) -> Self {
        value.iter().cloned().map(Into::into).collect()
    }
}

impl<T: Into<Value> + Clone, const N: usize> From<&[T; N]> for TableEntries {
    fn from(value: &[T; N]) -> Self {
        value.iter().cloned().map(Into::into).collect()
    }
}

impl<K: Into<Value>, V: Into<Value>> From<Vec<(K, V)>> for TableEntries {
    fn from(value: Vec<(K, V)>) -> Self {
        value
            .into_iter()
            .map(|(k, v)| (k.into(), v.into()))
            .collect()
    }
}

impl<K: Into<Value> + Clone, V: Into<Value> + Clone> From<&[(K, V)]> for TableEntries {
    fn from(value: &[(K, V)]) -> Self {
        value
            .iter()
            .cloned()
            .map(|(k, v)| (k.into(), v.into()))
            .collect()
    }
}

impl<K: Into<Value> + Clone, V: Into<Value> + Clone, const N: usize> From<&[(K, V); N]>
    for TableEntries
{
    fn from(value: &[(K, V); N]) -> Self {
        value
            .iter()
            .cloned()
            .map(|(k, v)| (k.into(), v.into()))
            .collect()
    }
}
