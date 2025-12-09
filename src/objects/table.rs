use std::{cmp::Ordering, rc::Rc};

use compact_str::{CompactString, ToCompactString};
use derive_more::Display;
use ordermap::{OrderMap, OrderSet};

use crate::{
    Context,
    compiler::value::{MetaMethod, MetaName},
    objects::{MetaResult, Value, call_metamethod, call_metamethod_error, impl_metamethod},
};

pub type Table = Rc<TableInner>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default, Display)]
#[display("<table {self:p}>")]
pub struct TableInner {
    pub entries: TableEntries,
    pub metatable: Option<Table>,
}

impl TableInner {
    pub fn new() -> Self {
        TableInner::default()
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
                .map(|v| (Value::Int(index.try_into().unwrap()), v.clone()))
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

    pub fn metatable(&self) -> Option<Table> {
        self.metatable.clone()
    }

    pub fn set_metatable(&mut self, metatable: Option<Table>) {
        self.metatable = metatable;
    }

    pub fn iter(&self) -> TableIter<'_> {
        TableIter { table: self, i: 0 }
    }

    // pub fn iter_callback(&self, ctx: Context) -> Callback {
    //     Callback::from_fn_with(
    //         &ctx,
    //         Gc::new(&ctx, RefLock::new(self.iter())),
    //         |iter, ctx, _args| {
    //             Ok(CallbackReturn::Return(iter.borrow_mut(&ctx).next().map_or(
    //                 Value::Null,
    //                 |(k, v)| {
    //                     let t = Table::new(&ctx);
    //                     t.set(ctx, 0_i64, k);
    //                     t.set(ctx, 1_i64, v);
    //                     t.into()
    //                 },
    //             )))
    //         },
    //     )
    // }
}

impl<'a> IntoIterator for &'a TableInner {
    type Item = (Value, Value);
    type IntoIter = TableIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl MetaMethod<&Context> for Table {
    impl_metamethod!(Table);

    #[inline]
    fn meta_call(self, ctx: &Context) -> Result<Self::ResultCall, Self::Error> {
        if let Some(metatable) = self.metatable() {
            #[expect(clippy::wildcard_enum_match_arm)]
            match metatable.get(MetaName::Call) {
                Value::Function(v) => Ok(v),
                Value::Table(v) => v.meta_call(ctx), // TODO: prevent infinite recursion
                v => Err(v.meta_error(ctx, MetaName::Call, vec![])),
            }
        } else {
            Err(self.meta_error(ctx, MetaName::Call, vec![]))
        }
    }

    // fn meta_iter(&self, ctx: &Context) -> Result<Self::ResultIter, Self::Error> {
    //     if let Some(metatable) = self.metatable() {
    //         let t = metatable.get(MetaName::Iter);
    //         if !t.is_null() {
    //             return Ok(Function::Callback(Callback::from_fn_with(
    //                 &ctx,
    //                 (t.meta_call(ctx)?, *self),
    //                 |(f, v), _ctx, _args| Ok(CallbackReturn::TailCall(*f, vec![(*v).into()])),
    //             )));
    //         }
    //     }
    //     Ok(Function::Callback(self.iter_callback(ctx)))
    // }

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

    call_metamethod_error!(1, meta_int, Int);
    call_metamethod_error!(1, meta_float, Float);

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

    call_metamethod_error!(1, meta_neg, Neg);
    call_metamethod_error!(2, meta_add, Add);
    call_metamethod_error!(2, meta_sub, Sub);
    call_metamethod_error!(2, meta_mul, Mul);
    call_metamethod_error!(2, meta_div, Div);
    call_metamethod_error!(2, meta_rem, Rem);

    impl_metamethod!(Table, eq_ne);
    call_metamethod_error!(2, meta_gt, Gt);
    call_metamethod_error!(2, meta_ge, Ge);
    call_metamethod_error!(2, meta_lt, Lt);
    call_metamethod_error!(2, meta_le, Le);

    #[inline]
    fn meta_get_attr(self, ctx: &Context, key: Self::Value) -> Result<Self::Result2, Self::Error> {
        call_metamethod!(ctx, MetaName::GetAttr, self, key);
        Ok(MetaResult::Value(self.get(key)))
    }

    #[inline]
    fn meta_get_item(self, ctx: &Context, key: Self::Value) -> Result<Self::Result2, Self::Error> {
        call_metamethod!(ctx, MetaName::GetItem, self, key);
        Ok(MetaResult::Value(self.get(key)))
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
    table: &'a TableInner,
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

impl From<TableInner> for Value {
    fn from(value: TableInner) -> Self {
        Value::Table(Table::new(value))
    }
}

impl<T: Into<TableEntries>> From<T> for Value {
    fn from(value: T) -> Self {
        Value::Table(Table::new(TableInner {
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

impl<K: Into<Value>, V: Into<Value>> From<Vec<(K, V)>> for TableEntries {
    fn from(value: Vec<(K, V)>) -> Self {
        value
            .into_iter()
            .map(|(k, v)| (k.into(), v.into()))
            .collect()
    }
}
