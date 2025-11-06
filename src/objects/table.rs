use compact_str::ToCompactString;
use gc_arena::{Collect, Gc, Mutation, lock::RefLock};
use indexmap::IndexMap;

use crate::{
    Context,
    compiler::value::{MetaMethod, MetaName},
    objects::{
        Callback, CallbackReturn, Equal, Function, IntoMetaResult, IntoValue, MetaResult, Repr,
        Value, call_metamethod, call_metamethod_error, define_object, impl_metamethod,
    },
};

pub type TableInner<'gc> = RefLock<TableState<'gc>>;

define_object!(Table, TableInner<'gc>, ptr, "table");

impl<'gc> Table<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Self {
        Table::from(mc, TableState::default())
    }

    pub fn from(mc: &Mutation<'gc>, table_state: TableState<'gc>) -> Self {
        Table(Gc::new(mc, RefLock::new(table_state)))
    }

    pub fn get<K: IntoValue<'gc>>(self, ctx: Context<'gc>, key: K) -> Value<'gc> {
        let key = key.into_value(ctx);
        let entries = &self.0.borrow().entries;
        if let Value::Int(key) = key
            && let Ok(key) = usize::try_from(key)
            && key < entries.array.len()
        {
            return entries.array[key];
        }
        entries.map.get(&key).cloned().unwrap_or(Value::Null)
    }

    pub fn get_index(self, index: usize) -> Option<(Value<'gc>, Value<'gc>)> {
        let entries = &self.0.borrow().entries;
        if index < entries.array.len() {
            entries
                .array
                .get(index)
                .map(|v| (Value::Int(index.try_into().unwrap()), *v))
        } else {
            entries
                .map
                .get_index(index - entries.array.len())
                .map(|(k, v)| (*k, *v))
        }
    }

    pub fn set<K: IntoValue<'gc>, V: IntoValue<'gc>>(self, ctx: Context<'gc>, key: K, value: V) {
        let key = key.into_value(ctx);
        let value = value.into_value(ctx);
        let entries = &mut self.0.borrow_mut(&ctx).entries;
        if let Value::Int(k) = key
            && let Ok(k) = usize::try_from(k)
        {
            match k.cmp(&entries.array.len()) {
                std::cmp::Ordering::Less => return entries.array[k] = value,
                std::cmp::Ordering::Equal => return entries.array.push(value),
                std::cmp::Ordering::Greater => (),
            }
        }
        if value.is_null() {
            entries.map.shift_remove(&key);
        } else {
            entries.map.insert(key, value);
        }
    }

    pub fn len(self) -> usize {
        let entries = &self.0.borrow().entries;
        entries.array.len() + entries.map.len()
    }

    pub fn is_empty(self) -> bool {
        self.len() == 0
    }

    pub fn metatable(self) -> Option<Table<'gc>> {
        self.0.borrow().metatable
    }

    pub fn set_metatable(self, mc: &Mutation<'gc>, metatable: Option<Table<'gc>>) {
        self.0.borrow_mut(mc).metatable = metatable;
    }

    pub fn iter(self) -> TableIter<'gc> {
        TableIter { table: self, i: 0 }
    }

    pub fn iter_callback(self, ctx: Context<'gc>) -> Callback<'gc> {
        Callback::from_fn_with(
            &ctx,
            Gc::new(&ctx, RefLock::new(self.iter())),
            |iter, ctx, _args| {
                Ok(CallbackReturn::Return(iter.borrow_mut(&ctx).next().map_or(
                    Value::Null,
                    |(k, v)| {
                        let t = Table::new(&ctx);
                        t.set(ctx, 0, k);
                        t.set(ctx, 1, v);
                        t.into()
                    },
                )))
            },
        )
    }
}

impl<'gc> MetaMethod<Context<'gc>> for Table<'gc> {
    impl_metamethod!(Table);

    fn meta_call(&self, ctx: Context<'gc>) -> Result<Self::ResultCall, Self::Error> {
        if let Some(metatable) = self.metatable() {
            match metatable.get(ctx, MetaName::Call) {
                Value::Function(v) => Ok(v),
                Value::Table(v) => v.meta_call(ctx),
                v => Err(v.meta_error(ctx, MetaName::Call, vec![])),
            }
        } else {
            Err(self.meta_error(ctx, MetaName::Call, vec![]))
        }
    }

    fn meta_iter(&self, ctx: Context<'gc>) -> Result<Self::ResultIter, Self::Error> {
        if let Some(metatable) = self.metatable() {
            let t = metatable.get(ctx, MetaName::Iter);
            if !t.is_null() {
                return Ok(Function::Callback(Callback::from_fn_with(
                    &ctx,
                    (t.meta_call(ctx)?, *self),
                    |(f, v), _ctx, _args| Ok(CallbackReturn::TailCall(*f, vec![(*v).into()])),
                )));
            }
        }
        Ok(Function::Callback(self.iter_callback(ctx)))
    }

    fn meta_len(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        call_metamethod!(ctx, MetaName::Len, self);
        Ok((self.len() as i64).into_meta_result(ctx))
    }

    fn meta_bool(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        call_metamethod!(ctx, MetaName::Bool, self);
        Ok((!self.is_empty()).into_meta_result(ctx))
    }

    call_metamethod_error!(1, meta_int, Int);
    call_metamethod_error!(1, meta_float, Float);

    fn meta_str(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        call_metamethod!(ctx, MetaName::Str, self);
        Ok(self.to_compact_string().into_meta_result(ctx))
    }

    fn meta_repr(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
        call_metamethod!(ctx, MetaName::Repr, self);
        Ok(self.repr().into_meta_result(ctx))
    }

    call_metamethod_error!(1, meta_neg, Neg);
    call_metamethod_error!(2, meta_add, Add);
    call_metamethod_error!(2, meta_sub, Sub);
    call_metamethod_error!(2, meta_mul, Mul);
    call_metamethod_error!(2, meta_div, Div);
    call_metamethod_error!(2, meta_rem, Rem);

    fn meta_eq(&self, ctx: Context<'gc>, other: Self::Value) -> Result<Self::Result2, Self::Error> {
        call_metamethod!(ctx, MetaName::Eq, self, other);
        Ok(if let Value::Table(other) = other {
            self.equal(&other)
        } else {
            false
        }
        .into_meta_result(ctx))
    }

    fn meta_ne(&self, ctx: Context<'gc>, other: Self::Value) -> Result<Self::Result2, Self::Error> {
        call_metamethod!(ctx, MetaName::Ne, self, other);
        Ok(if let Value::Table(other) = other {
            self.not_equal(&other)
        } else {
            true
        }
        .into_meta_result(ctx))
    }

    call_metamethod_error!(2, meta_gt, Gt);
    call_metamethod_error!(2, meta_ge, Ge);
    call_metamethod_error!(2, meta_lt, Lt);
    call_metamethod_error!(2, meta_le, Le);

    fn meta_get_attr(
        &self,
        ctx: Context<'gc>,
        key: Self::Value,
    ) -> Result<Self::Result2, Self::Error> {
        call_metamethod!(ctx, MetaName::GetAttr, self, key);
        Ok(MetaResult::Value(self.get(ctx, key)))
    }

    fn meta_get_item(
        &self,
        ctx: Context<'gc>,
        key: Self::Value,
    ) -> Result<Self::Result2, Self::Error> {
        call_metamethod!(ctx, MetaName::GetItem, self, key);
        Ok(MetaResult::Value(self.get(ctx, key)))
    }

    fn meta_set_attr(
        &self,
        ctx: Context<'gc>,
        key: Self::Value,
        value: Self::Value,
    ) -> Result<Self::Result3, Self::Error> {
        call_metamethod!(ctx, MetaName::SetAttr, self, key, value);
        self.set(ctx, key, value);
        Ok(MetaResult::Value(Value::Null))
    }

    fn meta_set_item(
        &self,
        ctx: Context<'gc>,
        key: Self::Value,
        value: Self::Value,
    ) -> Result<Self::Result3, Self::Error> {
        call_metamethod!(ctx, MetaName::SetItem, self, key, value);
        self.set(ctx, key, value);
        Ok(MetaResult::Value(Value::Null))
    }
}

#[derive(Debug, Default, Collect)]
#[collect(no_drop)]
pub struct TableState<'gc> {
    pub entries: TableEntries<'gc>,
    pub metatable: Option<Table<'gc>>,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct TableEntries<'gc> {
    array: Vec<Value<'gc>>,
    map: IndexMap<Value<'gc>, Value<'gc>>,
}

unsafe impl Collect for TableEntries<'_> {
    fn trace(&self, cc: &gc_arena::Collection) {
        self.array.trace(cc);
        for (key, value) in self.map.iter() {
            key.trace(cc);
            value.trace(cc);
        }
    }
}

impl<'gc> FromIterator<Value<'gc>> for TableEntries<'gc> {
    fn from_iter<T: IntoIterator<Item = Value<'gc>>>(iter: T) -> Self {
        let array = Vec::from_iter(iter);
        TableEntries {
            array,
            map: IndexMap::new(),
        }
    }
}

impl<'gc> FromIterator<(Value<'gc>, Value<'gc>)> for TableEntries<'gc> {
    fn from_iter<T: IntoIterator<Item = (Value<'gc>, Value<'gc>)>>(iter: T) -> Self {
        let map = IndexMap::from_iter(iter);
        TableEntries {
            array: Vec::new(),
            map,
        }
    }
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct TableIter<'gc> {
    table: Table<'gc>,
    i: usize,
}

impl<'gc> Iterator for TableIter<'gc> {
    type Item = (Value<'gc>, Value<'gc>);

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
