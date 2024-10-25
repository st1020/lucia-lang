use std::{
    hash::{Hash, Hasher},
    mem,
};

use compact_str::{format_compact, CompactString};
use gc_arena::{lock::RefLock, Collect, Gc, Mutation};
use indexmap::IndexMap;

use crate::{
    objects::{IntoValue, Value},
    Context,
};

pub type TableInner<'gc> = RefLock<TableState<'gc>>;

#[derive(Debug, Clone, Copy, Collect)]
#[collect(no_drop)]
pub struct Table<'gc>(Gc<'gc, TableInner<'gc>>);

impl<'gc> PartialEq for Table<'gc> {
    fn eq(&self, other: &Table<'gc>) -> bool {
        Gc::ptr_eq(self.0, other.0) || self.0.borrow().entries == other.0.borrow().entries
    }
}

impl<'gc> Eq for Table<'gc> {}

impl<'gc> Hash for Table<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Gc::as_ptr(self.0).hash(state)
    }
}

impl<'gc> Table<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Self {
        Table::from(mc, TableState::default())
    }

    pub fn from(mc: &Mutation<'gc>, table_state: TableState<'gc>) -> Self {
        Table(Gc::new(mc, RefLock::new(table_state)))
    }

    pub fn from_inner(inner: Gc<'gc, TableInner<'gc>>) -> Self {
        Self(inner)
    }

    pub fn into_inner(self) -> Gc<'gc, TableInner<'gc>> {
        self.0
    }

    pub fn get<K: IntoValue<'gc>>(self, ctx: Context<'gc>, key: K) -> Value<'gc> {
        let key = key.into_value(ctx);
        let entries = &self.0.borrow().entries;
        if let Value::Int(key) = key {
            if let Ok(key) = usize::try_from(key) {
                if key < entries.array.len() {
                    return entries.array[key];
                }
            }
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
        if let Value::Int(k) = key {
            if let Ok(k) = usize::try_from(k) {
                match k.cmp(&entries.array.len()) {
                    std::cmp::Ordering::Less => return entries.array[k] = value,
                    std::cmp::Ordering::Equal => return entries.array.push(value),
                    std::cmp::Ordering::Greater => (),
                }
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

    pub fn set_metatable(
        self,
        mc: &Mutation<'gc>,
        metatable: Option<Table<'gc>>,
    ) -> Option<Table<'gc>> {
        mem::replace(&mut self.0.borrow_mut(mc).metatable, metatable)
    }

    /// Return the repr string of the table.
    pub(crate) fn repr_table(self, t: Value<'gc>) -> CompactString {
        let mut temp = Vec::new();
        for i in 0..self.len() {
            if let Some((k, v)) = self.get_index(i) {
                temp.push(format!(
                    "{}: {}",
                    if k.is(t) {
                        CompactString::const_new("<table>")
                    } else if let Value::Table(k_t) = k {
                        k_t.repr_table(t)
                    } else {
                        k.repr()
                    },
                    if v.is(t) {
                        CompactString::const_new("<table>")
                    } else if let Value::Table(v_t) = v {
                        v_t.repr_table(t)
                    } else {
                        v.repr()
                    },
                ))
            }
        }
        format_compact!("{{{}}}", temp.join(", "))
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

unsafe impl<'gc> Collect for TableEntries<'gc> {
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
