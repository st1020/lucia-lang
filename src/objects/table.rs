use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::iter::FusedIterator;

use crate::utils::{Join, ValueDebug};

use super::Value;

/// The table implement.
#[derive(Clone, PartialEq, Eq)]
pub struct Table {
    pub array: Vec<(Value, Value)>,
    pub mapping: HashMap<Value, Value>,
    pub metatable: Value,
}

impl Table {
    #[inline]
    pub fn new() -> Self {
        Table {
            array: Vec::new(),
            mapping: HashMap::new(),
            metatable: Value::Null,
        }
    }

    pub fn iter(&self) -> Iter<'_> {
        Iter {
            array: self.array.iter(),
            mapping: self.mapping.iter(),
        }
    }

    pub fn iter_mut(&mut self) -> IterMut<'_> {
        IterMut {
            array: self.array.iter_mut(),
            mapping: self.mapping.iter_mut(),
        }
    }

    pub fn keys(&self) -> Keys<'_> {
        Keys { inner: self.iter() }
    }

    pub fn into_keys(self) -> IntoKeys {
        IntoKeys {
            inner: self.into_iter(),
        }
    }

    pub fn values(&self) -> Values<'_> {
        Values { inner: self.iter() }
    }

    pub fn values_mut(&mut self) -> ValuesMut<'_> {
        ValuesMut {
            inner: self.iter_mut(),
        }
    }

    pub fn into_values(self) -> IntoValues {
        IntoValues {
            inner: self.into_iter(),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.array.len() + self.mapping.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.array.is_empty() && self.mapping.is_empty()
    }

    #[inline]
    pub fn clear(&mut self) {
        self.array.clear();
        self.mapping.clear();
        self.metatable = Value::Null;
    }

    pub fn raw_get(&self, key: &Value) -> Option<&Value> {
        if let Some(key) = key.as_int() {
            if let Ok(key) = usize::try_from(key) {
                return self.array.get(key).map(|(_k, v)| v);
            }
        }
        self.mapping.get(key)
    }

    pub fn get(&self, key: &Value) -> Option<&Value> {
        self.raw_get(key)
            .or_else(|| self.metatable.as_table().and_then(|v| v.get(key)))
    }

    pub fn set(&mut self, key: &Value, value: Value) {
        if let Some(k) = key.as_int() {
            if let Ok(k) = usize::try_from(k) {
                if k == self.array.len() {
                    self.array.push((*key, value));
                    return;
                }
            }
        }
        if !value.is_null() {
            self.mapping.insert(*key, value);
        } else {
            self.mapping.remove(key);
        }
    }

    pub(crate) fn repr_table(&self, t: &Value) -> String {
        format!(
            "{{{}}}",
            self.iter()
                .map(|(k, v)| {
                    format!(
                        "{}: {}",
                        k.repr(),
                        if v.is(t) {
                            "<table>".to_string()
                        } else if let Some(v_t) = v.as_table() {
                            v_t.repr_table(t)
                        } else {
                            v.repr()
                        },
                    )
                })
                .join(", ")
        )
    }
}

impl Default for Table {
    fn default() -> Self {
        Self::new()
    }
}

impl Debug for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct DebugMetaSign {}

        impl Debug for DebugMetaSign {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "#")
            }
        }

        f.debug_map()
            .entries(
                self.array
                    .iter()
                    .map(|(k, v)| (ValueDebug(k), ValueDebug(v))),
            )
            .entries(
                self.mapping
                    .iter()
                    .map(|(k, v)| (ValueDebug(k), ValueDebug(v))),
            )
            .entry(&DebugMetaSign {}, &self.metatable)
            .finish()
    }
}

impl Display for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<table>")
    }
}

impl From<Vec<Value>> for Table {
    fn from(value: Vec<Value>) -> Self {
        let mut temp = Vec::new();
        for (i, t) in value.into_iter().enumerate() {
            temp.push((Value::Int(i.try_into().unwrap()), t));
        }
        Table {
            array: temp,
            mapping: HashMap::new(),
            metatable: Value::Null,
        }
    }
}

impl IntoIterator for Table {
    type Item = (Value, Value);
    type IntoIter = IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            array: self.array.into_iter(),
            mapping: self.mapping.into_iter(),
        }
    }
}

#[derive(Clone)]
pub struct Iter<'a> {
    array: std::slice::Iter<'a, (Value, Value)>,
    mapping: std::collections::hash_map::Iter<'a, Value, Value>,
}

impl<'a> Iterator for Iter<'a> {
    type Item = (&'a Value, &'a Value);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.array
            .next()
            .map(|(k, v)| (k, v))
            .or_else(|| self.mapping.next())
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let array = self.array.size_hint().0;
        let mapping = self.mapping.size_hint().0;
        (array + mapping, Some(array + mapping))
    }
}

impl ExactSizeIterator for Iter<'_> {
    #[inline]
    fn len(&self) -> usize {
        self.array.len() + self.mapping.len()
    }
}

impl FusedIterator for Iter<'_> {}

pub struct IterMut<'a> {
    array: std::slice::IterMut<'a, (Value, Value)>,
    mapping: std::collections::hash_map::IterMut<'a, Value, Value>,
}

impl<'a> Iterator for IterMut<'a> {
    type Item = (&'a Value, &'a mut Value);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.array
            .next()
            .map(|(k, v)| (&*k, v))
            .or_else(|| self.mapping.next())
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let array = self.array.size_hint().0;
        let mapping = self.mapping.size_hint().0;
        (array + mapping, Some(array + mapping))
    }
}

impl ExactSizeIterator for IterMut<'_> {
    #[inline]
    fn len(&self) -> usize {
        self.array.len() + self.mapping.len()
    }
}

impl FusedIterator for IterMut<'_> {}

pub struct IntoIter {
    array: std::vec::IntoIter<(Value, Value)>,
    mapping: std::collections::hash_map::IntoIter<Value, Value>,
}

impl Iterator for IntoIter {
    type Item = (Value, Value);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.array.next().or_else(|| self.mapping.next())
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let array = self.array.size_hint().0;
        let mapping = self.mapping.size_hint().0;
        (array + mapping, Some(array + mapping))
    }
}

impl ExactSizeIterator for IntoIter {
    #[inline]
    fn len(&self) -> usize {
        self.array.len() + self.mapping.len()
    }
}

impl FusedIterator for IntoIter {}

#[derive(Clone)]
pub struct Keys<'a> {
    inner: Iter<'a>,
}

impl<'a> Iterator for Keys<'a> {
    type Item = &'a Value;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(k, _)| k)
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl ExactSizeIterator for Keys<'_> {
    #[inline]
    fn len(&self) -> usize {
        self.inner.len()
    }
}

impl FusedIterator for Keys<'_> {}

pub struct Values<'a> {
    inner: Iter<'a>,
}

impl<'a> Iterator for Values<'a> {
    type Item = &'a Value;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(_, v)| v)
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl ExactSizeIterator for Values<'_> {
    #[inline]
    fn len(&self) -> usize {
        self.inner.len()
    }
}

impl FusedIterator for Values<'_> {}

pub struct ValuesMut<'a> {
    inner: IterMut<'a>,
}

impl<'a> Iterator for ValuesMut<'a> {
    type Item = &'a mut Value;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(_, v)| v)
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl ExactSizeIterator for ValuesMut<'_> {
    #[inline]
    fn len(&self) -> usize {
        self.inner.len()
    }
}

impl FusedIterator for ValuesMut<'_> {}

pub struct IntoKeys {
    inner: IntoIter,
}

impl Iterator for IntoKeys {
    type Item = Value;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(k, _)| k)
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl ExactSizeIterator for IntoKeys {
    #[inline]
    fn len(&self) -> usize {
        self.inner.len()
    }
}

impl FusedIterator for IntoKeys {}

pub struct IntoValues {
    inner: IntoIter,
}

impl Iterator for IntoValues {
    type Item = Value;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(_, v)| v)
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl ExactSizeIterator for IntoValues {
    #[inline]
    fn len(&self) -> usize {
        self.inner.len()
    }
}

impl FusedIterator for IntoValues {}

/// Helper macro for creating instances of `Table`.
#[macro_export]
macro_rules! table {
    () => {
        $crate::objects::table::Table::new()
    };
    [$($x:expr),* $(,)?] => {
        {
            let mut temp_vec = Vec::with_capacity(0 $( + {let _ = &$x; 1} )*);
            let mut count = -1;
            $(
                count += 1;
                temp_vec.push(($crate::objects::Value::Int(count), $x));
            )*
            $crate::objects::table::Table {
                array: temp_vec,
                mapping: std::collections::HashMap::new(),
                metatable: $crate::objects::Value::Null,
            }
        }
    };
    {$($k:expr => $v:expr),* $(,)?} => {
        {
            let mut temp_map = std::collections::HashMap::with_capacity(0 $( + {let _ = &$k; 1} )*);
            $(
                temp_map.insert($k, $v);
            )*
            $crate::objects::table::Table {
                array: std::vec::Vec::new(),
                mapping: temp_map,
                metatable: $crate::objects::Value::Null,
            }
        }
    };
}
