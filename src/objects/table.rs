use std::fmt::{Debug, Display};
use std::iter::FusedIterator;
use std::ops::Index;

use indexmap::IndexMap;

use crate::gc::Trace;
use crate::utils::{Join, ValueDebug};

use super::Value;

/// The table implement.
#[derive(Clone, PartialEq, Eq)]
pub struct Table {
    pub array: Vec<(Value, Value)>,
    pub mapping: IndexMap<Value, Value>,
    pub metatable: Value,
}

impl Table {
    /// Creates an empty Table.
    ///
    /// The table is initially created with a capacity of 0,
    /// so it will not allocate until it is first inserted into.
    #[inline]
    pub fn new() -> Self {
        Table {
            array: Vec::new(),
            mapping: IndexMap::new(),
            metatable: Value::Null,
        }
    }

    /// Creates a Table from an array.
    pub fn from_array(array: Vec<(Value, Value)>) -> Self {
        Table {
            array,
            mapping: IndexMap::new(),
            metatable: Value::Null,
        }
    }

    /// Creates a Table from a mapping.
    pub fn from_mapping(mapping: IndexMap<Value, Value>) -> Self {
        Table {
            array: Vec::new(),
            mapping,
            metatable: Value::Null,
        }
    }

    /// Return an iterator over the key-value pairs of the table, in their order.
    pub fn iter(&self) -> Iter<'_> {
        Iter {
            array: self.array.iter(),
            mapping: self.mapping.iter(),
        }
    }

    /// Return an iterator over the key-value pairs of the table, in their order.
    pub fn iter_mut(&mut self) -> IterMut<'_> {
        IterMut {
            array: self.array.iter_mut(),
            mapping: self.mapping.iter_mut(),
        }
    }

    /// Return an iterator over the keys of the table, in their order.
    pub fn keys(&self) -> Keys<'_> {
        Keys { inner: self.iter() }
    }

    /// Return an owning iterator over the keys of the table, in their order.
    pub fn into_keys(self) -> IntoKeys {
        IntoKeys {
            inner: self.into_iter(),
        }
    }

    /// Return an iterator over the values of the table, in their order.
    pub fn values(&self) -> Values<'_> {
        Values { inner: self.iter() }
    }

    /// Return an iterator over mutable references to the values of the table, in their order.
    pub fn values_mut(&mut self) -> ValuesMut<'_> {
        ValuesMut {
            inner: self.iter_mut(),
        }
    }

    /// Return an owning iterator over the values of the table, in their order.
    pub fn into_values(self) -> IntoValues {
        IntoValues {
            inner: self.into_iter(),
        }
    }

    /// Return the number of key-value pairs in the table.
    ///
    /// Computes in **O(1)** time.
    #[inline]
    pub fn len(&self) -> usize {
        self.array.len() + self.mapping.len()
    }

    /// Returns true if the table contains no elements.
    ///
    /// Computes in **O(1)** time.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.array.is_empty() && self.mapping.is_empty()
    }

    /// Remove all key-value pairs in the table, while preserving its capacity.
    ///
    /// Computes in **O(n)** time.
    #[inline]
    pub fn clear(&mut self) {
        self.array.clear();
        self.mapping.clear();
        self.metatable = Value::Null;
    }

    /// Get a key-value pair by index.
    ///
    /// Valid indices are *0 <= index < self.len()*.
    ///
    /// Computes in **O(1)** time.
    pub fn get_index(&self, index: usize) -> Option<(&Value, &Value)> {
        if index < self.array.len() {
            self.array.get(index).map(|(k, v)| (k, v))
        } else {
            self.mapping.get_index(index - self.array.len())
        }
    }

    /// Get a key-value pair by index.
    ///
    /// Valid indices are *0 <= index < self.len()*.
    ///
    /// Computes in **O(1)** time.
    pub fn get_index_mut(&mut self, index: usize) -> Option<(&mut Value, &mut Value)> {
        if index < self.array.len() {
            self.array.get_mut(index).map(|(k, v)| (k, v))
        } else {
            self.mapping.get_index_mut(index - self.array.len())
        }
    }

    /// Return a reference to the value stored for `key`, if it is present,
    /// else `None`.
    ///
    /// Computes in **O(1)** time (average).
    pub fn get(&self, key: &Value) -> Option<&Value> {
        if let Some(key) = key.as_int() {
            if let Ok(key) = usize::try_from(key) {
                return self.array.get(key).map(|(_k, v)| v);
            }
        }
        self.mapping.get(key)
    }

    /// Return a mutable reference to the value stored for `key`, if it is present,
    /// else `None`.
    ///
    /// Computes in **O(1)** time (average).
    pub fn get_mut(&mut self, key: &Value) -> Option<&mut Value> {
        if let Some(key) = key.as_int() {
            if let Ok(key) = usize::try_from(key) {
                return self.array.get_mut(key).map(|(_k, v)| v);
            }
        }
        self.mapping.get_mut(key)
    }

    /// Insert a key-value pair in the table.
    ///
    /// Computes in **O(1)** time (amortized average).
    pub fn set(&mut self, key: &Value, value: Value) {
        if let Some(k) = key.as_int() {
            if let Ok(k) = usize::try_from(k) {
                match k.cmp(&self.array.len()) {
                    std::cmp::Ordering::Less => return self.array[k] = (*key, value),
                    std::cmp::Ordering::Equal => return self.array.push((*key, value)),
                    std::cmp::Ordering::Greater => (),
                }
            }
        }
        if !value.is_null() {
            self.mapping.insert(*key, value);
        } else {
            self.mapping.remove(key);
        }
    }

    /// Return the repr string fo the table.
    pub(crate) fn repr_table(&self, t: &Value) -> String {
        format!(
            "{{{}}}",
            self.iter()
                .map(|(k, v)| {
                    format!(
                        "{}: {}",
                        if k.is(t) {
                            "<table>".to_string()
                        } else if let Some(k_t) = k.as_table() {
                            k_t.repr_table(t)
                        } else {
                            k.repr()
                        },
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

unsafe impl Trace for Table {
    #[inline]
    unsafe fn trace(&self) {
        self.metatable.trace();
        for (k, v) in self {
            k.trace();
            v.trace();
        }
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

        let mut t = f.debug_map();
        t.entries(
            self.array
                .iter()
                .map(|(k, v)| (ValueDebug(k), ValueDebug(v))),
        )
        .entries(
            self.mapping
                .iter()
                .map(|(k, v)| (ValueDebug(k), ValueDebug(v))),
        );
        if !self.metatable.is_null() {
            t.entry(&DebugMetaSign {}, &self.metatable);
        }
        t.finish()
    }
}

impl Display for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<table>")
    }
}

impl<const N: usize> From<[Value; N]> for Table {
    fn from(arr: [Value; N]) -> Self {
        Self::from_iter(arr)
    }
}

impl<const N: usize> From<[(Value, Value); N]> for Table {
    fn from(arr: [(Value, Value); N]) -> Self {
        Self::from_iter(arr)
    }
}

impl Index<&Value> for Table {
    type Output = Value;

    fn index(&self, index: &Value) -> &Self::Output {
        self.get(index).expect("no entry found for key")
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

impl<'a> IntoIterator for &'a Table {
    type Item = (&'a Value, &'a Value);

    type IntoIter = Iter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a> IntoIterator for &'a mut Table {
    type Item = (&'a Value, &'a mut Value);

    type IntoIter = IterMut<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

#[derive(Clone)]
pub struct Iter<'a> {
    array: std::slice::Iter<'a, (Value, Value)>,
    mapping: indexmap::map::Iter<'a, Value, Value>,
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
    mapping: indexmap::map::IterMut<'a, Value, Value>,
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
    mapping: indexmap::map::IntoIter<Value, Value>,
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

impl FromIterator<Value> for Table {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        let mut table = Table::new();
        table.extend(iter);
        table
    }
}

impl FromIterator<(Value, Value)> for Table {
    fn from_iter<T: IntoIterator<Item = (Value, Value)>>(iter: T) -> Self {
        let mut table = Table::new();
        table.extend(iter);
        table
    }
}

impl Extend<Value> for Table {
    fn extend<T: IntoIterator<Item = Value>>(&mut self, iter: T) {
        self.array.extend(
            iter.into_iter()
                .enumerate()
                .map(|(k, v)| (Value::Int(k.try_into().unwrap()), v)),
        )
    }
}

impl Extend<(Value, Value)> for Table {
    fn extend<T: IntoIterator<Item = (Value, Value)>>(&mut self, iter: T) {
        for (k, v) in iter {
            self.set(&k, v);
        }
    }
}

/// Helper macro for creating instances of `Table`.
#[macro_export]
macro_rules! table {
    () => {
        $crate::objects::Table::new()
    };
    [$($x:expr),* $(,)?] => {{
        let mut temp_vec = Vec::with_capacity(0 $( + {let _ = &$x; 1} )*);
        let mut count = -1;
        $(
            count += 1;
            temp_vec.push(($crate::objects::Value::Int(count), $x));
        )*
        $crate::objects::Table::from_array(temp_vec)
    }};
    {$($k:expr => $v:expr),* $(,)?} => {{
        let mut temp_map = $crate::objects::indexmap::IndexMap::with_capacity(0 $( + {let _ = &$k; 1} )*);
        $(
            temp_map.insert($k, $v);
        )*
        $crate::objects::Table::from_mapping(temp_map)
    }};
}
