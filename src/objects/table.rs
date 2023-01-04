use std::collections::HashMap;
use std::fmt::Display;
use std::iter::FusedIterator;

use super::LuciaValue;

/// The table implement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LuciaTable {
    pub array: Vec<(LuciaValue, LuciaValue)>,
    pub mapping: HashMap<LuciaValue, LuciaValue>,
    base_value: Option<LuciaValue>,
}

impl LuciaTable {
    #[inline]
    pub fn new() -> Self {
        LuciaTable {
            array: Vec::new(),
            mapping: HashMap::new(),
            base_value: None,
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
        self.base_value = None;
    }

    pub fn raw_get(&self, key: &LuciaValue) -> Option<LuciaValue> {
        if let Ok(key) = i64::try_from(*key) {
            if let Ok(key) = usize::try_from(key) {
                if let Some(v) = self.array.get(key) {
                    return Some(v.1);
                }
            }
        }
        self.mapping.get(key).copied()
    }

    pub fn get(&self, key: &LuciaValue) -> Option<LuciaValue> {
        self.raw_get(key).or_else(|| {
            self.base_value.and_then(
                |v| match <&LuciaTable>::try_from(self.raw_get(&v).unwrap()) {
                    Ok(v) => v.get(key),
                    Err(_) => None,
                },
            )
        })
    }

    pub fn set(&mut self, key: &LuciaValue, value: LuciaValue) {
        if let Ok(k) = i64::try_from(*key) {
            if let Ok(k) = usize::try_from(k) {
                if k == self.array.len() {
                    self.array.push((*key, value));
                    return;
                }
            }
        } else if let Ok(k) = String::try_from(*key) {
            if k == "__base__" && value != LuciaValue::Null {
                self.base_value = Some(*key);
            }
        }
        if value != LuciaValue::Null {
            self.mapping.insert(*key, value);
        } else {
            self.mapping.remove(key);
        }
    }
}

impl Default for LuciaTable {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for LuciaTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.iter()
                .map(|(k, v)| format!("{}: {}", k, v))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl From<Vec<LuciaValue>> for LuciaTable {
    fn from(value: Vec<LuciaValue>) -> Self {
        let mut temp = Vec::new();
        for (i, t) in value.into_iter().enumerate() {
            temp.push((LuciaValue::Int(i.try_into().unwrap()), t));
        }
        LuciaTable {
            array: temp,
            mapping: HashMap::new(),
            base_value: None,
        }
    }
}

impl IntoIterator for LuciaTable {
    type Item = (LuciaValue, LuciaValue);
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
    array: std::slice::Iter<'a, (LuciaValue, LuciaValue)>,
    mapping: std::collections::hash_map::Iter<'a, LuciaValue, LuciaValue>,
}

impl<'a> Iterator for Iter<'a> {
    type Item = (&'a LuciaValue, &'a LuciaValue);

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
    array: std::slice::IterMut<'a, (LuciaValue, LuciaValue)>,
    mapping: std::collections::hash_map::IterMut<'a, LuciaValue, LuciaValue>,
}

impl<'a> Iterator for IterMut<'a> {
    type Item = (&'a LuciaValue, &'a mut LuciaValue);

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
    array: std::vec::IntoIter<(LuciaValue, LuciaValue)>,
    mapping: std::collections::hash_map::IntoIter<LuciaValue, LuciaValue>,
}

impl Iterator for IntoIter {
    type Item = (LuciaValue, LuciaValue);

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
    type Item = &'a LuciaValue;

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
    type Item = &'a LuciaValue;

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
    type Item = &'a mut LuciaValue;

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
    type Item = LuciaValue;

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
    type Item = LuciaValue;

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
