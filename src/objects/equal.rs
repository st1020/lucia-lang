use crate::{
    objects::{Callback, Closure, Function, Str, Table, UserData, Value},
    utils::Float,
};

/// The `Equal` trait.
///
/// The `equal` method is used to compare two objects.
/// We use this because `PartialEq` and `Eq` must have same behavior whit `Hash`, but the `==` in
/// Lucia have different behavior.
pub trait Equal: Eq {
    fn equal(&self, other: &Self) -> bool {
        self.eq(other)
    }

    fn not_equal(&self, other: &Self) -> bool {
        !self.equal(other)
    }
}

macro_rules! impl_equal {
    ($($t:ty),* $(,)?) => {
        $(
            impl Equal for $t {}
        )*
    };
}
impl_equal!(
    bool,
    i64,
    Float,
    Str<'_>,
    Closure<'_>,
    Callback<'_>,
    Function<'_>,
    UserData<'_>,
);

impl Equal for Table<'_> {
    fn equal(&self, other: &Self) -> bool {
        if self.eq(other) {
            return true;
        }
        if self.len() != other.len() {
            return false;
        }
        for ((k, v), (k1, v1)) in self.iter().zip(other.iter()) {
            if !k.equal(&k1) || !v.equal(&v1) {
                return false;
            }
        }
        true
    }
}

impl Equal for Value<'_> {
    fn equal(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Null, Value::Null) => true,
            (Value::Bool(v), Value::Bool(v1)) => v.equal(v1),
            (Value::Int(v), Value::Int(v1)) => v.equal(v1),
            (Value::Float(v), Value::Float(v1)) => v.equal(v1),
            (Value::Str(v), Value::Str(v1)) => v.equal(v1),
            (Value::Table(v), Value::Table(v1)) => v.equal(v1),
            (Value::Function(v), Value::Function(v1)) => v.equal(v1),
            (Value::UserData(v), Value::UserData(v1)) => v.equal(v1),
            _ => false,
        }
    }
}
