macro_rules! impl_enum_from {
    ($enum_name:ident<$lifetime:tt>, { $($variant:ident($type:ty)),* $(,)? }) => {
        $(
            impl<$lifetime> From<$type> for $enum_name<$lifetime> {
                fn from(value: $type) -> Self {
                    $enum_name::$variant(value)
                }
            }
        )*
    };

    ($enum_name:ident, { $($variant:ident($type:ty)),* $(,)? }) => {
        $(
            impl From<$type> for $enum_name {
                fn from(value: $type) -> Self {
                    $enum_name::$variant(value)
                }
            }
        )*
    };
}

pub(crate) use impl_enum_from;
