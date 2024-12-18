macro_rules! define_object {
    ($name:ident, $inner_name:ty) => {
        #[derive(Debug, Clone, Copy, Collect)]
        #[collect(no_drop)]
        pub struct $name<'gc>(Gc<'gc, $inner_name>);

        impl<'gc> $name<'gc> {
            pub fn from_inner(inner: Gc<'gc, $inner_name>) -> Self {
                Self(inner)
            }

            pub fn into_inner(self) -> Gc<'gc, $inner_name> {
                self.0
            }
        }

        impl<'gc> std::ops::Deref for $name<'gc> {
            type Target = $inner_name;

            fn deref(&self) -> &$inner_name {
                &self.0
            }
        }
    };

    ($name:ident, $inner_name:ty, inner) => {
        define_object!($name, $inner_name);

        impl<'gc> PartialEq for $name<'gc> {
            fn eq(&self, other: &Self) -> bool {
                Gc::ptr_eq(self.0, other.0) || self.0 == other.0
            }
        }

        impl<'gc> Eq for $name<'gc> {}

        impl<'gc> std::hash::Hash for $name<'gc> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                (*self.0).hash(state);
            }
        }

        impl std::fmt::Display for $name<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }
    };

    ($name:ident, $inner_name:ty, ptr, $display_name:literal) => {
        define_object!($name, $inner_name);

        impl<'gc> PartialEq for $name<'gc> {
            fn eq(&self, other: &Self) -> bool {
                Gc::ptr_eq(self.0, other.0)
            }
        }

        impl<'gc> Eq for $name<'gc> {}

        impl<'gc> std::hash::Hash for $name<'gc> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                Gc::as_ptr(self.0).hash(state);
            }
        }

        impl std::fmt::Display for $name<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "<{} {:p}>", $display_name, Gc::as_ptr(self.into_inner()))
            }
        }
    };
}

pub(crate) use define_object;
