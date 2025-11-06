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
                &self.into_inner().as_ref()
            }
        }

        impl<'gc> AsRef<$inner_name> for $name<'gc> {
            fn as_ref(&self) -> &$inner_name {
                self.into_inner().as_ref()
            }
        }

        impl<'gc> std::borrow::Borrow<$inner_name> for $name<'gc> {
            fn borrow(&self) -> &$inner_name {
                self.into_inner().as_ref()
            }
        }
    };

    ($name:ident, $inner_name:ty, inner $(,)? $($ty:ty),* $(,)?) => {
        define_object!($name, $inner_name);

        impl<'gc> PartialEq for $name<'gc> {
            fn eq(&self, other: &Self) -> bool {
                Gc::ptr_eq(self.into_inner(), other.into_inner())
                    || self.into_inner() == other.into_inner()
            }
        }

        impl<'gc> Eq for $name<'gc> {}

        impl<'gc> std::hash::Hash for $name<'gc> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.into_inner().hash(state);
            }
        }

        impl std::fmt::Display for $name<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.into_inner().fmt(f)
            }
        }

        $(
            impl<'gc> AsRef<$ty> for $name<'gc> {
                fn as_ref(&self) -> &$ty {
                    self.into_inner().as_ref().as_ref()
                }
            }
        )*
    };

    ($name:ident, $inner_name:ty, $wrapped_ty:ty, inner $(,)? $($ty:ty),*) => {
        define_object!($name, $inner_name, inner, $wrapped_ty, $($ty),*);

        impl<'gc> std::borrow::Borrow<$wrapped_ty> for $name<'gc> {
            fn borrow(&self) -> &$wrapped_ty {
                self.into_inner().as_ref()
            }
        }
    };

    ($name:ident, $inner_name:ty, ptr, $display_name:literal) => {
        define_object!($name, $inner_name);

        impl<'gc> PartialEq for $name<'gc> {
            fn eq(&self, other: &Self) -> bool {
                Gc::ptr_eq(self.into_inner(), other.into_inner())
            }
        }

        impl<'gc> Eq for $name<'gc> {}

        impl<'gc> std::hash::Hash for $name<'gc> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                Gc::as_ptr(self.into_inner()).hash(state);
            }
        }

        impl std::fmt::Display for $name<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "<{} {:p}>", $display_name, Gc::as_ptr(self.into_inner()))
            }
        }
    };
}

macro_rules! impl_metamethod {
    ($type:ident) => {
        type Value = $crate::objects::Value<'gc>;
        type Error = $crate::errors::Error<'gc>;
        type ResultCall = $crate::objects::Function<'gc>;
        type ResultIter = Self::ResultCall;
        type Result1 = $crate::objects::MetaResult<'gc, 1>;
        type Result2 = $crate::objects::MetaResult<'gc, 2>;
        type Result3 = $crate::objects::MetaResult<'gc, 3>;

        fn meta_error(
            &self,
            _: Context<'gc>,
            operator: $crate::compiler::value::MetaName,
            args: Vec<Self::Value>,
        ) -> Self::Error {
            if args.len() == 0 {
                $crate::errors::Error::new($crate::errors::RuntimeError::MetaUnOperator {
                    operator,
                    operand: $crate::objects::ValueType::$type,
                })
            } else {
                $crate::errors::Error::new($crate::errors::RuntimeError::MetaBinOperator {
                    operator,
                    operand: ($crate::objects::ValueType::$type, args[0].value_type()),
                })
            }
        }
    };
    ($type:ident, str) => {
        fn meta_str(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
            Ok(compact_str::ToCompactString::to_compact_string(self).into_meta_result(ctx))
        }
    };
    ($type:ident, repr) => {
        fn meta_repr(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
            self.meta_str(ctx)
        }
    };
    ($type:ident, repr) => {
        fn meta_repr(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
            self.str(ctx)
        }
    };
    ($type:ident, neg) => {
        fn meta_neg(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
            Ok((-*self).into_meta_result(ctx))
        }
    };
    ($type:ident, arithmetic, $operator:ident, $name:ident, $fn_name:ident) => {
        fn $name(
            &self,
            ctx: Context<'gc>,
            other: Self::Value,
        ) -> Result<Self::Result2, Self::Error> {
            if let $crate::objects::Value::$type(other) = other {
                Ok(self.$fn_name(other).into_meta_result(ctx))
            } else {
                Err(self.meta_error(
                    ctx,
                    $crate::compiler::value::MetaName::$operator,
                    vec![other],
                ))
            }
        }
    };
    ($value:ident, eq_ne) => {
        fn meta_eq(
            &self,
            ctx: Context<'gc>,
            other: Self::Value,
        ) -> Result<Self::Result2, Self::Error> {
            if let $crate::objects::Value::$value(v) = &other {
                Ok((self == v).into_meta_result(ctx))
            } else {
                Ok(false.into_meta_result(ctx))
            }
        }
        fn meta_ne(
            &self,
            ctx: Context<'gc>,
            other: Self::Value,
        ) -> Result<Self::Result2, Self::Error> {
            if let $crate::objects::Value::$value(v) = &other {
                Ok((self != v).into_meta_result(ctx))
            } else {
                Ok(true.into_meta_result(ctx))
            }
        }
    };
    ($type:ident, compare, $operator:ident, $name:ident, $fn_name:ident) => {
        fn $name(
            &self,
            ctx: Context<'gc>,
            other: Self::Value,
        ) -> Result<Self::Result2, Self::Error> {
            if let $crate::objects::Value::$type(other) = &other {
                Ok(self.$fn_name(other).into_meta_result(ctx))
            } else {
                Err(self.meta_error(
                    ctx,
                    $crate::compiler::value::MetaName::$operator,
                    vec![other],
                ))
            }
        }
    };
}

macro_rules! call_metamethod {
    ($ctx:ident, $meta_name:expr, $self:ident $(,)? $($arg:ident),*) => {
        if let Some(metatable) = $self.metatable() {
            let metamethod = metatable.get($ctx, $meta_name);
            if !metamethod.is_null() {
                return Ok($crate::objects::MetaResult::Call(
                    metamethod.meta_call($ctx)?,
                    [(*$self).into(), $($arg.into()),*],
                ));
            }
        }
    };
}

macro_rules! call_metamethod_error {
    (1, $name:ident, $meta_name:ident) => {
        fn $name(&self, ctx: Context<'gc>) -> Result<Self::Result1, Self::Error> {
            $crate::objects::call_metamethod!(
                ctx,
                $crate::compiler::value::MetaName::$meta_name,
                self
            );
            Err(self.meta_error(ctx, $crate::compiler::value::MetaName::$meta_name, vec![]))
        }
    };
    (2, $name:ident, $meta_name:ident) => {
        fn $name(
            &self,
            ctx: Context<'gc>,
            other: Self::Value,
        ) -> Result<Self::Result2, Self::Error> {
            $crate::objects::call_metamethod!(
                ctx,
                $crate::compiler::value::MetaName::$meta_name,
                self,
                other
            );
            Err(self.meta_error(
                ctx,
                $crate::compiler::value::MetaName::$meta_name,
                vec![other],
            ))
        }
    };
    (3, $name:ident, $meta_name:ident) => {
        fn $name(
            &self,
            ctx: Context<'gc>,
            key: Self::Value,
            value: Self::Value,
        ) -> Result<Self::Result3, Self::Error> {
            $crate::objects::call_metamethod!(
                ctx,
                $crate::compiler::value::MetaName::$meta_name,
                self,
                key,
                value
            );
            Err(self.meta_error(
                ctx,
                $crate::compiler::value::MetaName::$meta_name,
                vec![key, value],
            ))
        }
    };
}

pub(crate) use call_metamethod;
pub(crate) use call_metamethod_error;
pub(crate) use define_object;
pub(crate) use impl_metamethod;
