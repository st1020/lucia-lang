#![expect(clippy::arbitrary_source_item_ordering)]

macro_rules! impl_metamethod {
    ($type:ident) => {
        type Value = $crate::objects::Value;
        type Error = $crate::errors::Error;
        type ResultCall = $crate::objects::Function;
        type ResultIter = Self::ResultCall;
        type Result1 = $crate::objects::MetaResult<1>;
        type Result2 = $crate::objects::MetaResult<2>;
        type Result3 = $crate::objects::MetaResult<3>;

        #[inline]
        fn meta_error(
            self,
            _: &Context,
            operator: $crate::compiler::value::MetaName,
            args: Vec<Self::Value>,
        ) -> Self::Error {
            if args.len() == 0 {
                $crate::errors::Error::new($crate::errors::ErrorKind::MetaUnOperator {
                    operator,
                    operand: $crate::objects::ValueType::$type,
                })
            } else {
                $crate::errors::Error::new($crate::errors::ErrorKind::MetaBinOperator {
                    operator,
                    operand: ($crate::objects::ValueType::$type, args[0].value_type()),
                })
            }
        }
    };
    ($type:ident, str) => {
        #[inline]
        fn meta_str(self, _: &Context) -> Result<Self::Result1, Self::Error> {
            Ok(compact_str::ToCompactString::to_compact_string(&self).into())
        }
    };
    ($type:ident, repr) => {
        #[inline]
        fn meta_repr(self, ctx: &Context) -> Result<Self::Result1, Self::Error> {
            self.meta_str(ctx)
        }
    };
    ($type:ident, neg) => {
        #[inline]
        fn meta_neg(self, _: &Context) -> Result<Self::Result1, Self::Error> {
            Ok((-self).into())
        }
    };
    ($type:ident, arithmetic, $operator:ident, $name:ident, $fn_name:ident) => {
        #[inline]
        fn $name(self, ctx: &Context, other: Self::Value) -> Result<Self::Result2, Self::Error> {
            if let $crate::objects::Value::$type(other) = other {
                Ok(self.$fn_name(other).into())
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
        #[inline]
        fn meta_eq(self, _: &Context, other: Self::Value) -> Result<Self::Result2, Self::Error> {
            if let $crate::objects::Value::$value(v) = other {
                Ok((self == v).into())
            } else {
                Ok(false.into())
            }
        }
        #[inline]
        fn meta_ne(self, _: &Context, other: Self::Value) -> Result<Self::Result2, Self::Error> {
            if let $crate::objects::Value::$value(v) = other {
                Ok((self != v).into())
            } else {
                Ok(true.into())
            }
        }
    };
    ($type:ident, compare, $operator:ident, $name:ident, $fn_name:ident) => {
        #[inline]
        fn $name(self, ctx: &Context, other: Self::Value) -> Result<Self::Result2, Self::Error> {
            if let $crate::objects::Value::$type(other) = &other {
                Ok(self.$fn_name(other).into())
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
            let metamethod = metatable.get($meta_name);
            if !metamethod.is_null() {
                return Ok($crate::objects::MetaResult::Call(
                    metamethod.meta_call($ctx)?,
                    [$self.clone().into(), $($arg.into()),*],
                ));
            }
        }
    };
}

macro_rules! call_metamethod_error {
    (1, $name:ident, $meta_name:ident) => {
        #[inline]
        fn $name(self, ctx: &Context) -> Result<Self::Result1, Self::Error> {
            $crate::objects::call_metamethod!(
                ctx,
                $crate::compiler::value::MetaName::$meta_name,
                self
            );
            Err(self.meta_error(ctx, $crate::compiler::value::MetaName::$meta_name, vec![]))
        }
    };
    (2, $name:ident, $meta_name:ident) => {
        #[inline]
        fn $name(self, ctx: &Context, other: Self::Value) -> Result<Self::Result2, Self::Error> {
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
        #[inline]
        fn $name(
            self,
            ctx: &Context,
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

macro_rules! unexpected_type_error {
    ($expected:expr, $found:expr) => {
        $crate::errors::Error::new($crate::errors::ErrorKind::UnexpectedType {
            expected: $expected,
            found: $found.value_type(),
        })
    };
}

pub(crate) use call_metamethod;
pub(crate) use call_metamethod_error;
pub(crate) use impl_metamethod;
pub(crate) use unexpected_type_error;
