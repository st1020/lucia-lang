//! The Lucia Standard Library.

mod builtin;
mod io;
mod string;
mod table;

pub use builtin::load_builtin;
pub use io::io_lib;
pub use string::string_lib;
pub use table::table_lib;

/// This macro is deprecated, and was instead by the IntoCallback trait.
/// But it takes me a lot of time to write it, so I don't want to remove it.
#[deprecated]
#[expect(unused_macros)]
macro_rules! check_args {
    (@AS_EXPR $e:expr) => {$e};

    (@ARGS_CHECK ($(,)?) -> ($($args:tt)*) ()) => {{
        $crate::check_args!(@AS_EXPR ($($args)*, Some($($args)*)))
    }};
    (@ARGS_CHECK (, *) -> ($($args:tt)*) ()) => {{
        ($crate::check_args!(@AS_EXPR ($($args)*)), None)
    }};
    (@ARGS_CHECK (, $arg:tt $($ty:tt)*) -> ($($args:tt)*) ()) => {{
        $crate::check_args!(@ARGS_CHECK ($($ty)*) -> ($($args)* + 1) ())
    }};
    (@ARGS_CHECK (| $($ty:tt)*) -> ($($args:tt)*) ()) => {{
        $crate::check_args!(
            @ARGS_CHECK (, $($ty)*) ->
            ($crate::check_args!(@AS_EXPR ($($args)*))) ($($args)*)
        )
    }};
    (@ARGS_CHECK ($(,)?) -> ($args:expr) ($($opt_args:tt)+)) => {{
        ($args, $crate::check_args!(@AS_EXPR Some($($opt_args)*)))
    }};
    (@ARGS_CHECK (, *) -> ($args:expr) ($($opt_args:tt)+)) => {{
        ($args, None)
    }};
    (@ARGS_CHECK (, $arg:tt $($ty:tt)*) -> ($args:expr) ($($opt_args:tt)+)) => {{
        $crate::check_args!(@ARGS_CHECK ($($ty)*) -> ($args) ($($opt_args)+ + 1))
    }};

    (@BUILD_TUPLE $args_iter:ident, $is_opt:tt ($(,)?) -> ($($body:tt)*)) => {{
        $crate::check_args!(@AS_EXPR ($($body)*))
    }};
    (@BUILD_TUPLE $args_iter:ident, $is_opt:tt (, *) -> ($($body:tt)*)) => {{
        $crate::check_args!(@AS_EXPR ($($body)* $args_iter.collect::<Vec<_>>()))
    }};
    (@BUILD_TUPLE $args_iter:ident, $is_opt:tt (| $($ty:tt)*) -> ($($body:tt)*)) => {{
        $crate::check_args!(
            @BUILD_TUPLE $args_iter, PARSE_ARG_OPT (, $($ty)*) -> ($($body)*)
        )
    }};
    (@BUILD_TUPLE $args_iter:ident, $is_opt:tt (, $arg:tt $($ty:tt)*) -> ($($body:tt)*)) => {{
        $crate::check_args!(
            @BUILD_TUPLE $args_iter, $is_opt ($($ty)*) ->
            ($($body)* $crate::check_args!(@$is_opt $args_iter, $arg),)
        )
    }};

    (@PARSE_ARG $args_iter:ident, Value) => {{
        $args_iter.next().unwrap()
    }};
    (@PARSE_ARG_OPT $args_iter:ident, Value) => {{
        $args_iter.next()
    }};
    (@PARSE_ARG $args_iter:ident, $ty:tt) => {{
        let t = $args_iter.next().unwrap();
        $crate::check_args!(@AS_VALUE_TYPE t, $ty)
    }};
    (@PARSE_ARG_OPT $args_iter:ident, $ty:tt) => {{
        match $args_iter.next() {
            Some(v) => Some($crate::check_args!(@AS_VALUE_TYPE v, $ty)),
            None => None,
        }
    }};

    (@AS_VALUE_TYPE $value:ident, $ty:tt) => {{
        if let Value::$ty(v) = $value {
            v
        } else {
            return Err($crate::errors::Error::new(
                $crate::errors::ErrorKind::UnexpectedType {
                    expected: $crate::objects::ValueType::$ty,
                    found: $value.value_type(),
                }
            ));
        }
    }};

    ($args:ident $(,)?) => {{
        let args_len = $args.len();
        let required = $crate::errors::CallArgumentsErrorKind::from(0);
        if !required.contains(&args_len) {
            return Err($crate::errors::Error::new(
                $crate::errors::ErrorKind::CallArguments {
                    value: None,
                    required,
                    given: args_len,
                }
            ));
        }
    }};
    ($args:ident $($cont:tt)*) => {{
        let args_len = $args.len();
        let required = $crate::errors::CallArgumentsErrorKind::from(
            $crate::check_args!(@ARGS_CHECK ($($cont)*) -> (0) ()),
        );
        if !required.contains(&args_len) {
            return Err($crate::errors::Error::new(
                $crate::errors::ErrorKind::CallArguments {
                    value: None,
                    required,
                    given: args_len,
                }
            ));
        }
        let mut args_iter = $args.iter().copied();
        $crate::check_args!(@BUILD_TUPLE args_iter, PARSE_ARG ($($cont)*) -> ())
    }};
}
