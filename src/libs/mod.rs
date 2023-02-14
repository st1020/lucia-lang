pub mod builtin;
pub mod io;
pub mod string;
pub mod table;

use std::collections::HashMap;

use crate::lvm::Lvm;
use crate::objects::Value;

#[macro_export]
macro_rules! check_args {
    (@AS_EXPR $e:expr) => {$e};

    (@ARGS_ITER $args:ident ()) => {{
        $args.iter()
    }};
    (@ARGS_ITER $args:ident (mut $($x:tt)*)) => {{
        $args.iter_mut()
    }};
    (@ARGS_ITER $args:ident ($_:tt $($x:tt)*)) => {{
        $crate::check_args!(@ARGS_ITER $args ($($x)*))
    }};

    (@ARGS_CHECK ($(,)?) -> ($($args:tt)*) ()) => {{
        $crate::check_args!(@AS_EXPR ($($args)*, Some($($args)*)))
    }};
    (@ARGS_CHECK (, *) -> ($($args:tt)*) ()) => {{
        ($crate::check_args!(@AS_EXPR ($($args)*)), None)
    }};
    (@ARGS_CHECK (, mut $arg:tt $($ty:tt)*) -> ($($args:tt)*) ()) => {{
        $crate::check_args!(@ARGS_CHECK ($($ty)*) -> ($($args)* + 1) ())
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
    (@ARGS_CHECK (, mut $arg:tt $($ty:tt)*) -> ($args:expr) ($($opt_args:tt)+)) => {{
        $crate::check_args!(@ARGS_CHECK ($($ty)*) -> ($args) ($($opt_args)+ + 1))
    }};
    (@ARGS_CHECK (, $arg:tt $($ty:tt)*) -> ($args:expr) ($($opt_args:tt)+)) => {{
        $crate::check_args!(@ARGS_CHECK ($($ty)*) -> ($args) ($($opt_args)+ + 1))
    }};


    (@BUILD_TUPLE $lvm:ident, $args_iter:ident, $is_opt:tt ($(,)?) -> ($($body:tt)*)) => {{
        $crate::check_args!(@AS_EXPR ($($body)*))
    }};
    (@BUILD_TUPLE $lvm:ident, $args_iter:ident, $is_opt:tt (, *) -> ($($body:tt)*)) => {{
        $crate::check_args!(@AS_EXPR ($($body)* $args_iter.collect::<Vec<_>>()))
    }};
    (@BUILD_TUPLE $lvm:ident, $args_iter:ident, $is_opt:tt (| $($ty:tt)*) -> ($($body:tt)*)) => {{
        $crate::check_args!(
            @BUILD_TUPLE $lvm, $args_iter, PARSE_ARG_OPT (, $($ty)*) -> ($($body)*)
        )
    }};
    (@BUILD_TUPLE $lvm:ident, $args_iter:ident, $is_opt:tt (, mut $arg:tt $($ty:tt)*) -> ($($body:tt)*)) => {{
        $crate::check_args!(
            @BUILD_TUPLE $lvm, $args_iter, $is_opt ($($ty)*) ->
            ($($body)* $crate::check_args!(@$is_opt $lvm, $args_iter, $arg, mut),)
        )
    }};
    (@BUILD_TUPLE $lvm:ident, $args_iter:ident, $is_opt:tt (, $arg:tt $($ty:tt)*) -> ($($body:tt)*)) => {{
        $crate::check_args!(
            @BUILD_TUPLE $lvm, $args_iter, $is_opt ($($ty)*) ->
            ($($body)* $crate::check_args!(@$is_opt $lvm, $args_iter, $arg),)
        )
    }};

    (@PARSE_ARG $lvm:ident, $args_iter:ident, Value) => {{
        $args_iter.next().copied().unwrap()
    }};
    (@PARSE_ARG_OPT $lvm:ident, $args_iter:ident, Value) => {{
        $args_iter.next().copied()
    }};
    (@PARSE_ARG $lvm:ident, $args_iter:ident, $ty:tt $(, $is_mut:tt)?) => {{
        let t = $args_iter.next().unwrap();
        $crate::try_as_value_type!($lvm, t, $ty $(, $is_mut)?)
    }};
    (@PARSE_ARG_OPT $lvm:ident, $args_iter:ident, $ty:tt) => {{
        match $args_iter.next() {
            Some(v) => Some($crate::try_as_value_type!($lvm, v, $ty)),
            None => None,
        }
    }};
    (@PARSE_ARG_OPT $lvm:ident, $args_iter:ident, $ty:tt, mut) => {{
        match $args_iter.next() {
            Some(v) => Some($crate::try_as_value_type!($lvm, v, $ty, mut)),
            None => None,
        }
    }};

    ($lvm:ident, $args:ident $(,)?) => {{
        let args_len = $args.len();
        let required = $crate::errors::CallArgumentsErrorKind::from(0);
        println!("{:?}", required);
        if !required.contains(&args_len) {
            $crate::return_error!($lvm, $crate::call_arguments_error!(None, required, args_len));
        }
    }};
    ($lvm:ident, $args:ident $($cont:tt)*) => {{
        let args_len = $args.len();
        let required = $crate::errors::CallArgumentsErrorKind::from(
            $crate::check_args!(@ARGS_CHECK ($($cont)*) -> (0) ()),
        );
        if !required.contains(&args_len) {
            $crate::return_error!($lvm, $crate::call_arguments_error!(None, required, args_len));
        }
        let mut args_iter = $crate::check_args!(@ARGS_ITER $args ($($cont)*));
        $crate::check_args!(@BUILD_TUPLE $lvm, args_iter, PARSE_ARG ($($cont)*) -> ())
    }};
}

pub fn std_libs(lvm: &mut Lvm) -> HashMap<String, Value> {
    let mut std_libs = HashMap::new();
    macro_rules! add_std_module {
        ($name:expr, $path:path) => {
            let t = $path(lvm);
            std_libs.insert(String::from($name), lvm.new_table_value(t));
        };
    }
    add_std_module!("std::io", io::libs);
    add_std_module!("std::string", string::libs);
    add_std_module!("std::table", table::libs);
    std_libs
}
