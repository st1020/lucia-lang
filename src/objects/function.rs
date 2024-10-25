use gc_arena::Collect;

use crate::{
    objects::{Callback, Closure},
    utils::impl_enum_from,
};

/// Enum of lucia function (Closure / Callback).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect(no_drop)]
pub enum Function<'gc> {
    Closure(Closure<'gc>),
    Callback(Callback<'gc>),
}

impl_enum_from!(Function<'gc>, {
    Closure(Closure<'gc>),
    Callback(Callback<'gc>),
});
