use std::rc::Rc;

use derive_more::{Display, From};

use crate::{
    Context,
    compiler::{
        code::{CodeParamsInfo, EffectConst, UserEffect},
        value::MetaMethod,
    },
    objects::{CallbackFn, CallbackInner, CallbackReturn, Str, Value, impl_metamethod},
};

pub use crate::compiler::value::BuiltinEffect;

pub type Effect = Rc<EffectInner>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display, From)]
#[display("<effect {self:p}>")]
pub enum EffectInner {
    User(Rc<UserEffect<Str>>),
    Builtin(BuiltinEffect),
}

impl EffectInner {
    pub fn new(effect: EffectConst<Str>) -> Self {
        match effect {
            EffectConst::User(v) => EffectInner::User(v),
            EffectConst::Builtin(v) => EffectInner::Builtin(v),
        }
    }

    pub fn params_info(&self) -> CodeParamsInfo {
        match self {
            EffectInner::User(effect) => effect.params.info(),
            EffectInner::Builtin(
                BuiltinEffect::Error
                | BuiltinEffect::Panic
                | BuiltinEffect::Assert
                | BuiltinEffect::Yield,
            ) => CodeParamsInfo {
                params_count: 1,
                has_variadic: false,
            },
        }
    }

    pub(crate) fn match_effect_handler(&self, expected: &Effect) -> bool {
        let params_info = self.params_info();
        let expected_params_info = expected.params_info();
        (params_info.params_count + 1 == expected_params_info.params_count)
            && (params_info.has_variadic == expected_params_info.has_variadic)
    }
}

impl MetaMethod<&Context> for Effect {
    impl_metamethod!(Effect);

    #[inline]
    fn meta_call(self, _: &Context) -> Result<Self::ResultCall, Self::Error> {
        Ok(Rc::new(CallbackInner::new(self)).into())
    }

    impl_metamethod!(Effect, str);
    impl_metamethod!(Effect, repr);

    impl_metamethod!(Effect, eq_ne);
}

impl CallbackFn for Effect {
    fn call(&self, _: &Context, args: &[Value]) -> super::CallbackResult {
        Ok(CallbackReturn::TailEffect(Rc::clone(self), args.to_vec()))
    }
}

impl From<EffectInner> for Value {
    fn from(value: EffectInner) -> Self {
        Value::Effect(Rc::new(value))
    }
}

impl From<BuiltinEffect> for Value {
    fn from(value: BuiltinEffect) -> Self {
        Value::Effect(Rc::new(EffectInner::Builtin(value)))
    }
}
