use std::rc::Rc;

use derive_more::{Display, From};

use crate::{
    Context,
    compiler::{
        code::{CodeParamsInfo, EffectConst, UserEffect},
        value::MetaMethod,
    },
    objects::{Callback, CallbackFn, CallbackReturn, RcStr, Value, impl_metamethod},
};

pub use crate::compiler::value::BuiltinEffect;

pub type RcEffect = Rc<Effect>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display, From)]
#[display("<effect {self:p}>")]
pub enum Effect {
    User(Rc<UserEffect<RcStr>>),
    Builtin(BuiltinEffect),
}

impl Effect {
    pub fn new(effect: EffectConst<RcStr>) -> Self {
        match effect {
            EffectConst::User(v) => Effect::User(v),
            EffectConst::Builtin(v) => Effect::Builtin(v),
        }
    }

    pub fn params_info(&self) -> CodeParamsInfo {
        match self {
            Effect::User(effect) => effect.params.info(),
            Effect::Builtin(BuiltinEffect::Yield | BuiltinEffect::Error | BuiltinEffect::Panic) => {
                CodeParamsInfo {
                    params_count: 1,
                    has_variadic: false,
                }
            }
        }
    }

    pub(crate) fn match_effect_handler(&self, expected: &RcEffect) -> bool {
        let params_info = self.params_info();
        let expected_params_info = expected.params_info();
        (params_info.params_count + 1 == expected_params_info.params_count)
            && (params_info.has_variadic == expected_params_info.has_variadic)
    }
}

impl MetaMethod<&Context> for RcEffect {
    impl_metamethod!(Effect);

    #[inline]
    fn meta_call(self, _ctx: &Context) -> Result<Self::ResultCall, Self::Error> {
        Ok(Rc::new(Callback::new(self)).into())
    }

    impl_metamethod!(Effect, str);
    impl_metamethod!(Effect, repr);

    impl_metamethod!(Effect, eq_ne);
}

impl CallbackFn for RcEffect {
    fn call(&mut self, _ctx: &Context, args: &[Value]) -> super::CallbackResult {
        Ok(CallbackReturn::TailPerform {
            effect: Rc::clone(self),
            args: args.to_vec(),
        })
    }
}

impl From<Effect> for Value {
    fn from(value: Effect) -> Self {
        Value::Effect(Rc::new(value))
    }
}

impl From<BuiltinEffect> for Value {
    fn from(value: BuiltinEffect) -> Self {
        Value::Effect(Rc::new(Effect::Builtin(value)))
    }
}
