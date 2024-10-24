use std::ops::Deref;

use gc_arena::{static_collect, Collect, Gc, Mutation};

use crate::{
    compiler::{
        ast::FunctionKind,
        code::{Code, ConstValue},
        opcode::OpCode,
    },
    objects::Str,
    utils::Float,
};

static_collect!(OpCode);
static_collect!(FunctionKind);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect(no_drop)]
pub struct RuntimeCode<'gc>(Gc<'gc, RuntimeCodeInner<'gc>>);

impl<'gc> Deref for RuntimeCode<'gc> {
    type Target = RuntimeCodeInner<'gc>;

    fn deref(&self) -> &RuntimeCodeInner<'gc> {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Collect)]
#[collect(no_drop)]
pub struct RuntimeCodeInner<'gc> {
    /// Name of parameters.
    pub params: Vec<Str<'gc>>,
    /// Name of variadic parameter.
    pub variadic: Option<Str<'gc>>,
    /// Function kind.
    pub kind: FunctionKind,
    /// Bytecode, a list of OpCodes.
    pub code: Vec<OpCode>,

    /// List of constants used in the bytecode.
    pub consts: Vec<RuntimeConstValue<'gc>>,
    /// List of local names.
    pub local_names: Vec<Str<'gc>>,
    /// List of global names.
    pub global_names: Vec<Str<'gc>>,
    /// List of Upvalue information.
    pub upvalue_names: Vec<(Str<'gc>, Option<usize>)>,

    /// The required virtual machine stack space.
    pub stack_size: usize,
}

impl<'gc> RuntimeCode<'gc> {
    pub fn new(mc: &Mutation<'gc>, function: Code<Str<'gc>>) -> Self {
        RuntimeCode(Gc::new(
            mc,
            RuntimeCodeInner {
                params: function.params.into_iter().collect(),
                variadic: function.variadic,
                kind: function.kind,
                code: function.code,
                consts: function
                    .consts
                    .into_iter()
                    .map(|v| RuntimeConstValue::new(mc, v))
                    .collect(),
                local_names: function.local_names.into_iter().collect(),
                global_names: function.global_names.into_iter().collect(),
                upvalue_names: function.upvalue_names.into_iter().collect(),
                stack_size: function.stack_size,
            },
        ))
    }
}

/// The const value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect(no_drop)]
pub enum RuntimeConstValue<'gc> {
    /// "null"
    Null,
    /// "true", "false"
    Bool(bool),
    /// "12", "0o100", "0b110"
    Int(i64),
    /// "12.34", "0b100.100"
    Float(Float),
    /// ""abc"", ""abc"
    Str(Str<'gc>),
    /// A function.
    Func(RuntimeCode<'gc>),
}

impl<'gc> RuntimeConstValue<'gc> {
    pub fn new(mc: &Mutation<'gc>, const_value: ConstValue<Str<'gc>>) -> Self {
        match const_value {
            ConstValue::Null => RuntimeConstValue::Null,
            ConstValue::Bool(v) => RuntimeConstValue::Bool(v),
            ConstValue::Int(v) => RuntimeConstValue::Int(v),
            ConstValue::Float(v) => RuntimeConstValue::Float(v),
            ConstValue::Str(v) => RuntimeConstValue::Str(v),
            ConstValue::Func(v) => RuntimeConstValue::Func(RuntimeCode::new(mc, *v)),
        }
    }
}
