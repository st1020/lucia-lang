use std::fmt;

use gc_arena::{Collect, Gc, Mutation, static_collect};

use crate::{
    compiler::{
        ast::FunctionKind,
        code::{Code, ConstValue},
        opcode::OpCode,
    },
    objects::{Bytes, Str, define_object},
    utils::Float,
};

static_collect!(OpCode);
static_collect!(FunctionKind);

define_object!(RuntimeCode, RuntimeCodeInner<'gc>, inner);

impl<'gc> RuntimeCode<'gc> {
    pub fn new(mc: &Mutation<'gc>, code: Code<Str<'gc>>) -> Self {
        RuntimeCode(Gc::new(
            mc,
            RuntimeCodeInner {
                name: code.name,
                params: code.params,
                variadic: code.variadic,
                kind: code.kind,
                code: code.code,
                consts: code
                    .consts
                    .into_iter()
                    .map(|v| RuntimeConstValue::new(mc, v))
                    .collect(),
                local_names: code.local_names,
                global_names: code.global_names,
                upvalue_names: code.upvalue_names,
                stack_size: code.stack_size,
            },
        ))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Collect)]
#[collect(no_drop)]
pub struct RuntimeCodeInner<'gc> {
    /// Function name.
    pub name: Option<Str<'gc>>,
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

impl fmt::Display for RuntimeCodeInner<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self, f)
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
    /// "b"abc""
    Bytes(Bytes<'gc>),
    /// A function code.
    Code(RuntimeCode<'gc>),
}

impl<'gc> RuntimeConstValue<'gc> {
    pub fn new(mc: &Mutation<'gc>, const_value: ConstValue<Str<'gc>>) -> Self {
        match const_value {
            ConstValue::Null => RuntimeConstValue::Null,
            ConstValue::Bool(v) => RuntimeConstValue::Bool(v),
            ConstValue::Int(v) => RuntimeConstValue::Int(v),
            ConstValue::Float(v) => RuntimeConstValue::Float(v),
            ConstValue::Str(v) => RuntimeConstValue::Str(v),
            ConstValue::Bytes(v) => RuntimeConstValue::Bytes(Bytes::new(mc, v)),
            ConstValue::Code(v) => RuntimeConstValue::Code(RuntimeCode::new(mc, *v)),
        }
    }
}
