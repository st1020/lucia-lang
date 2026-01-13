//! Code optimizer.

use std::{hash::Hash, rc::Rc};

use ordermap::{OrderMap, OrderSet};
use oxc_index::IndexVec;
use rustc_hash::FxBuildHasher;

use super::{
    code::{Code, ConstValue},
    index::{BasicBlockId, ConstCodeId, ConstId, FunctionId},
    ir::{BasicBlock, FunctionIR},
    opcode::OpCode,
};

pub fn optimize<S: Clone + Eq + Hash>(ir: IndexVec<FunctionId, FunctionIR<S>>) -> Code<S> {
    let mut codes = OrderMap::with_hasher(FxBuildHasher);
    for (function_id, function_ir) in ir.into_iter_enumerated().rev() {
        let code = optimize_function_ir(function_ir, &mut codes);
        codes.insert(function_id, code);
    }
    codes.remove(&FunctionId::new(0)).unwrap()
}

fn optimize_function_ir<S: Clone + Eq + Hash>(
    mut function_ir: FunctionIR<S>,
    codes: &mut OrderMap<FunctionId, Code<S>, FxBuildHasher>,
) -> Code<S> {
    for block in &mut function_ir.cfg.blocks {
        constant_fold(block, &mut function_ir.consts);
        eliminate_redundant_stack_ops(block);
    }

    let mut consts = IndexVec::with_capacity(function_ir.consts.len());
    let mut const_codes = IndexVec::new();
    for const_value in function_ir.consts {
        consts.push(match const_value {
            ConstValue::Null => ConstValue::Null,
            ConstValue::Bool(v) => ConstValue::Bool(v),
            ConstValue::Int(v) => ConstValue::Int(v),
            ConstValue::Float(v) => ConstValue::Float(v),
            ConstValue::Str(v) => ConstValue::Str(v),
            ConstValue::Bytes(v) => ConstValue::Bytes(v),
            ConstValue::Function(id) => {
                const_codes.push(Rc::new(codes.remove(&id).unwrap().clone()));
                ConstValue::Function(ConstCodeId::new(const_codes.len() - 1))
            }
            ConstValue::Effect(v) => ConstValue::Effect(v),
        });
    }
    Code {
        name: function_ir.name.clone(),
        params: function_ir.params.clone(),
        kind: function_ir.kind,
        code: function_ir.cfg.to_bytecode(),
        consts,
        const_codes,
        local_names: function_ir.local_names.into_values().collect(),
        global_names: function_ir.global_names.into_values().collect(),
        upvalue_names: function_ir.upvalue_names.into_values().collect(),
        stack_size: function_ir.cfg.max_stack_size,
    }
}

fn constant_fold<S: Clone + Eq + Hash>(
    block: &mut BasicBlock,
    consts: &mut OrderSet<ConstValue<S, FunctionId>, FxBuildHasher>,
) {
    let mut out = Vec::with_capacity(block.code.len());
    let mut stack = Vec::new();
    macro_rules! add_const {
        ($value:ident) => {
            ConstId::new(if let Some(index) = consts.get_index_of(&$value) {
                index
            } else {
                consts.insert($value);
                consts.len() - 1
            })
        };
    }
    for opcode in block.code.iter().copied() {
        #[expect(clippy::restriction)]
        match opcode {
            OpCode::LoadConst(i) => {
                stack.push(consts.get_index(i.index()).unwrap().clone());
                out.push(opcode);
            }
            OpCode::Neg | OpCode::Not | OpCode::TypeCheck(_) => {
                if let Some(v) = stack.pop()
                    && let Some(res) = eval_unary(opcode, &v)
                {
                    out.pop();
                    stack.push(res.clone());
                    out.push(OpCode::LoadConst(add_const!(res)));
                    continue;
                }
                stack.clear();
                out.push(opcode);
            }
            _ if opcode.is_arithmetic() || opcode.is_comparison() => {
                if let (Some(rhs), Some(lhs)) = (stack.pop(), stack.pop())
                    && let Some(res) = eval_binary(opcode, &lhs, &rhs)
                {
                    out.pop();
                    out.pop();
                    stack.push(res.clone());
                    out.push(OpCode::LoadConst(add_const!(res)));
                    continue;
                }
                stack.clear();
                out.push(opcode);
            }
            _ => {
                stack.clear();
                out.push(opcode);
            }
        }
    }
    block.code = out;
}

fn eval_unary<S>(
    opcode: OpCode<BasicBlockId>,
    v: &ConstValue<S, FunctionId>,
) -> Option<ConstValue<S, FunctionId>> {
    use ConstValue::{Bool, Float, Int};
    match (opcode, v) {
        (OpCode::Neg, Int(i)) => Some(Int(-i)),
        (OpCode::Neg, Float(f)) => Some(Float(-f)),

        (OpCode::Not, Bool(b)) => Some(Bool(!b)),

        (OpCode::TypeCheck(t), _) => Some(Bool(v.value_type() == t)),

        _ => None,
    }
}

fn eval_binary<S: Eq>(
    opcode: OpCode<BasicBlockId>,
    lhs: &ConstValue<S, FunctionId>,
    rhs: &ConstValue<S, FunctionId>,
) -> Option<ConstValue<S, FunctionId>> {
    use ConstValue::{Bool, Bytes, Float, Int, Null, Str};
    match (opcode, lhs, rhs) {
        (OpCode::Add, Int(lhs), Int(rhs)) => Some(Int(i64::wrapping_add(*lhs, *rhs))),
        (OpCode::Add, Float(lhs), Float(rhs)) => Some(Float(lhs + rhs)),

        (OpCode::Sub, Int(lhs), Int(rhs)) => Some(Int(i64::wrapping_sub(*lhs, *rhs))),
        (OpCode::Sub, Float(lhs), Float(rhs)) => Some(Float(lhs - rhs)),

        (OpCode::Mul, Int(lhs), Int(rhs)) => Some(Int(i64::wrapping_mul(*lhs, *rhs))),
        (OpCode::Mul, Float(lhs), Float(rhs)) => Some(Float(lhs * rhs)),

        (OpCode::Div, Int(lhs), Int(rhs)) => Some(Int(i64::wrapping_div(*lhs, *rhs))),
        (OpCode::Div, Float(lhs), Float(rhs)) => Some(Float(lhs / rhs)),

        (OpCode::Rem, Int(lhs), Int(rhs)) => Some(Int(i64::wrapping_rem(*lhs, *rhs))),
        (OpCode::Rem, Float(lhs), Float(rhs)) => Some(Float(lhs % rhs)),

        (OpCode::Eq, Null, Null) => Some(Bool(true)),
        (OpCode::Eq, Bool(lhs), Bool(rhs)) => Some(Bool(lhs == rhs)),
        (OpCode::Eq, Int(lhs), Int(rhs)) => Some(Bool(lhs == rhs)),
        (OpCode::Eq, Float(lhs), Float(rhs)) => Some(Bool(lhs == rhs)),
        (OpCode::Eq, Str(lhs), Str(rhs)) => Some(Bool(lhs == rhs)),
        (OpCode::Eq, Bytes(lhs), Bytes(rhs)) => Some(Bool(lhs == rhs)),

        (OpCode::Ne, Null, Null) => Some(Bool(false)),
        (OpCode::Ne, Bool(lhs), Bool(rhs)) => Some(Bool(lhs != rhs)),
        (OpCode::Ne, Int(lhs), Int(rhs)) => Some(Bool(lhs != rhs)),
        (OpCode::Ne, Float(lhs), Float(rhs)) => Some(Bool(lhs != rhs)),
        (OpCode::Ne, Str(lhs), Str(rhs)) => Some(Bool(lhs != rhs)),
        (OpCode::Ne, Bytes(lhs), Bytes(rhs)) => Some(Bool(lhs != rhs)),

        (OpCode::Gt, Int(lhs), Int(rhs)) => Some(Bool(lhs > rhs)),
        (OpCode::Gt, Float(lhs), Float(rhs)) => Some(Bool(lhs > rhs)),

        (OpCode::Ge, Int(lhs), Int(rhs)) => Some(Bool(lhs >= rhs)),
        (OpCode::Ge, Float(lhs), Float(rhs)) => Some(Bool(lhs >= rhs)),

        (OpCode::Lt, Int(lhs), Int(rhs)) => Some(Bool(lhs < rhs)),
        (OpCode::Lt, Float(lhs), Float(rhs)) => Some(Bool(lhs < rhs)),

        (OpCode::Le, Int(lhs), Int(rhs)) => Some(Bool(lhs <= rhs)),
        (OpCode::Le, Float(lhs), Float(rhs)) => Some(Bool(lhs <= rhs)),

        _ => None,
    }
}

fn eliminate_redundant_stack_ops(block: &mut BasicBlock) {
    let mut out = Vec::with_capacity(block.code.len());
    for opcode in block.code.iter().copied() {
        if opcode == OpCode::Pop
            && out
                .last()
                .is_some_and(|last| matches!(last, OpCode::Copy(_)) || last.is_load())
        {
            out.pop();
        } else {
            out.push(opcode);
        }
    }
    block.code = out;
}
