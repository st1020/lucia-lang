//! Code optimizer.

use std::rc::Rc;

use ordermap::OrderMap;
use oxc_index::IndexVec;
use rustc_hash::FxBuildHasher;

use crate::compiler::ir::BasicBlock;

use super::{
    code::{Code, ConstValue},
    index::FunctionId,
    ir::FunctionIR,
};

pub fn optimize<S: Clone>(ir: IndexVec<FunctionId, FunctionIR<S>>) -> Code<S> {
    let mut codes = OrderMap::with_hasher(FxBuildHasher);
    for (function_id, function_ir) in ir.into_iter_enumerated().rev() {
        let code = optimize_function_ir(function_ir, &mut codes);
        codes.insert(function_id, code);
    }
    codes.remove(&FunctionId::new(0)).unwrap()
}

fn optimize_function_ir<S: Clone>(
    mut function_ir: FunctionIR<S>,
    codes: &mut OrderMap<FunctionId, Code<S>, FxBuildHasher>,
) -> Code<S> {
    let mut consts = Vec::new();
    let mut const_codes = Vec::new();
    for const_value in function_ir.consts {
        if let ConstValue::Function(id) = const_value {
            const_codes.push(Rc::new(
                codes.remove(&FunctionId::from(id)).unwrap().clone(),
            ));
            consts.push(ConstValue::Function(const_codes.len() - 1));
        } else {
            consts.push(const_value);
        }
    }

    for block in &mut function_ir.cfg.blocks {
        optimize_basic_block(block);
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

#[expect(unused)]
fn optimize_basic_block(block: &mut BasicBlock) {
    // TODO: implement optimizations
}
