//! Control Flow Graph (CFG).

use std::{fmt, iter};

use itertools::Itertools;
use ordermap::{OrderMap, OrderSet};
use oxc_index::IndexVec;
use rustc_hash::FxBuildHasher;

use super::{
    code::{CodeParams, ConstValue, FunctionKind, UpvalueCapture},
    codegen::CodeMarker,
    index::{BasicBlockId, SymbolId},
    opcode::{JumpTarget, OpCode},
};

#[derive(Debug, Clone)]
pub struct FunctionIR<S> {
    pub name: Option<S>,
    pub params: CodeParams<S>,
    pub kind: FunctionKind,
    pub cfg: ControlFlowGraph,
    pub consts: OrderSet<ConstValue<S>, FxBuildHasher>,
    pub local_names: OrderMap<SymbolId, S, FxBuildHasher>,
    pub global_names: OrderMap<SymbolId, S, FxBuildHasher>,
    pub upvalue_names: OrderMap<SymbolId, (S, UpvalueCapture), FxBuildHasher>,
}

#[derive(Debug, Clone, Default)]
pub struct ControlFlowGraph {
    pub blocks: IndexVec<BasicBlockId, BasicBlock>,
    pub max_stack_size: usize,
}

impl ControlFlowGraph {
    pub(crate) fn from_bytecode(
        code: &[CodeMarker],
        jump_target_count: usize,
        init_stack_size: usize,
    ) -> Self {
        let mut cfg = ControlFlowGraph::default();

        // Determine which jump targets are used
        let mut jump_target_used = vec![false; jump_target_count];
        for marker in code.iter().copied() {
            if let CodeMarker::OpCode(opcode) = marker
                && let Some(JumpTarget(index)) = opcode.jump_target()
            {
                jump_target_used[index] = true;
            }
        }

        // Build basic blocks
        let mut jump_target_to_basic_block_id = vec![BasicBlockId::new(0); jump_target_count];
        let mut current_block_id = cfg.blocks.push(BasicBlock::default());
        macro_rules! push_basic_block {
            () => {
                if cfg.blocks[current_block_id].code.is_empty() {
                    current_block_id
                } else {
                    let next_block_id = cfg.blocks.push(BasicBlock::default());
                    cfg.blocks[current_block_id].next_block = Some(next_block_id);
                    current_block_id = next_block_id;
                    next_block_id
                }
            };
        }
        for marker in code.iter().copied() {
            match marker {
                CodeMarker::OpCode(opcode) => {
                    cfg.blocks[current_block_id].code.push(opcode);
                    if opcode.is_jump() || opcode.is_return() {
                        push_basic_block!();
                    }
                }
                CodeMarker::JumpTarget(JumpTarget(i)) => {
                    if jump_target_used[i] {
                        jump_target_to_basic_block_id[i] = push_basic_block!();
                    }
                }
            }
        }

        // Set jump target and next block for each basic block
        for block in &mut cfg.blocks.iter_mut() {
            for opcode in &mut block.code {
                if let Some(JumpTarget(i)) = opcode.jump_target_mut() {
                    *i = jump_target_to_basic_block_id[*i].into();
                }
                if let OpCode::Jump(JumpTarget(i)) = opcode {
                    // Only set next block to jump target for unconditional jumps
                    // JumpBackEdge/Break/Continue should be ignored here
                    block.next_block = Some(BasicBlockId::new(*i));
                }
                if opcode.is_return() {
                    block.next_block = None;
                }
            }
        }

        // Data flow analysis to determine stack size at each basic block
        let mut block_stack = vec![BasicBlockId::new(0)];
        cfg.blocks[BasicBlockId::new(0)].start_stack_size = init_stack_size;
        cfg.blocks[BasicBlockId::new(0)].reachable = true;
        while let Some(block_id) = block_stack.pop() {
            let current_block = &cfg.blocks[block_id];

            let mut handler_block_stack: Vec<(BasicBlockId, Option<usize>)> = Vec::new();
            let mut next_stack_size = current_block.start_stack_size;
            for opcode in current_block.code.iter().copied() {
                #[expect(clippy::match_same_arms)]
                match opcode {
                    OpCode::Pop => next_stack_size -= 1,
                    OpCode::Copy(_) => next_stack_size += 1,
                    OpCode::Swap(_) => (),
                    OpCode::LoadLocal(_)
                    | OpCode::LoadGlobal(_)
                    | OpCode::LoadUpvalue(_)
                    | OpCode::LoadConst(_) => next_stack_size += 1,
                    OpCode::StoreLocal(_) | OpCode::StoreGlobal(_) => next_stack_size -= 1,
                    OpCode::BuildTable(i) => next_stack_size = next_stack_size - i * 2 + 1,
                    OpCode::BuildList(i) => next_stack_size = next_stack_size - i + 1,
                    OpCode::GetAttr | OpCode::GetItem => next_stack_size -= 1,
                    OpCode::GetMeta => (),
                    OpCode::SetAttr | OpCode::SetItem => next_stack_size -= 2,
                    OpCode::SetMeta => next_stack_size -= 1,
                    OpCode::Neg | OpCode::Not => (),
                    OpCode::Add
                    | OpCode::Sub
                    | OpCode::Mul
                    | OpCode::Div
                    | OpCode::Rem
                    | OpCode::Eq
                    | OpCode::Ne
                    | OpCode::Gt
                    | OpCode::Ge
                    | OpCode::Lt
                    | OpCode::Le => next_stack_size -= 1,
                    OpCode::TypeCheck(_) => (),
                    OpCode::GetLen => (),
                    OpCode::Import(_) => next_stack_size += 1,
                    OpCode::ImportFrom(_) => next_stack_size += 1,
                    OpCode::ImportGlob => next_stack_size -= 1,
                    OpCode::Iter => (),
                    OpCode::Call(i) => {
                        next_stack_size -= i;
                        for (_, handler_stack_size) in &mut handler_block_stack {
                            if handler_stack_size.is_none() {
                                *handler_stack_size = Some(next_stack_size - 1);
                            }
                        }
                    }
                    OpCode::Return => next_stack_size -= 1,
                    OpCode::ReturnCall(i) => next_stack_size -= i + 1,
                    OpCode::LoadLocals => next_stack_size += 1,
                    OpCode::Jump(_)
                    | OpCode::JumpBackEdge(_)
                    | OpCode::Break(_)
                    | OpCode::Continue(_) => (),
                    OpCode::JumpPopIfNull(_) => (),
                    OpCode::PopJumpIfTrue(_) | OpCode::PopJumpIfFalse(_) => next_stack_size -= 1,
                    OpCode::JumpIfTrueOrPop(_) | OpCode::JumpIfFalseOrPop(_) => {
                        next_stack_size -= 1;
                    }
                    OpCode::RegisterHandler(JumpTarget(i)) => {
                        next_stack_size -= 1;
                        handler_block_stack.push((BasicBlockId::new(i), None));
                    }
                    OpCode::CheckEffect(i) => next_stack_size = next_stack_size + i - 1,
                    OpCode::MarkAddStackSize(i) => next_stack_size += i,
                }
                cfg.max_stack_size = cfg.max_stack_size.max(next_stack_size);
            }

            #[expect(clippy::match_same_arms)]
            let jump_target = match current_block.code.last() {
                Some(OpCode::Jump(_)) => None, // Handled by next_block
                Some(OpCode::JumpBackEdge(JumpTarget(i))) => {
                    Some((BasicBlockId::new(*i), next_stack_size))
                }
                Some(OpCode::Break(_) | OpCode::Continue(_)) => None, // Handled later
                Some(OpCode::JumpPopIfNull(JumpTarget(i))) => {
                    Some((BasicBlockId::new(*i), next_stack_size - 1))
                }
                Some(
                    OpCode::PopJumpIfTrue(JumpTarget(i)) | OpCode::PopJumpIfFalse(JumpTarget(i)),
                ) => Some((BasicBlockId::new(*i), next_stack_size)),
                Some(
                    OpCode::JumpIfTrueOrPop(JumpTarget(i))
                    | OpCode::JumpIfFalseOrPop(JumpTarget(i)),
                ) => Some((BasicBlockId::new(*i), next_stack_size + 1)),
                _ => None,
            };

            macro_rules! push_block_stack {
                ($block_id:expr, $stack_size:expr) => {
                    let target_block = &mut cfg.blocks[$block_id];
                    if target_block.reachable {
                        debug_assert_eq!(target_block.start_stack_size, $stack_size);
                    } else {
                        target_block.start_stack_size = $stack_size;
                        target_block.reachable = true;
                        block_stack.push($block_id);
                    }
                };
            }
            if let Some(next_block_id) = current_block.next_block {
                push_block_stack!(next_block_id, next_stack_size);
            }
            if let Some((target_block_id, target_stack_size)) = jump_target {
                push_block_stack!(target_block_id, target_stack_size);
            }
            for (handler_block_id, handler_stack_size) in handler_block_stack {
                push_block_stack!(handler_block_id, handler_stack_size.unwrap());
            }
            cfg.blocks[block_id].end_stack_size = next_stack_size;
        }

        // Add clear stack ops before break/continue/return
        for block_id in cfg.blocks.indices().clone() {
            let current_end_stack_size = cfg.blocks[block_id].end_stack_size;
            match cfg.blocks[block_id].code.last().copied() {
                Some(OpCode::Break(JumpTarget(i)) | OpCode::Continue(JumpTarget(i))) => {
                    let target_start_stack_size = cfg.blocks[BasicBlockId::new(i)].start_stack_size;
                    if current_end_stack_size != target_start_stack_size {
                        let opcode = cfg.blocks[block_id].code.pop().unwrap();
                        cfg.blocks[block_id].code.extend(iter::repeat_n(
                            OpCode::Pop,
                            current_end_stack_size - target_start_stack_size,
                        ));
                        cfg.blocks[block_id].code.push(opcode);
                    }
                }
                Some(OpCode::Return) => {
                    if current_end_stack_size != 0 {
                        let mut new_code = vec![OpCode::Pop; current_end_stack_size];
                        new_code.extend(&cfg.blocks[block_id].code);
                        cfg.blocks[block_id].code = new_code;
                        cfg.blocks[block_id].end_stack_size = 0;
                    }
                }
                _ => (),
            }
        }

        cfg
    }

    pub(crate) fn to_bytecode(&self) -> Vec<OpCode> {
        let mut code = Vec::new();
        let mut jump_target_table = Vec::with_capacity(self.blocks.len());
        for (current_block_id, block) in self.blocks.iter_enumerated() {
            jump_target_table.push(code.len());
            if block.reachable {
                code.extend(block.code.clone());
                if let Some(next_block_id) = block.next_block {
                    match (
                        next_block_id == current_block_id + 1,
                        matches!(code.last(), Some(OpCode::Jump(_))),
                    ) {
                        (true, true) => {
                            code.pop();
                        }
                        (false, false) => {
                            code.push(OpCode::Jump(JumpTarget(next_block_id.into())));
                        }
                        _ => (),
                    }
                }
            }
        }
        for opcode in &mut code {
            if let Some(JumpTarget(jump_target)) = opcode.jump_target_mut() {
                *jump_target = jump_target_table[*jump_target];
            }
        }
        code
    }
}

#[derive(Debug, Clone, Default)]
pub struct BasicBlock {
    pub code: Vec<OpCode>,
    pub next_block: Option<BasicBlockId>,
    pub start_stack_size: usize,
    pub end_stack_size: usize,
    pub reachable: bool,
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[expect(clippy::use_debug)]
        writeln!(f, "next_block: {:?}", self.next_block)?;
        writeln!(f, "start_stack_size: {}", self.start_stack_size)?;
        writeln!(f, "end_stack_size: {}", self.end_stack_size)?;
        writeln!(f, "reachable: {}", self.reachable)?;
        let code_str = self
            .code
            .iter()
            .enumerate()
            .map(|(index, code)| format!("{index:>12} {code}",))
            .join("\n");
        write!(f, "code:\n{code_str}")
    }
}
