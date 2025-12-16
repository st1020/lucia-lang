#![allow(clippy::multiple_inherent_impl)]

use std::rc::Rc;

use crate::{
    Context,
    compiler::{
        code::ConstValue,
        opcode::{JumpTarget, OpCode},
        value::MetaMethod,
    },
    errors::Error,
    executor::ExecutorInner,
    frame::{EffectHandlerInfo, Frame},
    objects::{ClosureInner, EffectInner, MetaResult, TableEntries, TableInner, Value},
};

impl ExecutorInner {
    /// Run this stack frame.
    pub(crate) fn run_vm(
        &mut self,
        ctx: &mut Context,
        mut instructions: u32,
    ) -> Result<u32, Error> {
        assert_ne!(instructions, 0);

        let Some(Frame::Lucia(frame)) = self.frames.last_mut() else {
            panic!("top frame is already finished");
        };
        let code = &frame.closure.code;

        macro_rules! operator_error {
            ($operator:expr, $arg1:expr) => {
                Error::UnOperator {
                    operator: $operator,
                    operand: $arg1.value_type(),
                }
            };
            ($operator:expr, $arg1:expr, $arg2:expr) => {
                Error::BinOperator {
                    operator: $operator,
                    operand: ($arg1.value_type(), $arg2.value_type()),
                }
            };
        }
        macro_rules! call_metamethod {
            ($meta_result:expr) => {
                match $meta_result? {
                    MetaResult::Value(v) => frame.stack.push(v),
                    MetaResult::TailCall(callee, args) => {
                        self.call_function(callee, args.to_vec());
                        break;
                    }
                    MetaResult::TailEffect(effect, args) => {
                        self.perform_effect(effect, args);
                        break;
                    }
                }
            };
        }
        macro_rules! bin_op {
            ($name:ident) => {{
                let rhs = frame.stack.pop().unwrap();
                let lhs = frame.stack.pop().unwrap();
                call_metamethod!(lhs.$name(ctx, rhs));
            }};
        }
        macro_rules! get_table {
            ($name:ident) => {{
                let key = frame.stack.pop().unwrap();
                let table = frame.stack.pop().unwrap();
                call_metamethod!(table.$name(ctx, key));
            }};
        }
        macro_rules! set_table {
            ($name:ident) => {{
                let key = frame.stack.pop().unwrap();
                let table = frame.stack.pop().unwrap();
                let value = frame.stack.pop().unwrap();
                call_metamethod!(table.$name(ctx, key, value))
            }};
        }

        loop {
            let opcode = code.code[frame.pc];
            frame.pc += 1;

            match opcode {
                OpCode::Pop => {
                    frame.stack.pop().unwrap();
                }
                OpCode::Copy(i) => {
                    frame.stack.push(frame.stack[frame.stack.len() - i].clone());
                }
                OpCode::Swap(i) => {
                    let stack_len = frame.stack.len();
                    frame.stack.swap(stack_len - i, stack_len - 1);
                }
                OpCode::LoadLocal(i) => {
                    frame.stack.push(frame.locals[i].clone());
                }
                OpCode::LoadGlobal(i) => {
                    let mut v = ctx.globals.get(Rc::clone(&code.global_names[i]));
                    if v.is_null() {
                        v = ctx.builtins.get(Rc::clone(&code.global_names[i]));
                    }
                    frame.stack.push(v);
                }
                OpCode::LoadUpvalue(i) => {
                    frame.stack.push(frame.closure.upvalues[i].clone());
                }
                OpCode::LoadConst(i) => {
                    frame.stack.push(match &code.consts[i] {
                        ConstValue::Null => Value::Null,
                        ConstValue::Bool(v) => Value::Bool(*v),
                        ConstValue::Int(v) => Value::Int(*v),
                        ConstValue::Float(v) => Value::Float(*v),
                        ConstValue::Str(v) => Value::Str(Rc::clone(v)),
                        ConstValue::Bytes(v) => Value::Bytes(Rc::new(v.clone().into())),
                        ConstValue::Code(code) => {
                            ClosureInner::new(Rc::clone(code), Some(frame)).into()
                        }
                        ConstValue::Effect(effect) => EffectInner::new(effect.clone()).into(),
                    });
                }
                OpCode::StoreLocal(i) => {
                    frame.locals[i] = frame.stack.pop().unwrap();
                }
                OpCode::StoreGlobal(i) => {
                    let value = frame.stack.pop().unwrap();
                    ctx.globals.set(Rc::clone(&code.global_names[i]), value);
                }
                OpCode::BuildTable(i) => {
                    #[expect(clippy::missing_asserts_for_indexing)]
                    let table = frame
                        .stack
                        .split_off(frame.stack.len() - i * 2)
                        .chunks(2)
                        .map(|chunk| (chunk[0].clone(), chunk[1].clone()))
                        .collect::<TableEntries>()
                        .into();
                    frame.stack.push(table);
                }
                OpCode::BuildList(i) => {
                    let table = frame.stack.split_off(frame.stack.len() - i).into();
                    frame.stack.push(table);
                }
                OpCode::GetAttr => get_table!(meta_get_attr),
                OpCode::GetItem => get_table!(meta_get_item),
                OpCode::GetMeta => {
                    let value = frame.stack.pop().unwrap();
                    let metatable = value.metatable().into();
                    frame.stack.push(metatable);
                }
                OpCode::SetAttr => set_table!(meta_set_attr),
                OpCode::SetItem => set_table!(meta_set_item),
                OpCode::SetMeta => {
                    let table = frame.stack.pop().unwrap();
                    let metatable = frame.stack.pop().unwrap();
                    let new_table = match (table, metatable) {
                        (Value::Table(table), Value::Null) => {
                            let mut table = Rc::unwrap_or_clone(table);
                            table.set_metatable(None);
                            table.into()
                        }
                        (Value::Table(table), Value::Table(metatable)) => {
                            let mut table = Rc::unwrap_or_clone(table);
                            table.set_metatable(Some(metatable));
                            table.into()
                        }
                        (table, metatable) => {
                            return Err(operator_error!(opcode, table, metatable));
                        }
                    };
                    frame.stack.push(new_table);
                }
                OpCode::Neg => {
                    let value = frame.stack.pop().unwrap();
                    call_metamethod!(value.meta_neg(ctx));
                }
                OpCode::Not => {
                    let value = frame.stack.pop().unwrap();
                    if let Value::Bool(v) = value {
                        frame.stack.push(Value::Bool(!v));
                    } else {
                        return Err(operator_error!(opcode, value));
                    }
                }
                OpCode::Add => bin_op!(meta_add),
                OpCode::Sub => bin_op!(meta_sub),
                OpCode::Mul => bin_op!(meta_mul),
                OpCode::Div => bin_op!(meta_div),
                OpCode::Rem => bin_op!(meta_rem),
                OpCode::Eq => bin_op!(meta_eq),
                OpCode::Ne => bin_op!(meta_ne),
                OpCode::Gt => bin_op!(meta_gt),
                OpCode::Ge => bin_op!(meta_ge),
                OpCode::Lt => bin_op!(meta_lt),
                OpCode::Le => bin_op!(meta_le),
                OpCode::Identical => {
                    let rhs = frame.stack.pop().unwrap();
                    let lhs = frame.stack.pop().unwrap();
                    frame.stack.push(Value::Bool(lhs.identical(&rhs)));
                }
                OpCode::NotIdentical => {
                    let rhs = frame.stack.pop().unwrap();
                    let lhs = frame.stack.pop().unwrap();
                    frame.stack.push(Value::Bool(!lhs.identical(&rhs)));
                }
                OpCode::TypeCheck(ty) => {
                    let value = frame.stack.pop().unwrap();
                    frame.stack.push(Value::Bool(value.value_type() == ty));
                }
                OpCode::GetLen => {
                    let value = frame.stack.pop().unwrap();
                    call_metamethod!(value.meta_len(ctx));
                }
                OpCode::Import(i) => {
                    if let ConstValue::Str(v) = &code.consts[i] {
                        frame.stack.push(ctx.libs.get(Rc::clone(v)));
                    } else {
                        panic!("program error");
                    }
                }
                OpCode::ImportFrom(i) => {
                    let module = frame.stack.last().cloned().unwrap();
                    match (module, code.consts[i].clone()) {
                        (Value::Table(module), ConstValue::Str(key)) => {
                            frame.stack.push(module.get(key));
                        }
                        (module, _) => return Err(operator_error!(opcode, module)),
                    }
                }
                OpCode::ImportGlob => {
                    let module = frame.stack.pop().unwrap();
                    if let Value::Table(module) = module {
                        for (k, v) in module.iter() {
                            if let Value::Str(k) = k {
                                ctx.globals.set(k, v);
                            }
                        }
                    } else {
                        return Err(operator_error!(opcode, module));
                    }
                }
                OpCode::Iter => {
                    let value = frame.stack.pop().unwrap();
                    frame.stack.push(value.meta_iter(ctx)?.into());
                }
                OpCode::Call(i) => {
                    let args = frame.stack.split_off(frame.stack.len() - i);
                    let callee = frame.stack.pop().unwrap();
                    self.call_function(callee.meta_call(ctx)?, args);
                    break;
                }
                OpCode::Return => {
                    debug_assert_eq!(frame.stack.len(), 1);
                    let value = frame.stack.pop().unwrap();
                    self.return_upper(value);
                    break;
                }
                OpCode::ReturnCall(i) => {
                    let args = frame.stack.split_off(frame.stack.len() - i);
                    let callee = frame.stack.pop().unwrap();
                    self.tail_call(callee.meta_call(ctx)?, args);
                    break;
                }
                OpCode::LoadLocals => {
                    let mut table = TableInner::new();
                    for i in 0..code.local_names.len() {
                        table.set(Rc::clone(&code.local_names[i]), frame.locals[i].clone());
                    }
                    frame.stack.push(table.into());
                }
                OpCode::Jump(JumpTarget(i)) => {
                    frame.pc = i;
                    continue;
                }
                OpCode::JumpPopIfNull(JumpTarget(i)) => {
                    let value = frame.stack.last().cloned().unwrap();
                    if let Value::Null = value {
                        frame.stack.pop().unwrap();
                        frame.pc = i;
                        continue;
                    }
                }
                OpCode::PopJumpIfTrue(JumpTarget(i)) => {
                    let value = frame.stack.pop().unwrap();
                    #[expect(clippy::wildcard_enum_match_arm)]
                    match value {
                        Value::Bool(true) => {
                            frame.pc = i;
                            continue;
                        }
                        Value::Bool(false) => (),
                        _ => return Err(operator_error!(opcode, value)),
                    }
                }
                OpCode::PopJumpIfFalse(JumpTarget(i)) => {
                    let value = frame.stack.pop().unwrap();
                    #[expect(clippy::wildcard_enum_match_arm)]
                    match value {
                        Value::Bool(true) => (),
                        Value::Bool(false) => {
                            frame.pc = i;
                            continue;
                        }
                        _ => return Err(operator_error!(opcode, value)),
                    }
                }
                OpCode::JumpIfTrueOrPop(JumpTarget(i)) => {
                    let value = frame.stack.last().cloned().unwrap();
                    #[expect(clippy::wildcard_enum_match_arm)]
                    match value {
                        Value::Bool(true) => {
                            frame.pc = i;
                            continue;
                        }
                        Value::Bool(false) => {
                            frame.stack.pop().unwrap();
                        }
                        _ => return Err(operator_error!(opcode, value)),
                    }
                }
                OpCode::JumpIfFalseOrPop(JumpTarget(i)) => {
                    let value = frame.stack.last().cloned().unwrap();
                    #[expect(clippy::wildcard_enum_match_arm)]
                    match value {
                        Value::Bool(true) => {
                            frame.stack.pop().unwrap();
                        }
                        Value::Bool(false) => {
                            frame.pc = i;
                            continue;
                        }
                        _ => return Err(operator_error!(opcode, value)),
                    }
                }
                OpCode::RegisterHandler(JumpTarget(i)) => {
                    let effect = frame.stack.pop().unwrap();
                    if let Value::Effect(effect) = effect {
                        frame.effect_handlers.insert(
                            effect,
                            EffectHandlerInfo {
                                jump_target: i,
                                stack_size: frame.stack.len(),
                            },
                        );
                    } else {
                        return Err(operator_error!(opcode, effect));
                    }
                }
                OpCode::CheckEffect(_) => {
                    let expected_effect = frame.stack.pop().unwrap();
                    let effect = frame.stack.pop().unwrap();
                    match (effect, expected_effect) {
                        (Value::Effect(effect), Value::Effect(expected_effect))
                            if effect.match_effect_handler(&expected_effect) => {}
                        (effect, expected_effect) => {
                            return Err(operator_error!(opcode, expected_effect, effect));
                        }
                    }
                }
            }

            if instructions == 0 {
                break;
            }
            instructions -= 1;
        }
        Ok(instructions)
    }
}
