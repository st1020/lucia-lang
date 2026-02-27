#![allow(clippy::multiple_inherent_impl)]

use std::rc::Rc;

use crate::{
    Context,
    compiler::{code::ConstValue, opcode::OpCode, value::MetaMethod},
    errors::Error,
    executor::Executor,
    frame::{EffectHandlerInfo, Frame},
    objects::{Closure, Effect, MetaResult, Table, TableEntries, Value},
};

impl Executor {
    /// Run this stack frame.
    #[expect(clippy::panic_in_result_fn)]
    pub(crate) fn run_vm(
        &mut self,
        ctx: &mut Context,
        mut instructions: u32,
    ) -> Result<u32, Error> {
        assert_ne!(instructions, 0, "instructions must be greater than 0");

        let Some(Frame::Lucia {
            pc,
            closure,
            locals,
            stack,
            effect_handlers,
        }) = self.frames.last_mut()
        else {
            unreachable!("invalid frame type");
        };
        let code = &closure.code;

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
                    MetaResult::TailCall { function, args } => {
                        self.call_function(function, args.to_vec());
                        break;
                    }
                    MetaResult::TailEffect { effect, args } => {
                        self.perform_effect(effect, args);
                        break;
                    }
                    MetaResult::ReturnValue { value } => stack.push(value),
                }
            };
        }
        macro_rules! bin_op {
            ($name:ident) => {{
                let rhs = stack.pop().unwrap();
                let lhs = stack.pop().unwrap();
                call_metamethod!(lhs.$name(ctx, rhs));
            }};
        }
        macro_rules! get_table {
            ($name:ident) => {{
                let key = stack.pop().unwrap();
                let table = stack.pop().unwrap();
                call_metamethod!(table.$name(ctx, key));
            }};
        }
        macro_rules! set_table {
            ($name:ident) => {{
                let key = stack.pop().unwrap();
                let table = stack.pop().unwrap();
                let value = stack.pop().unwrap();
                call_metamethod!(table.$name(ctx, key, value))
            }};
        }

        loop {
            let opcode = code.code[*pc];
            *pc += 1;

            match opcode {
                OpCode::Pop => {
                    stack.pop().unwrap();
                }
                OpCode::Copy(i) => {
                    stack.push(stack[stack.len() - i].clone());
                }
                OpCode::Swap(i) => {
                    let stack_len = stack.len();
                    stack.swap(stack_len - i, stack_len - 1);
                }
                OpCode::LoadLocal(i) => {
                    stack.push(locals[i].clone());
                }
                OpCode::LoadGlobal(i) => {
                    let mut v = ctx.globals.get(Rc::clone(&code.global_names[i]));
                    if v.is_null() {
                        v = ctx.builtins.get(Rc::clone(&code.global_names[i]));
                    }
                    stack.push(v);
                }
                OpCode::LoadUpvalue(i) => {
                    stack.push(closure.upvalues[i].clone());
                }
                OpCode::LoadConst(i) => {
                    stack.push(match &code.consts[i] {
                        ConstValue::Null => Value::Null,
                        ConstValue::Bool(v) => Value::Bool(*v),
                        ConstValue::Int(v) => Value::Int(*v),
                        ConstValue::Float(v) => Value::Float(*v),
                        ConstValue::Str(v) => Value::Str(Rc::clone(v)),
                        ConstValue::Bytes(v) => Value::Bytes(Rc::new(v.clone().into())),
                        ConstValue::Function(v) => {
                            let code = Rc::clone(&code.const_codes[*v]);
                            Closure::with_base_closure(code, locals, closure).into()
                        }
                        ConstValue::Effect(v) => Effect::new(v.clone()).into(),
                    });
                }
                OpCode::StoreLocal(i) => {
                    locals[i] = stack.pop().unwrap();
                }
                OpCode::StoreGlobal(i) => {
                    let value = stack.pop().unwrap();
                    ctx.globals.set(Rc::clone(&code.global_names[i]), value);
                }
                OpCode::BuildTable(i) => {
                    #[expect(clippy::missing_asserts_for_indexing)]
                    let table = stack
                        .split_off(stack.len() - i * 2)
                        .chunks(2)
                        .map(|chunk| (chunk[0].clone(), chunk[1].clone()))
                        .collect::<TableEntries>()
                        .into();
                    stack.push(table);
                }
                OpCode::BuildList(i) => {
                    let table = stack.split_off(stack.len() - i).into();
                    stack.push(table);
                }
                OpCode::GetAttr => get_table!(meta_get_attr),
                OpCode::GetItem => get_table!(meta_get_item),
                OpCode::GetMeta => {
                    let value = stack.pop().unwrap();
                    let metatable = value.metatable().into();
                    stack.push(metatable);
                }
                OpCode::SetAttr => set_table!(meta_set_attr),
                OpCode::SetItem => set_table!(meta_set_item),
                OpCode::SetMeta => {
                    let table = stack.pop().unwrap();
                    let metatable = stack.pop().unwrap();
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
                    stack.push(new_table);
                }
                OpCode::Neg => {
                    let value = stack.pop().unwrap();
                    call_metamethod!(value.meta_neg(ctx));
                }
                OpCode::Not => {
                    let value = stack.pop().unwrap();
                    if let Value::Bool(v) = value {
                        stack.push(Value::Bool(!v));
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
                OpCode::TypeCheck(ty) => {
                    let value = stack.pop().unwrap();
                    stack.push(Value::Bool(value.value_type() == ty));
                }
                OpCode::GetLen => {
                    let value = stack.pop().unwrap();
                    call_metamethod!(value.meta_len(ctx));
                }
                OpCode::Import(i) => {
                    if let ConstValue::Str(v) = &code.consts[i] {
                        stack.push(ctx.libs.get(Rc::clone(v)));
                    } else {
                        unreachable!("program error");
                    }
                }
                OpCode::ImportFrom(i) => {
                    let module = stack.last().cloned().unwrap();
                    match (module, code.consts[i].clone()) {
                        (Value::Table(module), ConstValue::Str(key)) => {
                            stack.push(module.get(key));
                        }
                        (module, _) => return Err(operator_error!(opcode, module)),
                    }
                }
                OpCode::ImportGlob => {
                    let module = stack.pop().unwrap();
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
                    let value = stack.pop().unwrap();
                    stack.push(value.meta_iter(ctx)?.into());
                }
                OpCode::Call(i) => {
                    let args = stack.split_off(stack.len() - i);
                    let callee = stack.pop().unwrap();
                    self.call_function(callee.meta_call(ctx)?, args);
                    break;
                }
                OpCode::Return => {
                    debug_assert_eq!(stack.len(), 1, "stack length must be 1 on return");
                    let value = stack.pop().unwrap();
                    self.return_value(value);
                    break;
                }
                OpCode::ReturnCall(i) => {
                    let args = stack.split_off(stack.len() - i);
                    let callee = stack.pop().unwrap();
                    self.tail_call(callee.meta_call(ctx)?, args);
                    break;
                }
                OpCode::LoadLocals => {
                    let mut table = Table::new();
                    for i in code.local_names.indices() {
                        table.set(Rc::clone(&code.local_names[i]), locals[i].clone());
                    }
                    stack.push(table.into());
                }
                OpCode::Jump(i)
                | OpCode::JumpBackEdge(i)
                | OpCode::Break(i)
                | OpCode::Continue(i) => {
                    *pc = i;
                    continue;
                }
                OpCode::JumpPopIfNull(i) => {
                    let value = stack.last().cloned().unwrap();
                    if let Value::Null = value {
                        stack.pop().unwrap();
                        *pc = i;
                        continue;
                    }
                }
                OpCode::PopJumpIfTrue(i) => {
                    let value = stack.pop().unwrap();
                    #[expect(clippy::wildcard_enum_match_arm)]
                    match value {
                        Value::Bool(true) => {
                            *pc = i;
                            continue;
                        }
                        Value::Bool(false) => (),
                        _ => return Err(operator_error!(opcode, value)),
                    }
                }
                OpCode::PopJumpIfFalse(i) => {
                    let value = stack.pop().unwrap();
                    #[expect(clippy::wildcard_enum_match_arm)]
                    match value {
                        Value::Bool(true) => (),
                        Value::Bool(false) => {
                            *pc = i;
                            continue;
                        }
                        _ => return Err(operator_error!(opcode, value)),
                    }
                }
                OpCode::JumpIfTrueOrPop(i) => {
                    let value = stack.last().cloned().unwrap();
                    #[expect(clippy::wildcard_enum_match_arm)]
                    match value {
                        Value::Bool(true) => {
                            *pc = i;
                            continue;
                        }
                        Value::Bool(false) => {
                            stack.pop().unwrap();
                        }
                        _ => return Err(operator_error!(opcode, value)),
                    }
                }
                OpCode::JumpIfFalseOrPop(i) => {
                    let value = stack.last().cloned().unwrap();
                    #[expect(clippy::wildcard_enum_match_arm)]
                    match value {
                        Value::Bool(true) => {
                            stack.pop().unwrap();
                        }
                        Value::Bool(false) => {
                            *pc = i;
                            continue;
                        }
                        _ => return Err(operator_error!(opcode, value)),
                    }
                }
                OpCode::RegisterHandler(i) => {
                    let effect = stack.pop().unwrap();
                    if let Value::Effect(effect) = effect {
                        effect_handlers.insert(
                            effect,
                            EffectHandlerInfo {
                                jump_target: i,
                                stack_size: stack.len(),
                            },
                        );
                    } else {
                        return Err(operator_error!(opcode, effect));
                    }
                }
                OpCode::CheckEffect(_) => {
                    let expected_effect = stack.pop().unwrap();
                    let effect = stack.pop().unwrap();
                    match (effect, expected_effect) {
                        (Value::Effect(effect), Value::Effect(expected_effect))
                            if effect.match_effect_handler(&expected_effect) => {}
                        (effect, expected_effect) => {
                            return Err(operator_error!(opcode, expected_effect, effect));
                        }
                    }
                }
                OpCode::MarkAddStackSize(_) => (),
            }

            if instructions == 0 {
                break;
            }
            instructions -= 1;
        }
        Ok(instructions)
    }
}
