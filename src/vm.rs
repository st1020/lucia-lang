use crate::{
    compiler::opcode::{JumpTarget, OpCode},
    errors::{Error, LuciaError, RuntimeError},
    frame::{CatchErrorKind, Frame},
    meta_ops,
    objects::{Closure, Function, IntoValue, RuntimeConstValue, Table, TableEntries, Value},
    thread::ThreadState,
    Context,
};

impl<'gc> ThreadState<'gc> {
    /// Run this stack frame.
    pub(crate) fn run_vm(
        &mut self,
        ctx: Context<'gc>,
        mut instructions: u32,
    ) -> Result<u32, Error<'gc>> {
        assert_ne!(instructions, 0);

        let frame = match self.frames.last_mut() {
            Some(Frame::Lua(frame)) => frame,
            _ => panic!("top frame is not lua frame"),
        };
        let function = &frame.closure.function;

        macro_rules! operator_error {
            ($operator:expr, $arg1:expr) => {
                Error::with_traceback(
                    RuntimeError::UnOperator {
                        operator: $operator,
                        operand: $arg1.value_type(),
                    },
                    self.traceback(),
                )
            };
            ($operator:expr, $arg1:expr, $arg2:expr) => {
                Error::with_traceback(
                    RuntimeError::BinOperator {
                        operator: $operator,
                        operand: ($arg1.value_type(), $arg2.value_type()),
                    },
                    self.traceback(),
                )
            };
        }
        macro_rules! call_meta_method {
            ($meta_result:expr) => {
                match $meta_result? {
                    meta_ops::MetaResult::Value(v) => frame.stack.push(v),
                    meta_ops::MetaResult::Call(callee, args) => {
                        self.call_function(ctx, callee, args.to_vec())?;
                        break;
                    }
                }
            };
        }
        macro_rules! bin_op {
            ($name:ident) => {{
                let rhs = frame.stack.pop().unwrap();
                let lhs = frame.stack.pop().unwrap();
                call_meta_method!(meta_ops::$name(ctx, lhs, rhs));
            }};
        }
        macro_rules! get_table {
            ($name:ident) => {{
                let key = frame.stack.pop().unwrap();
                let table = frame.stack.pop().unwrap();
                call_meta_method!(meta_ops::$name(ctx, table, key));
            }};
        }
        macro_rules! set_table {
            ($name:ident) => {{
                let key = frame.stack.pop().unwrap();
                let table = frame.stack.pop().unwrap();
                let value = frame.stack.pop().unwrap();
                match meta_ops::$name(ctx, table, key, value)? {
                    meta_ops::MetaResult::Value(_) => (),
                    meta_ops::MetaResult::Call(callee, args) => {
                        self.call_function(ctx, callee, args.to_vec())?;
                        break;
                    }
                }
            }};
        }

        loop {
            let code = function.code[frame.pc];
            frame.pc += 1;

            match code {
                OpCode::Pop => {
                    frame.stack.pop().unwrap();
                }
                OpCode::Copy(i) => {
                    frame.stack.push(frame.stack[frame.stack.len() - i]);
                }
                OpCode::Swap(i) => {
                    let stack_len = frame.stack.len();
                    frame.stack.swap(stack_len - i, stack_len - 1);
                }
                OpCode::LoadLocal(i) => {
                    frame.stack.push(frame.locals[i]);
                }
                OpCode::LoadGlobal(i) => {
                    let mut v = ctx.state.globals.get(ctx, function.global_names[i]);
                    if v.is_null() {
                        v = ctx.state.builtins.get(ctx, function.global_names[i]);
                    }
                    frame.stack.push(v);
                }
                OpCode::LoadUpvalue(i) => {
                    frame.stack.push(frame.upvalues[i].get());
                }
                OpCode::LoadConst(i) => {
                    frame.stack.push(match function.consts[i] {
                        RuntimeConstValue::Null => Value::Null,
                        RuntimeConstValue::Bool(v) => Value::Bool(v),
                        RuntimeConstValue::Int(v) => Value::Int(v),
                        RuntimeConstValue::Float(v) => Value::Float(v),
                        RuntimeConstValue::Str(v) => Value::Str(v),
                        RuntimeConstValue::Func(v) => {
                            Value::Function(Function::Closure(Closure::new(&ctx, v, Some(frame))))
                        }
                    });
                }
                OpCode::StoreLocal(i) => {
                    frame.locals[i] = frame.stack.pop().unwrap();
                }
                OpCode::StoreGlobal(i) => {
                    let value = frame.stack.pop().unwrap();
                    ctx.state.globals.set(ctx, function.global_names[i], value);
                }
                OpCode::StoreUpvalue(i) => {
                    frame.upvalues[i].set(&ctx, frame.stack.pop().unwrap());
                }
                OpCode::Import(i) => {
                    if let RuntimeConstValue::Str(v) = function.consts[i] {
                        frame.stack.push(ctx.state.libs.get(ctx, v));
                    } else {
                        panic!("program error");
                    }
                }
                OpCode::ImportFrom(i) => {
                    let module = frame.stack.last().copied().unwrap();
                    if let (Value::Table(module), RuntimeConstValue::Str(key)) =
                        (module, function.consts[i])
                    {
                        frame.stack.push(module.get(ctx, key));
                    } else {
                        return Err(operator_error!(code, module));
                    }
                }
                OpCode::ImportGlob => {
                    let module = frame.stack.pop().unwrap();
                    if let Value::Table(module) = module {
                        for (k, v) in module.iter() {
                            if let Value::Str(k) = k {
                                ctx.state.globals.set(ctx, k, v);
                            }
                        }
                    } else {
                        return Err(operator_error!(code, module));
                    };
                }
                OpCode::BuildTable(i) => {
                    let table = TableEntries::from_iter(
                        frame
                            .stack
                            .split_off(frame.stack.len() - i * 2)
                            .chunks_exact(2)
                            .map(|chunk| (chunk[0], chunk[1])),
                    )
                    .into_value(ctx);
                    frame.stack.push(table);
                }
                OpCode::BuildList(i) => {
                    let table = frame.stack.split_off(frame.stack.len() - i).into_value(ctx);
                    frame.stack.push(table);
                }
                OpCode::GetAttr => get_table!(get_attr),
                OpCode::GetItem => get_table!(get_item),
                OpCode::GetMeta => {
                    let value = frame.stack.pop().unwrap();
                    let metatable = value.metatable().into_value(ctx);
                    frame.stack.push(metatable);
                }
                OpCode::SetAttr => set_table!(set_attr),
                OpCode::SetItem => set_table!(set_item),
                OpCode::SetMeta => {
                    let table = frame.stack.pop().unwrap();
                    let metatable = frame.stack.pop().unwrap();
                    match (table, metatable) {
                        (Value::Table(table), Value::Null) => table.set_metatable(&ctx, None),
                        (Value::Table(table), Value::Table(metatable)) => {
                            table.set_metatable(&ctx, Some(metatable))
                        }
                        _ => return Err(operator_error!(code, table, metatable)),
                    }
                }
                OpCode::Neg => {
                    let value = frame.stack.pop().unwrap();
                    call_meta_method!(meta_ops::neg(ctx, value));
                }
                OpCode::Not => {
                    let value = frame.stack.pop().unwrap();
                    if let Value::Bool(v) = value {
                        frame.stack.push(Value::Bool(!v));
                    } else {
                        return Err(operator_error!(code, value));
                    }
                }
                OpCode::Add => bin_op!(add),
                OpCode::Sub => bin_op!(sub),
                OpCode::Mul => bin_op!(mul),
                OpCode::Div => bin_op!(div),
                OpCode::Rem => bin_op!(rem),
                OpCode::Eq => bin_op!(eq),
                OpCode::Ne => bin_op!(ne),
                OpCode::Gt => bin_op!(gt),
                OpCode::Ge => bin_op!(ge),
                OpCode::Lt => bin_op!(lt),
                OpCode::Le => bin_op!(le),
                OpCode::Identical => {
                    let rhs = frame.stack.pop().unwrap();
                    let lhs = frame.stack.pop().unwrap();
                    frame.stack.push(Value::Bool(lhs.identical(rhs)));
                }
                OpCode::NotIdentical => {
                    let rhs = frame.stack.pop().unwrap();
                    let lhs = frame.stack.pop().unwrap();
                    frame.stack.push(Value::Bool(!lhs.identical(rhs)));
                }
                OpCode::TypeCheck(ty) => {
                    let value = frame.stack.pop().unwrap();
                    frame.stack.push(Value::Bool(value.value_type() == ty));
                }
                OpCode::GetLen => {
                    let value = frame.stack.pop().unwrap();
                    call_meta_method!(meta_ops::len(ctx, value));
                }
                OpCode::Iter => {
                    let value = frame.stack.pop().unwrap();
                    frame.stack.push(meta_ops::iter(ctx, value)?.into());
                }
                OpCode::Jump(JumpTarget(i)) => {
                    frame.pc = i;
                    continue;
                }
                OpCode::JumpPopIfNull(JumpTarget(i)) => {
                    let value = frame.stack.last().copied().unwrap();
                    if let Value::Null = value {
                        frame.stack.pop().unwrap();
                        frame.pc = i;
                        continue;
                    }
                }
                OpCode::PopJumpIfTrue(JumpTarget(i)) => {
                    let value = frame.stack.pop().unwrap();
                    match value {
                        Value::Bool(true) => {
                            frame.pc = i;
                            continue;
                        }
                        Value::Bool(false) => (),
                        _ => return Err(operator_error!(code, value)),
                    }
                }
                OpCode::PopJumpIfFalse(JumpTarget(i)) => {
                    let value = frame.stack.pop().unwrap();
                    match value {
                        Value::Bool(true) => (),
                        Value::Bool(false) => {
                            frame.pc = i;
                            continue;
                        }
                        _ => return Err(operator_error!(code, value)),
                    }
                }
                OpCode::JumpIfTrueOrPop(JumpTarget(i)) => {
                    let value = frame.stack.last().copied().unwrap();
                    match value {
                        Value::Bool(true) => {
                            frame.pc = i;
                            continue;
                        }
                        Value::Bool(false) => {
                            frame.stack.pop().unwrap();
                        }
                        _ => return Err(operator_error!(code, value)),
                    }
                }
                OpCode::JumpIfFalseOrPop(JumpTarget(i)) => {
                    let value = frame.stack.last().copied().unwrap();
                    match value {
                        Value::Bool(true) => {
                            frame.stack.pop().unwrap();
                        }
                        Value::Bool(false) => {
                            frame.pc = i;
                            continue;
                        }
                        _ => return Err(operator_error!(code, value)),
                    }
                }
                OpCode::Call(i) => {
                    frame.catch_error = CatchErrorKind::None;
                    let args = frame.stack.split_off(frame.stack.len() - i);
                    let callee = frame.stack.pop().unwrap();
                    self.call_function(ctx, meta_ops::call(ctx, callee)?, args)?;
                    break;
                }
                OpCode::TryCall(i) => {
                    frame.catch_error = CatchErrorKind::Try;
                    let args = frame.stack.split_off(frame.stack.len() - i);
                    let callee = frame.stack.pop().unwrap();
                    self.call_function(ctx, meta_ops::call(ctx, callee)?, args)?;
                    break;
                }
                OpCode::TryOptionCall(i) => {
                    frame.catch_error = CatchErrorKind::TryOption;
                    let args = frame.stack.split_off(frame.stack.len() - i);
                    let callee = frame.stack.pop().unwrap();
                    self.call_function(ctx, meta_ops::call(ctx, callee)?, args)?;
                    break;
                }
                OpCode::TryPanicCall(i) => {
                    frame.catch_error = CatchErrorKind::TryPanic;
                    let args = frame.stack.split_off(frame.stack.len() - i);
                    let callee = frame.stack.pop().unwrap();
                    self.call_function(ctx, meta_ops::call(ctx, callee)?, args)?;
                    break;
                }
                OpCode::Return => {
                    debug_assert_eq!(frame.stack.len(), 1);
                    self.return_upper(ctx);
                    break;
                }
                OpCode::Throw => {
                    debug_assert_eq!(frame.stack.len(), 1);
                    let value = frame.stack.pop().unwrap();
                    return Err(Error::new(LuciaError::Error(value)));
                }
                OpCode::ReturnCall(i) => {
                    let args = frame.stack.split_off(frame.stack.len() - i);
                    let callee = frame.stack.pop().unwrap();
                    self.tail_call(ctx, meta_ops::call(ctx, callee)?, args)?;
                    break;
                }
                OpCode::LoadLocals => {
                    let table = Table::new(&ctx);
                    for i in 0..function.local_names.len() {
                        table.set(ctx, function.local_names[i], frame.locals[i]);
                    }
                    frame.stack.push(Value::Table(table));
                }
                OpCode::JumpTarget(_) => panic!("unexpected opcode: JumpTarget"),
            }

            if instructions == 0 {
                break;
            } else {
                instructions -= 1
            }
        }
        Ok(instructions)
    }
}
