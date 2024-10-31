use crate::{
    compiler::{
        code::FunctionKind,
        opcode::{JumpTarget, OpCode},
    },
    errors::{Error, LuciaError, RuntimeError},
    frame::{CatchErrorKind, Frame},
    meta_ops,
    objects::{Closure, Function, IntoValue, RuntimeConstValue, Table, Value},
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

        macro_rules! bin_op {
            ($name:ident) => {{
                let tos = frame.stack.pop().unwrap();
                let tos1 = frame.stack.pop().unwrap();
                match meta_ops::$name(ctx, tos1, tos)? {
                    meta_ops::MetaResult::Value(v) => frame.stack.push(v),
                    meta_ops::MetaResult::Call(callee, args) => {
                        self.call_function(ctx, callee, args.to_vec())?;
                        break;
                    }
                }
            }};
        }

        macro_rules! set_table {
            ($name:ident) => {{
                let tos = frame.stack.pop().unwrap();
                let tos1 = frame.stack.pop().unwrap();
                let tos2 = frame.stack.pop().unwrap();
                match meta_ops::$name(ctx, tos1, tos, tos2)? {
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

            // println!("{} {} {:?}", frame.pc, code, frame.stack);
            match code {
                OpCode::Pop => {
                    frame.stack.pop();
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
                    let mut v = ctx
                        .state
                        .globals
                        .get(ctx, function.global_names[i].to_owned());
                    if v.is_null() {
                        v = ctx
                            .state
                            .builtins
                            .get(ctx, function.global_names[i].to_owned());
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
                    ctx.state.globals.set(
                        ctx,
                        function.global_names[i].to_owned(),
                        frame.stack.pop().unwrap(),
                    );
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
                    let tos = *frame.stack.last().unwrap();
                    if let (Value::Table(module), RuntimeConstValue::Str(v)) =
                        (tos, function.consts[i])
                    {
                        frame.stack.push(module.get(ctx, v));
                    } else {
                        return Err(operator_error!(code, tos));
                    }
                }
                OpCode::ImportGlob => {
                    let tos = frame.stack.pop().unwrap();
                    if let Value::Table(module) = tos {
                        for (k, v) in (0..module.len()).map(|i| module.get_index(i).unwrap()) {
                            if let Value::Str(k) = k {
                                ctx.state.globals.set(ctx, k, v);
                            }
                        }
                    } else {
                        return Err(operator_error!(code, tos));
                    };
                }
                OpCode::BuildTable(i) => {
                    let temp = frame.stack.split_off(frame.stack.len() - i * 2);
                    let table = Table::new(&ctx);
                    for i in temp.chunks_exact(2) {
                        table.set(ctx, i[0], i[1]);
                    }
                    frame.stack.push(Value::Table(table));
                }
                OpCode::BuildList(i) => {
                    let temp = frame.stack.split_off(frame.stack.len() - i);
                    frame.stack.push(temp.into_value(ctx));
                }
                OpCode::GetAttr => bin_op!(get_attr),
                OpCode::GetItem => bin_op!(get_item),
                OpCode::GetMeta => {
                    let tos = frame.stack.pop().unwrap();
                    frame
                        .stack
                        .push(tos.metatable().map_or(Value::Null, Value::Table));
                }
                OpCode::SetAttr => set_table!(set_attr),
                OpCode::SetItem => set_table!(set_item),
                OpCode::SetMeta => {
                    let tos = frame.stack.pop().unwrap();
                    let tos1 = frame.stack.pop().unwrap();
                    match tos {
                        Value::Table(t) => match tos1 {
                            Value::Null => {
                                t.set_metatable(&ctx, None);
                            }
                            Value::Table(tos1) => {
                                t.set_metatable(&ctx, Some(tos1));
                            }
                            _ => return Err(operator_error!(code, tos, tos1)),
                        },
                        _ => return Err(operator_error!(code, tos, tos1)),
                    }
                }
                OpCode::Neg => {
                    let tos = frame.stack.pop().unwrap();
                    match meta_ops::neg(ctx, tos)? {
                        meta_ops::MetaResult::Value(v) => frame.stack.push(v),
                        meta_ops::MetaResult::Call(callee, args) => {
                            self.call_function(ctx, callee, args.to_vec())?;
                            break;
                        }
                    }
                }
                OpCode::Not => {
                    let tos = frame.stack.pop().unwrap();
                    if let Value::Bool(v) = tos {
                        frame.stack.push(Value::Bool(!v));
                    } else {
                        return Err(operator_error!(code, tos));
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
                    let tos = frame.stack.pop().unwrap();
                    let tos1 = frame.stack.pop().unwrap();
                    frame.stack.push(Value::Bool(tos1.identical(tos)));
                }
                OpCode::NotIdentical => {
                    let tos = frame.stack.pop().unwrap();
                    let tos1 = frame.stack.pop().unwrap();
                    frame.stack.push(Value::Bool(!tos1.identical(tos)));
                }
                OpCode::TypeCheck(ty) => {
                    let tos = frame.stack.pop().unwrap();
                    frame.stack.push(Value::Bool(tos.value_type() == ty));
                }
                OpCode::Iter => {
                    let tos = frame.stack.pop().unwrap();
                    frame.stack.push(meta_ops::iter(ctx, tos)?.into());
                }
                OpCode::Jump(JumpTarget(i)) => {
                    frame.pc = i;
                    continue;
                }
                OpCode::JumpIfNull(JumpTarget(i)) => {
                    let tos = frame.stack.last().unwrap();
                    if let Value::Null = tos {
                        frame.pc = i;
                        continue;
                    }
                }
                OpCode::JumpPopIfFalse(JumpTarget(i)) => {
                    let tos = frame.stack.pop().unwrap();
                    if let Value::Bool(v) = tos {
                        if !v {
                            frame.pc = i;
                            continue;
                        }
                    } else {
                        return Err(operator_error!(code, tos));
                    }
                }
                OpCode::JumpIfTrueOrPop(JumpTarget(i)) => {
                    let tos = frame.stack.last().unwrap();
                    if let Value::Bool(v) = tos {
                        if *v {
                            frame.pc = i;
                            continue;
                        } else {
                            frame.stack.pop().unwrap();
                        }
                    } else {
                        return Err(operator_error!(code, tos));
                    }
                }
                OpCode::JumpIfFalseOrPop(JumpTarget(i)) => {
                    let tos = frame.stack.last().unwrap();
                    if let Value::Bool(v) = tos {
                        if !*v {
                            frame.pc = i;
                            continue;
                        } else {
                            frame.stack.pop().unwrap();
                        }
                    } else {
                        return Err(operator_error!(code, tos));
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
                    if function.kind == FunctionKind::Do {
                        let table = Table::new(&ctx);
                        for i in 0..function.local_names.len() {
                            table.set(ctx, function.local_names[i], frame.locals[i]);
                        }
                        frame.stack.push(Value::Table(table));
                    }
                    self.return_upper(ctx);
                    break;
                }
                OpCode::Throw => {
                    let tos = frame.stack.pop().unwrap();
                    return Err(Error::new(LuciaError::Error(tos)));
                }
                OpCode::ReturnCall(i) => {
                    let args = frame.stack.split_off(frame.stack.len() - i);
                    let callee = frame.stack.pop().unwrap();
                    self.tail_call(ctx, meta_ops::call(ctx, callee)?, args)?;
                    break;
                }
                OpCode::JumpTarget(_) => {
                    panic!("unexpected opcode: JumpTarget")
                }
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
