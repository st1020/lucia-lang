use core::ptr::NonNull;
use std::alloc::{dealloc, Layout};
use std::cmp::Ordering;
use std::collections::HashMap;

use crate::code::{Code, ConstlValue, FunctionKind};
use crate::errors::{
    BuiltinError, Error, ProgramError, Result, RuntimeError, RuntimeErrorKind, TracebackFrame,
};
use crate::gc::{Gc, Heap, RefCell, Trace};
use crate::libs;
use crate::objects::table::Iter;
use crate::objects::*;
use crate::opcode::{JumpTarget, OpCode};
use crate::{
    call_arguments_error, check_args, not_callable_error, operator_error, table, try_as_value_type,
};

#[macro_export]
macro_rules! return_error {
    ($lvm:expr, $value:expr) => {
        return Ok($value.into_table_value($lvm))
    };
}

#[macro_export]
macro_rules! try_get {
    (($vec:expr)[$index:expr], $error:expr) => {
        $vec.get($index).ok_or_else(|| $error)?
    };
}

#[macro_export]
macro_rules! try_set {
    (($vec:expr)[$index:expr] = $val:expr, $error:expr) => {
        if $index < $vec.len() {
            $vec[$index] = $val;
        } else {
            return Err($error);
        }
    };
}

#[macro_export]
macro_rules! get_metamethod {
    ($lvm:expr, $val:ident, $name:expr) => {{
        if let Some(t) = $val.clone().as_table() {
            match t.metatable.as_table() {
                Some(t) => t.get(&$lvm.get_builtin_str($name)).copied(),
                None => None,
            }
        } else if let Some(t) = $val.as_userdata() {
            t.metatable.get(&$lvm.get_builtin_str($name)).copied()
        } else {
            None
        }
    }};
}

#[derive(Clone)]
pub struct Frame {
    pc: usize,
    closure: Gc<RefCell<Closure>>,
    prev_frame: Option<NonNull<Frame>>,
    locals: Vec<Value>,
    operate_stack: Vec<Value>,
}

impl Frame {
    pub fn new(
        closure: Gc<RefCell<Closure>>,
        prev_frame: Option<NonNull<Frame>>,
        locals_count: usize,
        stack_size: usize,
    ) -> Self {
        Frame {
            pc: 0,
            closure,
            prev_frame,
            locals: vec![Value::Null; locals_count],
            operate_stack: Vec::with_capacity(stack_size),
        }
    }

    pub fn run(&mut self, lvm: &mut Lvm) -> Result<Value> {
        macro_rules! call {
            ($arg_num:expr) => {{
                let args = self
                    .operate_stack
                    .split_off(self.operate_stack.len() - $arg_num);
                let callee = try_stack!(self.operate_stack.pop());
                lvm.call(callee, args)
            }};
        }

        macro_rules! stack_error {
            () => {
                Error::RuntimeError(RuntimeError {
                    kind: RuntimeErrorKind::StackError,
                    traceback: lvm.traceback(),
                })
            };
        }

        macro_rules! program_error {
            ($value:expr) => {
                Error::RuntimeError(RuntimeError {
                    kind: RuntimeErrorKind::ProgramError($value),
                    traceback: lvm.traceback(),
                })
            };
        }

        macro_rules! throw_error {
            ($value:expr) => {
                Error::RuntimeError(RuntimeError {
                    kind: RuntimeErrorKind::ThrowError($value),
                    traceback: lvm.traceback(),
                })
            };
        }

        macro_rules! try_stack {
            ($expr:expr) => {
                $expr.ok_or_else(|| stack_error!())?
            };
        }

        macro_rules! return_error {
            ($value:expr) => {
                $crate::return_error!(lvm, $value)
            };
        }

        macro_rules! bin_op {
            ($op: tt, $name:expr, $operator:expr) => {{
                let tos = try_stack!(self.operate_stack.pop());
                let tos1 = try_stack!(self.operate_stack.pop());
                if let Some(v) = get_metamethod!(lvm, tos1, $name) {
                    self.operate_stack.push(lvm.call(v, vec![tos1, tos])?);
                } else {
                    self.operate_stack.push(match (tos1, tos) {
                        (Value::Int(v1), Value::Int(v2)) => Value::Int(v1 $op v2),
                        (Value::Float(v1), Value::Float(v2)) => Value::Float(v1 $op v2),
                        _ => return_error!(operator_error!($operator.clone(), tos1, tos)),
                    });
                }
            }};
        }

        macro_rules! eq_ne {
            ($op: tt, $name:expr) => {{
                let tos = try_stack!(self.operate_stack.pop());
                let tos1 = try_stack!(self.operate_stack.pop());
                if let Some(v) = get_metamethod!(lvm, tos1, $name) {
                    self.operate_stack.push(lvm.call(v, vec![tos1, tos])?);
                } else {
                    self.operate_stack.push(Value::Bool(tos1 $op tos));
                }
            }};
        }

        macro_rules! compare {
            ($op: tt, $name:expr, $operator:expr) => {{
                let tos = try_stack!(self.operate_stack.pop());
                let tos1 = try_stack!(self.operate_stack.pop());
                if let Some(v) = get_metamethod!(lvm, tos1, $name) {
                    self.operate_stack.push(lvm.call(v, vec![tos1, tos])?);
                } else {
                    self.operate_stack.push(Value::Bool(match (tos1, tos) {
                        (Value::Int(v1), Value::Int(v2)) => v1 $op v2,
                        (Value::Float(v1), Value::Float(v2)) => v1 $op v2,
                        _ => return_error!(operator_error!($operator.clone(), tos1, tos)),
                    }));
                }
            }};
        }

        macro_rules! get_table {
            ($name:expr, $operator:expr) => {{
                let tos = try_stack!(self.operate_stack.pop());
                let tos1 = try_stack!(self.operate_stack.pop());
                if let Some(v) = get_metamethod!(lvm, tos1, $name) {
                    self.operate_stack.push(lvm.call(v, vec![tos1, tos])?);
                } else if let Some(t) = tos1.as_table() {
                    self.operate_stack
                        .push(t.get(&tos).copied().unwrap_or(Value::Null));
                } else {
                    return_error!(operator_error!($operator.clone(), tos1));
                }
            }};
        }

        macro_rules! set_table {
            ($name:expr, $operator:expr) => {{
                let tos = try_stack!(self.operate_stack.pop());
                let tos1 = try_stack!(self.operate_stack.pop());
                let tos2 = try_stack!(self.operate_stack.pop());
                if let Some(v) = get_metamethod!(lvm, tos1, $name) {
                    self.operate_stack.push(lvm.call(v, vec![tos1, tos, tos2])?);
                } else if let Some(mut t) = tos1.as_table_mut() {
                    t.set(&tos, tos2);
                } else {
                    return_error!(operator_error!($operator.clone(), tos1));
                }
            }};
        }

        let mut closure = unsafe { self.closure.ptr.as_ref().data.borrow_mut() };
        loop {
            let code = try_get!(
                (closure.function.code)[self.pc],
                program_error!(ProgramError::CodeIndexError(self.pc))
            )
            .clone();
            // println!("{} {} {:?}", self.pc, code, self.operate_stack);
            match code {
                OpCode::Pop => {
                    try_stack!(self.operate_stack.pop());
                }
                OpCode::Dup => {
                    self.operate_stack
                        .push(*try_stack!(self.operate_stack.last()));
                }
                OpCode::DupTwo => {
                    let tos = try_stack!(self.operate_stack.pop());
                    let tos1 = try_stack!(self.operate_stack.pop());
                    self.operate_stack.push(tos1);
                    self.operate_stack.push(tos);
                    self.operate_stack.push(tos1);
                    self.operate_stack.push(tos);
                }
                OpCode::RotTwo => {
                    let tos = try_stack!(self.operate_stack.pop());
                    let tos1 = try_stack!(self.operate_stack.pop());
                    self.operate_stack.push(tos);
                    self.operate_stack.push(tos1);
                }
                OpCode::RotThree => {
                    let tos = try_stack!(self.operate_stack.pop());
                    let tos1 = try_stack!(self.operate_stack.pop());
                    let tos2 = try_stack!(self.operate_stack.pop());
                    self.operate_stack.push(tos);
                    self.operate_stack.push(tos2);
                    self.operate_stack.push(tos1);
                }
                OpCode::LoadLocal(i) => {
                    self.operate_stack.push(*try_get!(
                        (self.locals)[i],
                        program_error!(ProgramError::LocalNameError(i))
                    ));
                }
                OpCode::LoadGlobal(i) => {
                    let t = try_get!(
                        (closure.function.global_names)[i],
                        program_error!(ProgramError::GlobalNameError(i))
                    );
                    self.operate_stack.push(
                        lvm.get_global_variable(t)
                            .unwrap_or_else(|| lvm.get_builtin_variable(t).unwrap_or(Value::Null)),
                    );
                }
                OpCode::LoadUpvalue(i) => {
                    let (_, func_count, upvalue_id) = try_get!(
                        (closure.function.upvalue_names)[i],
                        program_error!(ProgramError::UpvalueError(i))
                    );
                    let (func_count, upvalue_id) = (*func_count, *upvalue_id);
                    if func_count == 0 {
                        self.operate_stack.push(*try_get!(
                            (closure.upvalues)[upvalue_id],
                            program_error!(ProgramError::UpvalueError(i))
                        ));
                    } else {
                        let mut base_closure = closure.base_closure;
                        for _ in 0..(func_count - 1) {
                            base_closure = base_closure
                                .ok_or_else(|| program_error!(ProgramError::UpvalueError(i)))?
                                .borrow()
                                .base_closure;
                        }
                        self.operate_stack.push(*try_get!(
                            (base_closure
                                .ok_or_else(|| program_error!(ProgramError::UpvalueError(i)))?
                                .borrow()
                                .upvalues)[upvalue_id],
                            program_error!(ProgramError::UpvalueError(i))
                        ));
                    }
                }
                OpCode::LoadConst(i) => {
                    self.operate_stack.push(
                        match try_get!(
                            (closure.function.consts)[i],
                            program_error!(ProgramError::ConstError(i))
                        ) {
                            ConstlValue::Null => Value::Null,
                            ConstlValue::Bool(v) => Value::Bool(*v),
                            ConstlValue::Int(v) => Value::Int(*v),
                            ConstlValue::Float(v) => Value::Float(*v),
                            ConstlValue::Str(v) => lvm.new_str_value(v.clone()),
                            ConstlValue::Func(v) => {
                                let base_closure = if v.kind == FunctionKind::Closure {
                                    Some(self.closure)
                                } else {
                                    None
                                };
                                lvm.new_closure_value(Closure::new(v.clone(), base_closure))
                            }
                        },
                    );
                }
                OpCode::StoreLocal(i) => {
                    try_set!(
                        (self.locals)[i] = try_stack!(self.operate_stack.pop()),
                        program_error!(ProgramError::LocalNameError(i))
                    );
                }
                OpCode::StoreGlobal(i) => {
                    lvm.set_global_variable(
                        try_get!(
                            (closure.function.global_names)[i],
                            program_error!(ProgramError::GlobalNameError(i))
                        )
                        .clone(),
                        try_stack!(self.operate_stack.pop()),
                    );
                }
                OpCode::StoreUpvalue(i) => {
                    let (_, func_count, upvalue_id) = try_get!(
                        (closure.function.upvalue_names)[i],
                        program_error!(ProgramError::UpvalueError(i))
                    );
                    let (func_count, upvalue_id) = (*func_count, *upvalue_id);
                    if func_count == 0 {
                        closure.upvalues[upvalue_id] = try_stack!(self.operate_stack.pop());
                    } else {
                        let mut base_closure = closure.base_closure;
                        for _ in 0..(func_count - 1) {
                            base_closure = base_closure
                                .ok_or_else(|| program_error!(ProgramError::UpvalueError(i)))?
                                .borrow()
                                .base_closure;
                        }
                        base_closure
                            .ok_or_else(|| program_error!(ProgramError::UpvalueError(i)))?
                            .borrow_mut()
                            .upvalues[upvalue_id] = try_stack!(self.operate_stack.pop());
                    }
                }
                OpCode::Import(i) => {
                    if let ConstlValue::Str(v) = try_get!(
                        (closure.function.consts)[i],
                        program_error!(ProgramError::ConstError(i))
                    ) {
                        if let Some(module) = lvm.libs.get(v) {
                            self.operate_stack.push(*module);
                        } else {
                            return Err(program_error!(ProgramError::ConstError(i)));
                        }
                    } else {
                        return Err(program_error!(ProgramError::ConstError(i)));
                    }
                }
                OpCode::ImportFrom(i) => {
                    let tos = *try_stack!(self.operate_stack.last());
                    if let Some(module) = tos.as_table() {
                        if let ConstlValue::Str(t) = try_get!(
                            (closure.function.consts)[i],
                            program_error!(ProgramError::ConstError(i))
                        ) {
                            self.operate_stack.push(
                                module
                                    .get(&lvm.get_builtin_str(t))
                                    .copied()
                                    .unwrap_or(Value::Null),
                            );
                        } else {
                            return Err(program_error!(ProgramError::ConstError(i)));
                        }
                    } else {
                        return_error!(operator_error!(code, tos));
                    };
                }
                OpCode::ImportGlob => {
                    let tos = try_stack!(self.operate_stack.pop());
                    if let Some(module) = tos.as_table() {
                        for (k, v) in module.iter() {
                            if let Some(k) = k.as_str() {
                                lvm.set_global_variable(k.to_string(), *v);
                            }
                        }
                    } else {
                        return_error!(operator_error!(code, tos));
                    };
                }
                OpCode::BuildTable(i) => {
                    if self.operate_stack.len() >= i * 2 {
                        let temp = self
                            .operate_stack
                            .split_off(self.operate_stack.len() - i * 2);
                        let mut table: Table = Table::new();
                        for i in temp.chunks(2) {
                            table.set(&i[0], i[1]);
                        }
                        self.operate_stack.push(lvm.new_table_value(table));
                    } else {
                        return Err(stack_error!());
                    }
                }
                OpCode::GetAttr => get_table!("__getattr__", code),
                OpCode::GetItem => get_table!("__getitem__", code),
                OpCode::GetMeta => {
                    let tos = try_stack!(self.operate_stack.pop());
                    if let Some(t) = tos.as_table() {
                        self.operate_stack.push(t.metatable);
                    } else {
                        return_error!(operator_error!(code, tos));
                    };
                }
                OpCode::SetAttr => set_table!("__setattr__", code),
                OpCode::SetItem => set_table!("__setitem__", code),
                OpCode::SetMeta => {
                    let tos = try_stack!(self.operate_stack.pop());
                    let tos1 = try_stack!(self.operate_stack.pop());
                    if let Some(mut t) = tos.as_table_mut() {
                        if tos1.is_table() || tos1.is_null() {
                            t.metatable = tos1;
                        } else {
                            return_error!(operator_error!(code, tos, tos1));
                        }
                    } else {
                        return_error!(operator_error!(code, tos, tos1));
                    };
                }
                OpCode::Neg => {
                    let tos = try_stack!(self.operate_stack.pop());
                    if let Some(v) = get_metamethod!(lvm, tos, "__neg__") {
                        self.operate_stack.push(lvm.call(v, vec![tos])?);
                    } else {
                        self.operate_stack.push(match tos {
                            Value::Int(v) => Value::Int(-v),
                            Value::Float(v) => Value::Float(-v),
                            _ => return_error!(operator_error!(code, tos)),
                        });
                    }
                }
                OpCode::Not => {
                    let tos = try_stack!(self.operate_stack.pop());
                    if let Some(v) = tos.as_bool() {
                        self.operate_stack.push(Value::Bool(v));
                    } else {
                        return_error!(operator_error!(code, tos));
                    }
                }
                OpCode::Add => {
                    let tos = try_stack!(self.operate_stack.pop());
                    let tos1 = try_stack!(self.operate_stack.pop());
                    if let Some(v) = get_metamethod!(lvm, tos1, "__add__") {
                        self.operate_stack.push(lvm.call(v, vec![tos1, tos])?);
                    } else if let Some(v1) = tos1.as_str() {
                        if let Some(v2) = tos.as_str() {
                            self.operate_stack
                                .push(lvm.new_str_value(v1.to_string() + v2));
                        }
                    } else {
                        self.operate_stack.push(match (tos1, tos) {
                            (Value::Int(v1), Value::Int(v2)) => Value::Int(v1 + v2),
                            (Value::Float(v1), Value::Float(v2)) => Value::Float(v1 + v2),
                            _ => return_error!(operator_error!(code, tos1, tos)),
                        });
                    }
                }
                OpCode::Sub => bin_op!(-, "__sub__", code),
                OpCode::Mul => bin_op!(*, "__mul__", code),
                OpCode::Div => bin_op!(/, "__div__", code),
                OpCode::Mod => bin_op!(%, "__mod__", code),
                OpCode::Eq => eq_ne!(==, "__eq__"),
                OpCode::Ne => eq_ne!(!=, "__ne__"),
                OpCode::Gt => compare!(>, "__gt__", code),
                OpCode::Ge => compare!(>=, "__ge__", code),
                OpCode::Lt => compare!(<, "__lt__", code),
                OpCode::Le => compare!(<=, "__le__", code),
                OpCode::Is => {
                    let tos = try_stack!(self.operate_stack.pop());
                    let tos1 = try_stack!(self.operate_stack.pop());
                    self.operate_stack.push(Value::Bool(tos1.is(&tos)));
                }
                OpCode::For(JumpTarget(i)) => {
                    let mut tos = *try_stack!(self.operate_stack.last());
                    if let Some(v) = get_metamethod!(lvm, tos, "__iter__") {
                        if !v.is(&tos) {
                            let v = lvm.call(v, vec![tos])?;
                            try_stack!(self.operate_stack.pop());
                            self.operate_stack.push(v);
                            tos = v;
                        }
                    } else if tos.is_table() && get_metamethod!(lvm, tos, "__call__").is_none() {
                        let v = lvm.iter_table(tos)?;
                        try_stack!(self.operate_stack.pop());
                        self.operate_stack.push(v);
                        tos = v;
                    };
                    let return_value = lvm.call(tos, Vec::new())?;
                    if return_value.is_null() {
                        self.pc = i;
                        continue;
                    } else {
                        self.operate_stack.push(return_value);
                    }
                }
                OpCode::Jump(JumpTarget(i)) => {
                    self.pc = i;
                    continue;
                }
                OpCode::JumpIfNull(JumpTarget(i)) => {
                    let tos = try_stack!(self.operate_stack.last());
                    if tos.is_null() {
                        self.pc = i;
                        continue;
                    }
                }
                OpCode::JumpPopIfFalse(JumpTarget(i)) => {
                    let tos = try_stack!(self.operate_stack.pop());
                    if !bool::from(tos) {
                        self.pc = i;
                        continue;
                    }
                }
                OpCode::JumpIfTureOrPop(JumpTarget(i)) => {
                    let tos = try_stack!(self.operate_stack.last());
                    if bool::from(*tos) {
                        self.pc = i;
                        continue;
                    } else {
                        try_stack!(self.operate_stack.pop());
                    }
                }
                OpCode::JumpIfFalseOrPop(JumpTarget(i)) => {
                    let tos = try_stack!(self.operate_stack.last());
                    if !bool::from(*tos) {
                        self.pc = i;
                        continue;
                    } else {
                        try_stack!(self.operate_stack.pop());
                    }
                }
                OpCode::Call(i) => {
                    drop(closure);
                    let return_value = call!(i)?;
                    self.operate_stack.push(return_value);
                    closure = unsafe { self.closure.ptr.as_ref().data.borrow_mut() };
                }
                OpCode::TryCall(i) => {
                    let return_value = call!(i)?;
                    if return_value.is_error() {
                        return Ok(return_value);
                    }
                    self.operate_stack.push(return_value);
                }
                OpCode::Return => {
                    if closure.function.kind == FunctionKind::Do {
                        let mut temp = Table::new();
                        for i in 0..closure.function.local_names.len() {
                            temp.set(
                                &lvm.new_str_value(closure.function.local_names[i].clone()),
                                self.locals[i],
                            )
                        }
                        return Ok(lvm.new_table_value(temp));
                    } else {
                        return Ok(try_stack!(self.operate_stack.pop()));
                    }
                }
                OpCode::Throw => {
                    let mut tos = try_stack!(self.operate_stack.pop());
                    if tos.set_error() {
                        return Ok(tos);
                    } else {
                        return Err(throw_error!(tos));
                    }
                }
                OpCode::ReturnCall(i) => {
                    drop(closure);
                    let mut args = self.operate_stack.split_off(self.operate_stack.len() - i);
                    let mut callee = try_stack!(self.operate_stack.pop());
                    if let Some(v) = get_metamethod!(lvm, callee, "__call__") {
                        args.insert(0, callee);
                        callee = v;
                    }

                    if let Some(f) = callee.as_ext_function() {
                        return f(args, lvm);
                    } else if let Some(mut c) = callee.as_ext_closure_mut() {
                        return (c.func)(args, &mut c.upvalues, lvm);
                    } else if let Some(v) = callee.as_closure() {
                        if let Err(e) = self.set_args(lvm, &v, args) {
                            return_error!(e);
                        }
                        drop(v);
                        self.closure = match callee {
                            Value::Closure(c) => c,
                            _ => panic!("unexpect error"),
                        };
                        closure = unsafe { self.closure.ptr.as_ref().data.borrow_mut() };
                        self.pc = 0;
                        continue;
                    } else {
                        return_error!(not_callable_error!(callee));
                    };
                }
                OpCode::JumpTarget(_) => {
                    return Err(program_error!(ProgramError::UnexpectCodeError(code)))
                }
            }
            self.pc += 1;
            if lvm.gc_status {
                unsafe { lvm.gc() }
                lvm.gc_status = false;
            }
        }
    }
    fn set_args(
        &mut self,
        lvm: &mut Lvm,
        closure: &Closure,
        mut args: Vec<Value>,
    ) -> std::result::Result<(), BuiltinError> {
        let params_num = closure.function.params.len();
        self.operate_stack = vec![Value::Null; params_num];
        match args.len().cmp(&params_num) {
            Ordering::Less => {
                if closure.function.variadic.is_none() {
                    return Err(call_arguments_error!(
                        Some(Box::new(closure.clone())),
                        params_num,
                        args.len()
                    ));
                } else {
                    return Err(call_arguments_error!(
                        Some(Box::new(closure.clone())),
                        (params_num, None),
                        args.len()
                    ));
                }
            }
            Ordering::Equal => {
                self.operate_stack[..params_num].copy_from_slice(&args[..]);
                if closure.function.variadic.is_some() {
                    self.operate_stack.push(lvm.new_table_value(Table::new()));
                }
            }
            Ordering::Greater => {
                if closure.function.variadic.is_none() {
                    return Err(call_arguments_error!(
                        Some(Box::new(closure.clone())),
                        params_num,
                        args.len()
                    ));
                } else {
                    let t = args.split_off(params_num);
                    self.operate_stack[..params_num].copy_from_slice(&args[..]);
                    self.operate_stack
                        .push(lvm.new_table_value(Table::from_iter(t)));
                }
            }
        }
        Ok(())
    }
}

pub struct Lvm {
    pub global_variables: HashMap<String, Value>,
    pub builtin_variables: HashMap<String, Value>,
    pub libs: HashMap<String, Value>,
    pub current_frame: Option<NonNull<Frame>>,
    heap: Heap,
    last_heap_len: usize,
    gc_status: bool,
    builtin_str_value: HashMap<String, Value>,
}

impl Lvm {
    pub fn new() -> Self {
        let mut t = Lvm {
            global_variables: HashMap::new(),
            builtin_variables: libs::builtin::builtin_variables(),
            libs: HashMap::new(),
            current_frame: None,
            heap: Heap::new(),
            last_heap_len: 64,
            gc_status: false,
            builtin_str_value: HashMap::new(),
        };
        t.libs = libs::std_libs(&mut t);
        t
    }

    pub fn run(&mut self, code: Code) -> Result<Value> {
        let callee = self.new_closure_value(Closure::new(code, None));
        self.call(callee, Vec::new())
    }

    #[inline]
    pub fn call(&mut self, mut callee: Value, mut args: Vec<Value>) -> Result<Value> {
        macro_rules! return_error {
            ($value:expr) => {
                return Ok($value.into_table_value(self))
            };
        }

        if let Some(v) = get_metamethod!(self, callee, "__call__") {
            args.insert(0, callee);
            callee = v;
        }

        if let Some(f) = callee.as_ext_function() {
            f(args, self)
        } else if let Some(mut c) = callee.as_ext_closure_mut() {
            (c.func)(args, &mut c.upvalues, self)
        } else if let Some(v) = callee.as_closure() {
            let current_frame = self.current_frame;

            let mut frame = Frame::new(
                match callee {
                    Value::Closure(v) => v,
                    _ => panic!("unexpect error"),
                },
                self.current_frame,
                v.function.local_names.len(),
                v.function.stack_size,
            );
            if let Err(e) = frame.set_args(self, &v, args) {
                return_error!(e);
            }

            drop(v);

            self.current_frame = Some(NonNull::new(&mut frame).unwrap());
            let return_value = frame.run(self);

            self.current_frame = current_frame;
            return_value
        } else {
            return_error!(not_callable_error!(callee));
        }
    }

    #[inline]
    pub fn set_global_variable(&mut self, key: String, value: Value) {
        self.global_variables.insert(key, value);
    }

    #[inline]
    pub fn get_global_variable(&self, key: &str) -> Option<Value> {
        self.global_variables.get(key).copied()
    }

    #[inline]
    pub fn get_builtin_variable(&self, key: &str) -> Option<Value> {
        self.builtin_variables.get(key).copied()
    }

    #[inline]
    pub fn get_builtin_str(&mut self, key: &str) -> Value {
        match self.builtin_str_value.get(key) {
            Some(v) => *v,
            None => {
                let t = self.new_str_value(key.to_string());
                self.builtin_str_value.insert(key.to_string(), t);
                t
            }
        }
    }

    pub fn iter_table(&mut self, table_value: Value) -> Result<Value> {
        let table = try_as_value_type!(self, table_value, Table);
        let mut userdata_table = Table::new();
        userdata_table.set(&self.get_builtin_str("_marker"), table_value);
        userdata_table.set(
            &self.get_builtin_str("__call__"),
            Value::ExtFunction(|mut args, lvm| {
                let (t,) = check_args!(lvm, args, mut UserData);
                let iter = unsafe { (t.ptr as *mut Iter).as_mut().unwrap() };
                Ok(iter
                    .next()
                    .map(|(k, v)| lvm.new_table_value(table![*k, *v]))
                    .unwrap_or(Value::Null))
            }),
        );
        Ok(self.new_userdata_value(UserData::new(
            Box::into_raw(Box::new(table.iter())) as *mut u8,
            userdata_table,
            |userdata| unsafe {
                userdata.ptr.drop_in_place();
                dealloc(userdata.ptr as *mut u8, Layout::new::<Iter>());
            },
        )))
    }

    #[inline]
    pub fn new_gc_object<T: Trace + 'static>(&mut self, value: T) -> Gc<T> {
        if self.heap.len() > self.last_heap_len * 2 && self.current_frame.is_some() {
            self.gc_status = true;
        }
        self.heap.new_gc_object(value)
    }

    #[inline]
    pub fn new_str_value(&mut self, value: String) -> Value {
        Value::Str(self.new_gc_object(value))
    }

    #[inline]
    pub fn new_table_value(&mut self, value: Table) -> Value {
        Value::Table(self.new_gc_object(RefCell::new(value)))
    }

    #[inline]
    pub fn new_userdata_value(&mut self, value: UserData) -> Value {
        Value::UserData(self.new_gc_object(RefCell::new(value)))
    }

    #[inline]
    pub fn new_closure_value(&mut self, value: Closure) -> Value {
        Value::Closure(self.new_gc_object(RefCell::new(value)))
    }

    #[inline]
    pub fn new_ext_closure_value(&mut self, value: ExtClosure) -> Value {
        Value::ExtClosure(self.new_gc_object(RefCell::new(value)))
    }

    pub fn traceback(&self) -> Vec<TracebackFrame> {
        let mut traceback_frames = Vec::new();
        let mut frame = self.current_frame;
        while let Some(f) = frame {
            let f = unsafe { f.as_ref() };
            traceback_frames.push(TracebackFrame {
                pc: f.pc,
                operate_stack: f.operate_stack.clone(),
                closure: unsafe { f.closure.as_ptr().as_ref().unwrap().clone() },
            });
            frame = f.prev_frame;
        }
        traceback_frames
    }

    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn gc(&mut self) {
        // mark
        if let Some(frame) = self.current_frame {
            let mut frame = frame.as_ref();
            loop {
                frame.closure.trace();
                for value in &frame.operate_stack {
                    value.trace();
                }
                match frame.prev_frame {
                    Some(t) => frame = t.as_ref(),
                    None => break,
                }
            }
        }
        for v in self.global_variables.values() {
            v.trace();
        }
        for v in self.builtin_variables.values() {
            v.trace();
        }
        for v in self.builtin_str_value.values() {
            v.trace();
        }
        // sweep
        self.heap.sweep();
        self.last_heap_len = self.heap.len();
    }
}

impl Default for Lvm {
    fn default() -> Self {
        Self::new()
    }
}

// fn set_closure_args(
//     lvm: &mut Lvm,
//     v: &mut Closure,
//     mut args: Vec<Value>,
// ) -> std::result::Result<(), BuiltinError> {
//     let params_num = v.function.params.len();
//     match args.len().cmp(&params_num) {
//         Ordering::Less => {
//             if v.function.variadic.is_none() {
//                 return Err(call_arguments_error!(
//                     Some(Box::new(v.clone())),
//                     params_num,
//                     args.len()
//                 ));
//             } else {
//                 return Err(call_arguments_error!(
//                     Some(Box::new(v.clone())),
//                     (params_num, None),
//                     args.len()
//                 ));
//             }
//         }
//         Ordering::Equal => {
//             v.upvalues[..params_num].copy_from_slice(&args[..]);
//             if v.function.variadic.is_some() {
//                 v.upvalues[params_num] = lvm.new_table_value(Table::new());
//             }
//         }
//         Ordering::Greater => {
//             if v.function.variadic.is_none() {
//                 return Err(call_arguments_error!(
//                     Some(Box::new(v.clone())),
//                     params_num,
//                     args.len()
//                 ));
//             } else {
//                 let t = args.split_off(params_num);
//                 v.upvalues[..params_num].copy_from_slice(&args[..]);
//                 v.upvalues[params_num] = lvm.new_table_value(Table::from_iter(t));
//             }
//         }
//     }
//     Ok(())
// }
