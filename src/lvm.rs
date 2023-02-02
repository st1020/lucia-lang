use core::ptr::NonNull;
use std::alloc::{alloc, dealloc, Layout};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

use crate::codegen::{ConstlValue, FunctionKind, JumpTarget, OpCode, Program};
use crate::errors::{
    BuiltinError, Error, ProgramError, Result, RuntimeError, RuntimeErrorKind, TracebackFrame,
};
use crate::libs;
use crate::objects::*;
use crate::{build_table_error, call_arguments_error, not_callable_error, operator_error};

#[macro_export]
macro_rules! error {
    ($value:expr) => {{
        let t = $value;
        if let $crate::objects::Value::GCObject(v) = t {
            #[allow(unused_unsafe)]
            unsafe {
                (*v).is_error = true
            }
        }
        t
    }};
}

#[macro_export]
macro_rules! return_error {
    ($value:expr, $lvm:expr) => {
        return Ok($value.into_table_value($lvm))
    };
}

#[macro_export]
macro_rules! try_convert {
    ($lvm:expr, $expr:expr, $as:tt, $to:tt) => {
        match $expr.$as() {
            Some(val) => val,
            None => $crate::return_error!(
                $crate::type_convert_error!($expr.value_type(), $crate::objects::ValueType::$to),
                $lvm
            ),
        }
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
    ($lvm:expr, $val:expr, $name:expr) => {
        $val.metatable()
            .and_then(|t| t.get(&$lvm.get_builtin_str($name)))
    };
}

#[derive(Debug, Clone)]
pub struct Frame {
    pc: usize,
    closure: NonNull<GCObject>,
    operate_stack: Vec<Value>,
    prev_frame: Option<NonNull<Frame>>,
}

impl Frame {
    pub fn new(
        closure: NonNull<GCObject>,
        prev_frame: Option<NonNull<Frame>>,
        stack_size: usize,
    ) -> Self {
        Frame {
            pc: 0,
            closure,
            operate_stack: Vec::with_capacity(stack_size),
            prev_frame,
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
                $crate::return_error!($value, lvm)
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
                        _ => return_error!(operator_error!($operator, tos1, tos)),
                    });
                }
            }};
        }

        macro_rules! eq_ne {
            ($op: tt, $name:expr, $operator:expr) => {{
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
                        _ => return_error!(operator_error!($operator, tos1, tos)),
                    }));
                }
            }};
        }

        macro_rules! get_table {
            ($name:expr) => {{
                let tos = try_stack!(self.operate_stack.pop());
                let tos1 = try_stack!(self.operate_stack.pop());
                if let Some(v) = get_metamethod!(lvm, tos1, $name) {
                    self.operate_stack.push(lvm.call(v, vec![tos1, tos])?);
                } else if let Some(t) = tos1.as_table() {
                    self.operate_stack.push(t.get(&tos).unwrap_or(Value::Null));
                } else {
                    return_error!(crate::type_convert_error!(
                        tos1.value_type(),
                        crate::objects::ValueType::Table
                    ));
                }
            }};
        }

        macro_rules! set_table {
            ($name:expr) => {{
                let tos = try_stack!(self.operate_stack.pop());
                let mut tos1 = try_stack!(self.operate_stack.pop());
                let tos2 = try_stack!(self.operate_stack.pop());
                if let Some(v) = get_metamethod!(lvm, tos1, $name) {
                    self.operate_stack.push(lvm.call(v, vec![tos1, tos, tos2])?);
                } else if let Some(t) = tos1.as_table_mut() {
                    t.set(&tos, tos2);
                } else {
                    return_error!($crate::type_convert_error!(
                        tos1.value_type(),
                        $crate::objects::ValueType::Table
                    ));
                }
            }};
        }

        let mut closure = unsafe {
            match &mut self.closure.as_mut().kind {
                GCObjectKind::Closure(v) => v,
                _ => panic!("unexpect error"),
            }
        };
        loop {
            let code = try_get!(
                (closure.function.code_list)[self.pc],
                program_error!(ProgramError::CodeIndexError(self.pc))
            );
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
                        (closure.variables)[*i],
                        program_error!(ProgramError::LocalNameError(*i))
                    ));
                }
                OpCode::LoadGlobal(i) => {
                    let t = try_get!(
                        (closure.function.global_names)[*i],
                        program_error!(ProgramError::GlobalNameError(*i))
                    );
                    self.operate_stack.push(
                        lvm.get_global_variable(t)
                            .unwrap_or_else(|| lvm.get_builtin_variable(t).unwrap_or(Value::Null)),
                    );
                }
                OpCode::LoadUpvalue(i) => {
                    let (_, func_count, upvalue_id) = try_get!(
                        (closure.function.upvalue_names)[*i],
                        program_error!(ProgramError::UpvalueError(*i))
                    );
                    let mut base_closure = closure.base_closure;
                    for _ in 0..*func_count {
                        base_closure = match unsafe {
                            &base_closure
                                .ok_or_else(|| program_error!(ProgramError::UpvalueError(*i)))?
                                .as_ref()
                                .kind
                        } {
                            GCObjectKind::Closure(v) => v.base_closure,
                            _ => return Err(program_error!(ProgramError::UpvalueError(*i))),
                        };
                    }
                    match unsafe {
                        &base_closure
                            .ok_or_else(|| program_error!(ProgramError::UpvalueError(*i)))?
                            .as_ref()
                            .kind
                    } {
                        GCObjectKind::Closure(v) => self.operate_stack.push(*try_get!(
                            (v.variables)[*upvalue_id],
                            program_error!(ProgramError::UpvalueError(*i))
                        )),
                        _ => return Err(program_error!(ProgramError::UpvalueError(*i))),
                    };
                }
                OpCode::LoadConst(i) => {
                    let module = try_get!(
                        (lvm.modules)[closure.module_id],
                        program_error!(ProgramError::ModuleError(closure.module_id))
                    );
                    let t = try_get!(
                        (module.const_list)[*i],
                        program_error!(ProgramError::ConstError(*i))
                    )
                    .clone();
                    let v = match t {
                        ConstlValue::Null => Value::Null,
                        ConstlValue::Bool(v) => Value::Bool(v),
                        ConstlValue::Int(v) => Value::Int(v),
                        ConstlValue::Float(v) => Value::Float(v),
                        ConstlValue::Str(v) => lvm.new_str_value(v),
                        ConstlValue::Func(func_id) => {
                            let f = try_get!(
                                (module.func_list)[func_id],
                                program_error!(ProgramError::FuncListError(func_id))
                            )
                            .clone();
                            let base_closure = if f.kind == FunctionKind::Closure {
                                Some(self.closure)
                            } else {
                                None
                            };
                            lvm.new_closure_value(Closure::new(closure.module_id, f, base_closure))
                        }
                    };
                    self.operate_stack.push(v);
                }
                OpCode::StoreLocal(i) => {
                    try_set!(
                        (closure.variables)[*i] = try_stack!(self.operate_stack.pop()),
                        program_error!(ProgramError::LocalNameError(*i))
                    );
                }
                OpCode::StoreGlobal(i) => {
                    lvm.set_global_variable(
                        try_get!(
                            (closure.function.global_names)[*i],
                            program_error!(ProgramError::GlobalNameError(*i))
                        )
                        .clone(),
                        try_stack!(self.operate_stack.pop()),
                    );
                }
                OpCode::StoreUpvalue(i) => {
                    let (_, func_count, upvalue_id) = try_get!(
                        (closure.function.upvalue_names)[*i],
                        program_error!(ProgramError::UpvalueError(*i))
                    );
                    let mut base_closure = closure.base_closure;
                    for _ in 0..*func_count {
                        base_closure = match unsafe {
                            &base_closure
                                .ok_or_else(|| program_error!(ProgramError::UpvalueError(*i)))?
                                .as_ref()
                                .kind
                        } {
                            GCObjectKind::Closure(v) => v.base_closure,
                            _ => return Err(program_error!(ProgramError::UpvalueError(*i))),
                        };
                    }
                    match unsafe {
                        &mut base_closure
                            .ok_or_else(|| program_error!(ProgramError::UpvalueError(*i)))?
                            .as_mut()
                            .kind
                    } {
                        GCObjectKind::Closure(v) => {
                            v.variables[*upvalue_id] = try_stack!(self.operate_stack.pop())
                        }
                        _ => return Err(program_error!(ProgramError::UpvalueError(*i))),
                    };
                }
                OpCode::Import(i) => {
                    if let ConstlValue::Str(v) = try_get!(
                        (lvm.modules[closure.module_id].const_list)[*i],
                        program_error!(ProgramError::ConstError(*i))
                    )
                    .clone()
                    {
                        if let Some(module) = lvm.libs.get(&v).cloned() {
                            self.operate_stack.push(module);
                        } else {
                            let mut path = PathBuf::new();
                            if let Some(v) = lvm.get_global_variable("__module_path__") {
                                path.push(try_convert!(lvm, v, as_str, Str));
                            }
                            path.push(v);
                            path.set_extension("lucia");
                            self.operate_stack
                                .push(match fs::read_to_string(path.clone()) {
                                    Ok(input_file) => {
                                        lvm.modules.push(Program::try_from(&input_file)?);
                                        lvm.run_module(lvm.modules.len() - 1)?
                                    }
                                    Err(_) => BuiltinError::ImportError(format!(
                                        "can not read file: {}",
                                        path.to_str().unwrap_or("unknown")
                                    ))
                                    .into_table_value(lvm),
                                });
                        }
                    } else {
                        return Err(program_error!(ProgramError::ConstError(*i)));
                    }
                }
                OpCode::ImportFrom(i) => {
                    let module =
                        try_convert!(lvm, try_stack!(self.operate_stack.last()), as_table, Table);
                    if let ConstlValue::Str(t) = &try_get!(
                        (lvm.modules[closure.module_id].const_list)[*i],
                        program_error!(ProgramError::ConstError(*i))
                    )
                    .clone()
                    {
                        self.operate_stack.push(
                            module
                                .raw_get(&lvm.get_builtin_str(t))
                                .unwrap_or(Value::Null),
                        );
                    } else {
                        return Err(program_error!(ProgramError::ConstError(*i)));
                    }
                }
                OpCode::ImportGlob => {
                    let tos = try_stack!(self.operate_stack.pop());
                    let module = try_convert!(lvm, tos, as_table, Table);
                    for (k, v) in module.clone() {
                        lvm.set_global_variable(try_convert!(lvm, k, as_str, Str).to_string(), v);
                    }
                }
                OpCode::BuildTable(i) => {
                    if self.operate_stack.len() >= *i * 2 {
                        let temp = self
                            .operate_stack
                            .split_off(self.operate_stack.len() - *i * 2);
                        let mut table: Table = Table::new();
                        for i in temp.chunks(2) {
                            if let Value::GCObject(_) = i[0] {
                                if !i[0].is_str() {
                                    return_error!(build_table_error!(i[0]));
                                }
                            }
                            table.set(&i[0], i[1]);
                        }
                        self.operate_stack.push(lvm.new_table_value(table));
                    } else {
                        return Err(stack_error!());
                    }
                }
                OpCode::GetAttr => get_table!("__getattr__"),
                OpCode::GetItem => get_table!("__getitem__"),
                OpCode::GetMeta => {
                    let tos = try_stack!(self.operate_stack.pop());
                    if let Some(t) = tos.as_table() {
                        self.operate_stack.push(t.metatable);
                    } else {
                        return_error!(crate::type_convert_error!(
                            tos.value_type(),
                            crate::objects::ValueType::Table
                        ));
                    }
                }
                OpCode::SetAttr => set_table!("__setattr__"),
                OpCode::SetItem => set_table!("__setitem__"),
                OpCode::SetMeta => {
                    let mut tos = try_stack!(self.operate_stack.pop());
                    let tos1 = try_stack!(self.operate_stack.pop());
                    if let Some(t) = tos.as_table_mut() {
                        t.metatable = tos1;
                    } else {
                        return_error!(crate::type_convert_error!(
                            tos.value_type(),
                            crate::objects::ValueType::Table
                        ));
                    }
                }
                OpCode::Neg => {
                    let tos = try_stack!(self.operate_stack.pop());
                    if let Some(v) = get_metamethod!(lvm, tos, "__neg__") {
                        self.operate_stack.push(lvm.call(v, vec![tos])?);
                    } else {
                        self.operate_stack.push(match tos {
                            Value::Int(v) => Value::Int(-v),
                            Value::Float(v) => Value::Float(-v),
                            _ => return_error!(operator_error!(code.clone(), tos)),
                        });
                    }
                }
                OpCode::Not => {
                    let tos = try_stack!(self.operate_stack.pop());
                    self.operate_stack
                        .push(Value::Bool(!try_convert!(lvm, tos, as_bool, Bool)));
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
                            _ => return_error!(operator_error!(code.clone(), tos1, tos)),
                        });
                    }
                }
                OpCode::Sub => bin_op!(-, "__sub__", code.clone()),
                OpCode::Mul => bin_op!(*, "__mul__", code.clone()),
                OpCode::Div => bin_op!(/, "__div__", code.clone()),
                OpCode::Mod => bin_op!(%, "__mod__", code.clone()),
                OpCode::Eq => eq_ne!(==, "__eq__", code.clone()),
                OpCode::Ne => eq_ne!(!=, "__ne__", code.clone()),
                OpCode::Gt => compare!(>, "__gt__", code.clone()),
                OpCode::Ge => compare!(>=, "__ge__", code.clone()),
                OpCode::Lt => compare!(<, "__lt__", code.clone()),
                OpCode::Le => compare!(<=, "__le__", code.clone()),
                OpCode::Is => {
                    let tos = try_stack!(self.operate_stack.pop());
                    let tos1 = try_stack!(self.operate_stack.pop());
                    self.operate_stack.push(Value::Bool(tos1.is(&tos)));
                }
                OpCode::For(JumpTarget(i)) => {
                    let return_value =
                        lvm.call(*try_stack!(self.operate_stack.last()), Vec::new())?;
                    if return_value.is_null() {
                        self.pc = *i;
                        continue;
                    } else {
                        self.operate_stack.push(return_value);
                    }
                }
                OpCode::Jump(JumpTarget(i)) => {
                    self.pc = *i;
                    continue;
                }
                OpCode::JumpIfNull(JumpTarget(i)) => {
                    let tos = try_stack!(self.operate_stack.last());
                    if tos.is_null() {
                        self.pc = *i;
                        continue;
                    }
                }
                OpCode::JumpPopIfFalse(JumpTarget(i)) => {
                    let tos = try_stack!(self.operate_stack.pop());
                    if let Some(v) = tos.as_bool() {
                        if !v {
                            self.pc = *i;
                            continue;
                        }
                    }
                }
                OpCode::JumpIfTureOrPop(JumpTarget(i)) => {
                    let tos = try_stack!(self.operate_stack.last());
                    if let Some(v) = tos.as_bool() {
                        if v {
                            self.pc = *i;
                            continue;
                        } else {
                            try_stack!(self.operate_stack.pop());
                        }
                    }
                }
                OpCode::JumpIfFalseOrPop(JumpTarget(i)) => {
                    let tos = try_stack!(self.operate_stack.last());
                    if let Some(v) = tos.as_bool() {
                        if !v {
                            self.pc = *i;
                            continue;
                        } else {
                            try_stack!(self.operate_stack.pop());
                        }
                    }
                }
                OpCode::Call(i) => {
                    let return_value = call!(*i)?;
                    self.operate_stack.push(return_value);
                }
                OpCode::TryCall(i) => {
                    let return_value = call!(*i)?;
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
                                closure.variables[i],
                            )
                        }
                        return Ok(lvm.new_table_value(temp));
                    } else {
                        return Ok(try_stack!(self.operate_stack.pop()));
                    }
                }
                OpCode::Throw => {
                    let tos = try_stack!(self.operate_stack.pop());
                    if tos.is_str() || tos.is_table() || tos.is_userdata() {
                        return Ok(error!(tos));
                    } else {
                        return Err(throw_error!(tos));
                    }
                }
                OpCode::ReturnCall(i) => {
                    let mut args = self.operate_stack.split_off(self.operate_stack.len() - i);
                    let mut callee = try_stack!(self.operate_stack.pop());

                    if let Some(t) = callee.metatable() {
                        args.insert(0, callee);
                        callee = match t.get(&lvm.get_builtin_str("__call__")) {
                            Some(v) => v,
                            None => return_error!(not_callable_error!(callee)),
                        };
                    }

                    if let Some(f) = callee.as_ext_function() {
                        return f(args, lvm);
                    } else if let Some(c) = callee.as_ext_closure_mut() {
                        return (c.func)(args, &mut c.upvalues, lvm);
                    } else if let Some(v) = callee.clone().as_closure_mut() {
                        if let Err(e) = set_closure_args(lvm, v, args) {
                            return_error!(e);
                        }

                        self.closure = match callee {
                            Value::GCObject(gc_obj) => NonNull::new(gc_obj).unwrap(),
                            _ => panic!("unexpect error"),
                        };
                        closure = unsafe {
                            match &mut self.closure.as_mut().kind {
                                GCObjectKind::Closure(v) => v,
                                _ => panic!("unexpect error"),
                            }
                        };
                        self.operate_stack = Vec::with_capacity(v.function.stack_size);
                        self.pc = 0;
                        continue;
                    } else {
                        return_error!(not_callable_error!(callee));
                    }
                }
                OpCode::JumpTarget(_) => {
                    return Err(program_error!(ProgramError::UnexpectCodeError(
                        code.clone()
                    )))
                }
            }
            self.pc += 1;
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lvm {
    pub modules: Vec<Program>,
    pub global_variables: HashMap<String, Value>,
    pub builtin_variables: HashMap<String, Value>,
    pub libs: HashMap<String, Value>,
    pub current_frame: Option<NonNull<Frame>>,
    mem_layout: Layout,
    heap: Vec<*mut GCObject>,
    last_heap_len: usize,
    builtin_str_value: HashMap<String, *mut GCObject>,
}

impl Lvm {
    pub fn new() -> Self {
        let mut t = Lvm {
            modules: Vec::new(),
            global_variables: HashMap::new(),
            builtin_variables: libs::builtin::builtin_variables(),
            libs: HashMap::new(),
            current_frame: None,
            mem_layout: Layout::new::<GCObject>(),
            heap: Vec::new(),
            last_heap_len: 64,
            builtin_str_value: HashMap::new(),
        };
        t.libs = libs::std_libs(&mut t);
        t
    }

    pub fn run(&mut self) -> Result<Value> {
        self.run_module(0)
    }

    pub fn run_module(&mut self, module_id: usize) -> Result<Value> {
        macro_rules! program_error {
            ($value:expr) => {
                Error::RuntimeError(RuntimeError {
                    kind: RuntimeErrorKind::ProgramError($value),
                    traceback: self.traceback(),
                })
            };
        }
        let module = try_get!(
            (self.modules)[module_id],
            program_error!(ProgramError::ModuleError(module_id))
        );
        let func = try_get!(
            (module.func_list)[0],
            program_error!(ProgramError::ModuleError(module_id))
        )
        .clone();
        let callee = self.new_closure_value(Closure::new(module_id, func, None));
        self.call(callee, Vec::new())
    }

    #[inline]
    pub fn call(&mut self, mut callee: Value, mut args: Vec<Value>) -> Result<Value> {
        macro_rules! return_error {
            ($value:expr) => {
                return Ok($value.into_table_value(self))
            };
        }

        if let Some(t) = callee.metatable() {
            args.insert(0, callee);
            callee = match t.get(&self.get_builtin_str("__call__")) {
                Some(v) => v,
                None => return_error!(not_callable_error!(callee)),
            };
        }

        if let Some(f) = callee.as_ext_function() {
            f(args, self)
        } else if let Some(c) = callee.as_ext_closure_mut() {
            (c.func)(args, &mut c.upvalues, self)
        } else if let Some(v) = callee.clone().as_closure_mut() {
            if let Err(e) = set_closure_args(self, v, args) {
                return_error!(e);
            }

            let current_frame = self.current_frame;

            let mut frame = Frame::new(
                match callee {
                    Value::GCObject(gc_obj) => NonNull::new(gc_obj).unwrap(),
                    _ => panic!("unexpect error"),
                },
                self.current_frame,
                v.function.stack_size,
            );
            self.current_frame = Some(NonNull::new(&mut frame).unwrap());
            let return_value = frame.run(self);

            self.current_frame = current_frame;
            return_value
        } else {
            return_error!(not_callable_error!(callee));
        }
    }

    pub fn set_global_variable(&mut self, key: String, value: Value) {
        self.global_variables.insert(key, value);
    }

    pub fn get_global_variable(&self, key: &str) -> Option<Value> {
        self.global_variables.get(key).copied()
    }

    pub fn get_builtin_variable(&self, key: &str) -> Option<Value> {
        self.builtin_variables.get(key).copied()
    }

    pub fn get_builtin_str(&mut self, key: &str) -> Value {
        Value::GCObject(match self.builtin_str_value.get(key) {
            Some(v) => *v,
            None => {
                let t = self.new_gc_object(GCObjectKind::Str(key.to_string()));
                self.builtin_str_value.insert(key.to_string(), t);
                t
            }
        })
    }

    pub fn new_gc_object(&mut self, value: GCObjectKind) -> *mut GCObject {
        if self.heap.len() > self.last_heap_len * 2 {
            self.last_heap_len = self.heap.len();
            self.gc();
        }
        let value = GCObject::new(value);
        unsafe {
            let ptr = alloc(self.mem_layout) as *mut GCObject;
            ptr.write(value);
            self.heap.push(ptr);
            ptr
        }
    }

    #[inline]
    pub fn new_gc_value(&mut self, value: GCObjectKind) -> Value {
        Value::GCObject(self.new_gc_object(value))
    }

    #[inline]
    pub fn new_str_value(&mut self, value: String) -> Value {
        self.new_gc_value(GCObjectKind::Str(value))
    }

    #[inline]
    pub fn new_table_value(&mut self, value: Table) -> Value {
        self.new_gc_value(GCObjectKind::Table(value))
    }

    #[inline]
    pub fn new_userdata_value(&mut self, value: UserData) -> Value {
        self.new_gc_value(GCObjectKind::UserData(value))
    }

    #[inline]
    pub fn new_closure_value(&mut self, value: Closure) -> Value {
        self.new_gc_value(GCObjectKind::Closure(value))
    }

    #[inline]
    pub fn new_ext_closure_value(&mut self, value: ExtClosure) -> Value {
        self.new_gc_value(GCObjectKind::ExtClosure(value))
    }

    pub fn traceback(&self) -> Vec<TracebackFrame> {
        let mut traceback_frames = Vec::new();
        let mut frame = self.current_frame;
        while let Some(f) = frame {
            let f = unsafe { f.as_ref() };
            traceback_frames.push(TracebackFrame {
                pc: f.pc,
                operate_stack: f.operate_stack.clone(),
                closure: unsafe {
                    match &f.closure.as_ref().kind {
                        GCObjectKind::Closure(v) => v.clone(),
                        _ => panic!("unexpect error"),
                    }
                },
            });
            frame = f.prev_frame;
        }
        traceback_frames
    }

    pub fn gc(&mut self) {
        unsafe {
            // mark
            for ptr in &self.heap {
                (**ptr).gc_state = true;
            }
            if let Some(frame) = self.current_frame {
                let mut frame = frame.as_ref();
                loop {
                    Self::gc_mark_object(frame.closure.as_ptr());
                    for value in &frame.operate_stack {
                        if let Value::GCObject(ptr) = value {
                            Self::gc_mark_object(*ptr);
                        }
                    }
                    match frame.prev_frame {
                        Some(t) => frame = t.as_ref(),
                        None => break,
                    }
                }
            }
            for v in self.global_variables.values() {
                if let Value::GCObject(ptr) = v {
                    Self::gc_mark_object(*ptr);
                }
            }
            for v in self.builtin_variables.values() {
                if let Value::GCObject(ptr) = v {
                    Self::gc_mark_object(*ptr);
                }
            }
            for v in self.builtin_str_value.values() {
                Self::gc_mark_object(*v);
            }
            // sweep
            let mut new_heap = Vec::new();
            for ptr in self.heap.clone() {
                if (*ptr).gc_state {
                    if let GCObjectKind::UserData(t) = &mut (*ptr).kind {
                        (t.drop_func)(t);
                    }
                    ptr.drop_in_place();
                    dealloc(ptr as *mut u8, self.mem_layout);
                } else {
                    new_heap.push(ptr);
                }
            }
            self.heap = new_heap;
        }
    }

    fn gc_mark_object(ptr: *mut GCObject) {
        macro_rules! mark_table {
            ($table:expr) => {
                for (k, v) in $table.iter() {
                    if let Value::GCObject(ptr) = k {
                        Self::gc_mark_object(*ptr);
                    }
                    if let Value::GCObject(ptr) = v {
                        Self::gc_mark_object(*ptr);
                    }
                }
            };
        }
        unsafe {
            (*ptr).gc_state = false;
            match &(*ptr).kind {
                GCObjectKind::Str(_) => (),
                GCObjectKind::Table(table) => {
                    mark_table!(table);
                    if let Value::GCObject(ptr) = table.metatable {
                        Self::gc_mark_object(ptr);
                    }
                }
                GCObjectKind::UserData(userdata) => mark_table!(userdata.metatable),
                GCObjectKind::Closure(closure) => {
                    if let Some(ptr) = closure.base_closure {
                        Self::gc_mark_object(ptr.as_ptr());
                    }
                    for v in &closure.variables {
                        if let Value::GCObject(ptr) = v {
                            Self::gc_mark_object(*ptr);
                        }
                    }
                }
                GCObjectKind::ExtClosure(ext_closure) => {
                    for v in &ext_closure.upvalues {
                        if let Value::GCObject(ptr) = v {
                            Self::gc_mark_object(*ptr);
                        }
                    }
                }
            }
        }
    }
}

impl Default for Lvm {
    fn default() -> Self {
        Self::new()
    }
}

impl From<Program> for Lvm {
    fn from(program: Program) -> Self {
        let mut lvm = Lvm::new();
        lvm.modules.push(program);
        lvm
    }
}

fn set_closure_args(
    lvm: &mut Lvm,
    v: &mut Closure,
    mut args: Vec<Value>,
) -> std::result::Result<(), BuiltinError> {
    let params_num = v.function.params.len();
    match args.len().cmp(&params_num) {
        Ordering::Less => {
            if v.function.variadic.is_none() {
                return Err(call_arguments_error!(
                    Some(Box::new(v.clone())),
                    Eq(params_num),
                    args.len()
                ));
            } else {
                return Err(call_arguments_error!(
                    Some(Box::new(v.clone())),
                    RangeFrom(params_num..),
                    args.len()
                ));
            }
        }
        Ordering::Equal => {
            v.variables[..params_num].copy_from_slice(&args[..]);
            if v.function.variadic.is_some() {
                v.variables[params_num] = lvm.new_table_value(Table::new());
            }
        }
        Ordering::Greater => {
            if v.function.variadic.is_none() {
                return Err(call_arguments_error!(
                    Some(Box::new(v.clone())),
                    Eq(params_num),
                    args.len()
                ));
            } else {
                let t = args.split_off(params_num);
                v.variables[..params_num].copy_from_slice(&args[..]);
                v.variables[params_num] = lvm.new_table_value(t.into());
            }
        }
    }
    Ok(())
}
