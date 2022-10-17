use core::ptr::NonNull;
use std::alloc::{alloc, dealloc, Layout};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fs;
use std::path::PathBuf;

use crate::codegen::{ConstlValue, FunctionKind, JumpTarget, OPCode, Program};
use crate::errors::{LResult, LuciaError, RuntimeErrorKind, TypeErrorKind};
use crate::libs;
use crate::object::*;

#[macro_export]
macro_rules! str_to_value {
    ($lvm:expr ,$name:expr) => {
        $lvm.new_gc_value($crate::object::GCObjectKind::Str(String::from($name)))
    };
}

#[macro_export]
macro_rules! unsupported_operand_type {
    ($operator:expr, $arg1:expr) => {
        $crate::errors::LuciaError::TypeError($crate::errors::TypeErrorKind::UnOperatorError {
            operator: $operator,
            operand: $arg1.value_type(),
        })
    };
    ($operator:expr, $arg1:expr, $arg2:expr) => {
        $crate::errors::LuciaError::TypeError($crate::errors::TypeErrorKind::BinOperatorError {
            operator: $operator,
            operand: ($arg1.value_type(), $arg2.value_type()),
        })
    };
}

#[macro_export]
macro_rules! type_convert_error {
    ($from:expr, $to:expr) => {
        $crate::errors::LuciaError::TypeError($crate::errors::TypeErrorKind::ConvertError {
            from: $from,
            to: $to,
        })
    };
}

#[macro_export]
macro_rules! not_callable_error {
    ($value:expr) => {
        $crate::errors::LuciaError::TypeError($crate::errors::TypeErrorKind::NotCallableError(
            $value.value_type(),
        ))
    };
}

#[macro_export]
macro_rules! call_arguments_error {
    ($value:expr, $require:expr, $give:expr) => {
        $crate::errors::LuciaError::TypeError($crate::errors::TypeErrorKind::CallArgumentsError {
            value: $value,
            required: $require,
            given: $give,
        })
    };
}

const STACK_ERROR: LuciaError = LuciaError::RuntimeError(RuntimeErrorKind::StackError);
const IMPORT_ERROR: LuciaError = LuciaError::RuntimeError(RuntimeErrorKind::ImportError);
const UPVALUE_ERROR: LuciaError = LuciaError::RuntimeError(RuntimeErrorKind::UpvalueError);
const PROGRAM_ERROR: LuciaError = LuciaError::RuntimeError(RuntimeErrorKind::ProgramError);

#[derive(Debug, Clone)]
pub struct Frame {
    pc: usize,
    closure: *mut GCObject,
    operate_stack: Vec<LuciaValue>,
    prev_frame: Option<NonNull<Frame>>,
    lvm: *mut Lvm,
}

impl Frame {
    pub fn new(
        closure: *mut GCObject,
        lvm: *mut Lvm,
        prev_frame: Option<NonNull<Frame>>,
        stack_size: usize,
    ) -> Self {
        Frame {
            pc: 0,
            closure,
            operate_stack: Vec::with_capacity(stack_size),
            prev_frame,
            lvm,
        }
    }

    pub fn run(&mut self) -> LResult<LuciaValue> {
        macro_rules! run_default {
            ($block:expr, $arg1:ident, $arg2:ident, $special_name:expr, $operator:expr) => {
                if $arg1.value_type() == LuciaValueType::Table {
                    match <&LuciaTable>::try_from($arg1)
                        .expect("unexpect error")
                        .get_by_str($special_name)
                    {
                        Some(v) => {
                            self.operate_stack.push(v);
                            self.operate_stack.push($arg1);
                            self.operate_stack.push($arg2);
                            self.call(2, true)?;
                        }
                        None => return Err(unsupported_operand_type!($operator, $arg1, $arg2)),
                    }
                } else {
                    $block
                }
            };
        }

        macro_rules! run_bin_op {
            ($op: tt, $special_name:expr, $operator:expr) => {{
                let arg2 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                run_default!({
                    self.operate_stack.push(match (arg1, arg2) {
                        (LuciaValue::Int(v1), LuciaValue::Int(v2)) => LuciaValue::Int(v1 $op v2),
                        (LuciaValue::Float(v1), LuciaValue::Float(v2)) => LuciaValue::Float(v1 $op v2),
                        _ => return Err(unsupported_operand_type!($operator, arg1, arg2)),
                    });
                }, arg1, arg2, $special_name, $operator)
            }};
        }

        macro_rules! run_eq_ne {
            ($op: tt, $special_name:expr, $operator:expr) => {{
                let arg2 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                run_default!({
                    self.operate_stack.push(LuciaValue::Bool(arg1 $op arg2));
                }, arg1, arg2, $special_name, $operator)
            }};
        }

        macro_rules! run_compare {
            ($op: tt, $special_name:expr, $operator:expr) => {{
                let arg2 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                run_default!({
                    self.operate_stack.push(LuciaValue::Bool(match (arg1, arg2) {
                        (LuciaValue::Int(v1), LuciaValue::Int(v2)) => v1 $op v2,
                        (LuciaValue::Float(v1), LuciaValue::Float(v2)) => v1 $op v2,
                        _ => return Err(unsupported_operand_type!($operator, arg1, arg2)),
                    }));
                }, arg1, arg2, $special_name, $operator)
            }};
        }

        macro_rules! get_table {
            ($special_name:expr) => {{
                let arg2 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                let t = <&mut LuciaTable>::try_from(arg1)?;
                match t.get_by_str($special_name) {
                    Some(v) => {
                        self.operate_stack.push(v);
                        self.operate_stack.push(arg1);
                        self.operate_stack.push(arg2);
                        self.call(2, true)?;
                    }
                    None => self
                        .operate_stack
                        .push(t.get(&arg2).unwrap_or(LuciaValue::Null)),
                }
            }};
        }

        macro_rules! set_table {
            ($special_name:expr) => {{
                let arg3 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                let arg2 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                let t = <&mut LuciaTable>::try_from(arg1)?;
                match t.get_by_str($special_name) {
                    Some(v) => {
                        self.operate_stack.push(v);
                        self.operate_stack.push(arg1);
                        self.operate_stack.push(arg2);
                        self.operate_stack.push(arg3);
                        self.call(3, true)?;
                    }
                    None => t.set(&arg2, arg3),
                }
            }};
        }

        let lvm = unsafe { self.lvm.as_mut().expect("unexpect error") };
        let closure = unsafe {
            match &mut self.closure.as_mut().unwrap().kind {
                GCObjectKind::Closure(v) => v,
                _ => panic!("unexpect error"),
            }
        };
        loop {
            let code = closure
                .function
                .code_list
                .get(self.pc)
                .ok_or_else(|| PROGRAM_ERROR)?;
            match code {
                OPCode::Pop => {
                    self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                }
                OPCode::Dup => {
                    self.operate_stack
                        .push(*self.operate_stack.last().ok_or_else(|| STACK_ERROR)?);
                }
                OPCode::DupTwo => {
                    let a = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                    let b = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                    self.operate_stack.push(b);
                    self.operate_stack.push(a);
                }
                OPCode::Rot => {
                    let a = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                    let b = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                    self.operate_stack.push(a);
                    self.operate_stack.push(b);
                }
                OPCode::LoadLocal(i) => {
                    self.operate_stack.push(closure.variables[*i]);
                }
                OPCode::LoadGlobal(i) => {
                    let t = &closure.function.global_names[*i];
                    self.operate_stack.push(
                        lvm.get_global_variable(t)
                            .unwrap_or(lvm.get_builtin_variable(t).unwrap_or(LuciaValue::Null)),
                    );
                }
                OPCode::LoadUpvalue(i) => {
                    let (_, func_count, upvalue_id) = closure.function.upvalue_names[*i];
                    let mut base_closure = closure.base_closure;
                    for _ in 0..func_count {
                        base_closure = match unsafe {
                            &base_closure.ok_or_else(|| UPVALUE_ERROR)?.as_ref().kind
                        } {
                            GCObjectKind::Closure(v) => v.base_closure,
                            _ => return Err(UPVALUE_ERROR),
                        };
                    }
                    match unsafe { &base_closure.ok_or_else(|| UPVALUE_ERROR)?.as_ref().kind } {
                        GCObjectKind::Closure(v) => {
                            self.operate_stack.push(v.variables[upvalue_id])
                        }
                        _ => return Err(UPVALUE_ERROR),
                    };
                }
                OPCode::LoadConst(i) => {
                    let t = lvm.module_list[closure.module_id].const_list[*i].clone();
                    let v = match t {
                        ConstlValue::Null => LuciaValue::Null,
                        ConstlValue::Bool(v) => LuciaValue::Bool(v),
                        ConstlValue::Int(v) => LuciaValue::Int(v),
                        ConstlValue::Float(v) => LuciaValue::Float(v),
                        ConstlValue::Str(v) => lvm.new_gc_value(GCObjectKind::Str(v)),
                        ConstlValue::Func(func_id) => {
                            let f = lvm.module_list[closure.module_id].func_list[func_id].clone();
                            lvm.new_gc_value(GCObjectKind::Closure(Closure {
                                module_id: closure.module_id,
                                base_closure: if f.kind == FunctionKind::Closure {
                                    NonNull::new(self.closure)
                                } else {
                                    None
                                },
                                variables: {
                                    let mut temp: Vec<LuciaValue> =
                                        Vec::with_capacity(f.local_names.len());
                                    for _ in 0..f.local_names.len() {
                                        temp.push(LuciaValue::Null);
                                    }
                                    temp
                                },
                                function: f,
                            }))
                        }
                    };
                    self.operate_stack.push(v);
                }
                OPCode::StoreLocal(i) => {
                    closure.variables[*i] = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                }
                OPCode::StoreGlobal(i) => {
                    lvm.set_global_variable(
                        closure.function.global_names[*i].clone(),
                        self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?,
                    );
                }
                OPCode::StoreUpvalue(i) => {
                    let (_, func_count, upvalue_id) = closure.function.upvalue_names[*i];
                    let mut base_closure = closure.base_closure;
                    for _ in 0..func_count {
                        base_closure = match unsafe {
                            &base_closure.ok_or_else(|| UPVALUE_ERROR)?.as_ref().kind
                        } {
                            GCObjectKind::Closure(v) => v.base_closure,
                            _ => return Err(UPVALUE_ERROR),
                        };
                    }
                    match unsafe { &mut base_closure.ok_or_else(|| UPVALUE_ERROR)?.as_mut().kind } {
                        GCObjectKind::Closure(v) => {
                            v.variables[upvalue_id] =
                                self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?
                        }
                        _ => return Err(UPVALUE_ERROR),
                    };
                }
                OPCode::Import(i) => {
                    if let ConstlValue::Str(v) =
                        lvm.module_list[closure.module_id].const_list[*i].clone()
                    {
                        if let Some(v) = v.strip_prefix("std/") {
                            match lvm.std_libs.get(&String::from(v)) {
                                Some(module) => match <&LuciaTable>::try_from(*module) {
                                    Ok(_) => self.operate_stack.push(*module),
                                    Err(_) => return Err(IMPORT_ERROR),
                                },
                                None => return Err(IMPORT_ERROR),
                            }
                        } else {
                            let mut path = PathBuf::new();
                            if let Some(v) = lvm.get_global_variable("__module_path__") {
                                path.push(String::try_from(v)?);
                            }
                            path.push(v);
                            path.set_extension("lucia");
                            let input_file = fs::read_to_string(path).expect("Read file error!");
                            lvm.module_list.push(Program::try_from(&input_file)?);
                            let module = lvm.run_module(lvm.module_list.len() - 1)?;
                            match <&LuciaTable>::try_from(module) {
                                Ok(_) => self.operate_stack.push(module),
                                Err(_) => return Err(IMPORT_ERROR),
                            }
                        }
                    } else {
                        return Err(IMPORT_ERROR);
                    }
                }
                OPCode::ImportFrom(i) => {
                    let module = <&LuciaTable>::try_from(
                        *self.operate_stack.last().ok_or_else(|| STACK_ERROR)?,
                    )?;
                    if let ConstlValue::Str(t) = &lvm.module_list[closure.module_id].const_list[*i]
                    {
                        self.operate_stack
                            .push(module.raw_get_by_str(t).ok_or_else(|| IMPORT_ERROR)?);
                    } else {
                        return Err(IMPORT_ERROR);
                    }
                }
                OPCode::ImportGlob => {
                    let module = <&LuciaTable>::try_from(
                        self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?,
                    )?;
                    for (k, v) in module.clone() {
                        lvm.set_global_variable(String::try_from(k)?, v);
                    }
                }
                OPCode::BuildTable(i) => {
                    let mut temp: Vec<LuciaValue> = Vec::new();
                    for _ in 0..(*i * 2) {
                        temp.push(self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?);
                    }
                    let mut table: Vec<(LuciaValue, LuciaValue)> = Vec::new();
                    for _ in 0..*i {
                        let arg1 = temp.pop().expect("unexpect error");
                        let arg2 = temp.pop().expect("unexpect error");
                        if let LuciaValue::GCObject(_) = arg1 {
                            if String::try_from(arg1).is_err() {
                                return Err(LuciaError::TypeError(TypeErrorKind::BuildTableError(
                                    arg1.value_type(),
                                )));
                            }
                        }
                        table.push((arg1, arg2));
                    }
                    self.operate_stack
                        .push(lvm.new_gc_value(GCObjectKind::Table(LuciaTable(table))));
                }
                OPCode::GetAttr => get_table!("__getattr__"),
                OPCode::GetItem => get_table!("__getitem__"),
                OPCode::SetAttr => set_table!("__setattr__"),
                OPCode::SetItem => set_table!("__setitem__"),
                OPCode::Neg => {
                    let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                    match <&LuciaTable>::try_from(arg1) {
                        Ok(v) => match v.get_by_str("__neg__") {
                            Some(v) => {
                                self.operate_stack.push(v);
                                self.operate_stack.push(arg1);
                                self.call(1, true)?;
                            }
                            None => return Err(unsupported_operand_type!(code.clone(), arg1)),
                        },
                        Err(_) => self.operate_stack.push(match arg1 {
                            LuciaValue::Int(v) => LuciaValue::Int(-v),
                            LuciaValue::Float(v) => LuciaValue::Float(-v),
                            _ => return Err(unsupported_operand_type!(code.clone(), arg1)),
                        }),
                    }
                }
                OPCode::Not => {
                    let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                    self.operate_stack
                        .push(LuciaValue::Bool(!bool::try_from(arg1)?));
                }
                OPCode::Add => {
                    let arg2 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                    let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                    match arg1.value_type() {
                        LuciaValueType::Str => {
                            match (String::try_from(arg1), String::try_from(arg2)) {
                                (Ok(v1), Ok(v2)) => self
                                    .operate_stack
                                    .push(lvm.new_gc_value(GCObjectKind::Str(v1 + &v2))),
                                _ => {
                                    return Err(unsupported_operand_type!(code.clone(), arg1, arg2))
                                }
                            };
                        }
                        LuciaValueType::Table => {
                            match <&LuciaTable>::try_from(arg1)
                                .expect("unexpect error")
                                .get_by_str("__add__")
                            {
                                Some(v) => {
                                    self.operate_stack.push(v);
                                    self.operate_stack.push(arg1);
                                    self.operate_stack.push(arg2);
                                    self.call(2, true)?;
                                }
                                None => {
                                    return Err(unsupported_operand_type!(code.clone(), arg1, arg2))
                                }
                            }
                        }
                        _ => {
                            self.operate_stack.push(match (arg1, arg2) {
                                (LuciaValue::Int(v1), LuciaValue::Int(v2)) => {
                                    LuciaValue::Int(v1 + v2)
                                }
                                (LuciaValue::Float(v1), LuciaValue::Float(v2)) => {
                                    LuciaValue::Float(v1 + v2)
                                }
                                _ => {
                                    return Err(unsupported_operand_type!(code.clone(), arg1, arg2))
                                }
                            });
                        }
                    }
                }
                OPCode::Sub => run_bin_op!(-, "__sub__", code.clone()),
                OPCode::Mul => run_bin_op!(*, "__mul__", code.clone()),
                OPCode::Div => run_bin_op!(/, "__div__", code.clone()),
                OPCode::Mod => run_bin_op!(%, "__mod__", code.clone()),
                OPCode::Eq => run_eq_ne!(==, "__eq__", code.clone()),
                OPCode::Ne => run_eq_ne!(!=, "__ne__", code.clone()),
                OPCode::Gt => run_compare!(>, "__gt__", code.clone()),
                OPCode::Ge => run_compare!(>=, "__ge__", code.clone()),
                OPCode::Lt => run_compare!(<, "__lt__", code.clone()),
                OPCode::Le => run_compare!(<=, "__le__", code.clone()),
                OPCode::Is => {
                    let arg2 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                    let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                    self.operate_stack
                        .push(LuciaValue::Bool(match (arg1, arg2) {
                            (LuciaValue::Null, LuciaValue::Null) => true,
                            (LuciaValue::Bool(v1), LuciaValue::Bool(v2)) => v1 == v2,
                            (LuciaValue::Int(v1), LuciaValue::Int(v2)) => v1 == v2,
                            (LuciaValue::Float(v1), LuciaValue::Float(v2)) => v1 == v2,
                            (LuciaValue::GCObject(v1), LuciaValue::GCObject(v2)) => v1 == v2,
                            _ => false,
                        }));
                }
                OPCode::For(JumpTarget(i)) => {
                    self.call(0, false)?;
                    if self.operate_stack.last().ok_or_else(|| STACK_ERROR)? == &LuciaValue::Null {
                        self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                        self.pc = *i;
                        continue;
                    }
                }
                OPCode::Jump(JumpTarget(i)) => {
                    self.pc = *i;
                    continue;
                }
                OPCode::JumpIfFalse(JumpTarget(i)) => {
                    let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                    if let Ok(v) = bool::try_from(arg1) {
                        if !v {
                            self.pc = *i;
                            continue;
                        }
                    }
                }
                OPCode::JumpIfTureOrPop(JumpTarget(i)) => {
                    let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                    if let Ok(v) = bool::try_from(arg1) {
                        if v {
                            self.pc = *i;
                            continue;
                        } else {
                            self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                        }
                    }
                }
                OPCode::JumpIfFalseOrPop(JumpTarget(i)) => {
                    let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                    if let Ok(v) = bool::try_from(arg1) {
                        if !v {
                            self.pc = *i;
                            continue;
                        } else {
                            self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                        }
                    }
                }
                OPCode::Call(i) => {
                    self.call(*i, true)?;
                }
                OPCode::Return => {
                    if closure.function.kind == FunctionKind::Do {
                        let mut temp = LuciaTable::new();
                        for i in 0..closure.function.local_names.len() {
                            temp.set(
                                &str_to_value!(lvm, closure.function.local_names[i].clone()),
                                closure.variables[i],
                            )
                        }
                        return Ok(lvm.new_gc_value(GCObjectKind::Table(temp)));
                    } else {
                        return Ok(self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?);
                    }
                }
                OPCode::JumpTarget(_) => return Err(PROGRAM_ERROR),
            }
            self.pc += 1;
        }
    }

    fn call(&mut self, arg_num: usize, pop: bool) -> LResult<()> {
        let lvm = unsafe { self.lvm.as_mut().expect("unexpect error") };

        let mut arguments = Vec::with_capacity(arg_num);
        for _ in 0..arg_num {
            arguments.push(self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?);
        }

        let callee = if pop {
            self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?
        } else {
            *self.operate_stack.last().ok_or_else(|| STACK_ERROR)?
        };
        if let LuciaValue::GCObject(gc_obj) = callee {
            let v = match unsafe { &mut gc_obj.as_mut().expect("unexpect error").kind } {
                GCObjectKind::Closure(v) => v,
                GCObjectKind::Table(v) => <&mut Closure>::try_from(
                    v.get_by_str("__call__")
                        .ok_or_else(|| not_callable_error!(callee))?,
                )
                .or_else(|_| Err(not_callable_error!(callee)))?,
                GCObjectKind::ExtClosure(v) => {
                    arguments.reverse();
                    self.operate_stack.push(v(arguments, lvm)?);
                    return Ok(());
                }
                _ => return Err(not_callable_error!(callee)),
            };
            let params_num = v.function.params.len();
            if arg_num < params_num || (v.function.variadic == None && arg_num != params_num) {
                return Err(call_arguments_error!(
                    Some(Box::new(v.clone())),
                    params_num,
                    arg_num
                ));
            }
            for i in 0..params_num {
                v.variables[i] = arguments.pop().expect("unexpect error");
            }
            if v.function.variadic != None {
                v.variables[params_num] =
                    lvm.new_gc_value(GCObjectKind::Table(LuciaTable::from(arguments)));
            }
            let mut frame = Frame::new(gc_obj, self.lvm, NonNull::new(self), v.function.stack_size);
            self.operate_stack.push(frame.run()?);
        } else if let LuciaValue::ExtFunction(f) = callee {
            arguments.reverse();
            self.operate_stack.push(f(arguments, lvm)?);
        } else {
            return Err(not_callable_error!(callee));
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Lvm {
    pub module_list: Vec<Program>,
    pub global_variables: HashMap<String, LuciaValue>,
    pub builtin_variables: HashMap<String, LuciaValue>,
    pub std_libs: HashMap<String, LuciaValue>,
    pub current_frame: NonNull<Frame>,
    mem_layout: Layout,
    heap: Vec<*mut GCObject>,
    last_heap_len: usize,
}

impl Lvm {
    pub fn new(program: Program) -> Self {
        let mut t = Lvm {
            module_list: vec![program],
            global_variables: HashMap::new(),
            builtin_variables: libs::builtin::builtin_variables(),
            std_libs: HashMap::new(),
            current_frame: NonNull::dangling(),
            mem_layout: Layout::new::<GCObject>(),
            heap: Vec::with_capacity(0),
            last_heap_len: 64,
        };
        t.std_libs = libs::std_libs(&mut t);
        t
    }

    pub fn run(&mut self) -> LResult<LuciaValue> {
        self.run_module(0)
    }

    pub fn run_module(&mut self, module_id: usize) -> LResult<LuciaValue> {
        let func = self.module_list[module_id]
            .func_list
            .first()
            .ok_or_else(|| PROGRAM_ERROR)?
            .clone();
        let stack_size = func.stack_size;
        let mut frame = Frame::new(
            self.new_gc_object(GCObjectKind::Closure(Closure {
                module_id,
                base_closure: None,
                variables: {
                    let mut temp: Vec<LuciaValue> = Vec::with_capacity(func.local_names.len());
                    for _ in 0..func.local_names.len() {
                        temp.push(LuciaValue::Null);
                    }
                    temp
                },
                function: func,
            })),
            self,
            None,
            stack_size,
        );
        self.current_frame = NonNull::new(&mut frame).expect("unexpect error");
        frame.run()
    }

    pub fn set_global_variable(&mut self, key: String, value: LuciaValue) {
        self.global_variables.insert(key, value);
    }

    pub fn get_global_variable(&self, key: &str) -> Option<LuciaValue> {
        match self.global_variables.get(key) {
            Some(v) => Some(*v),
            None => None,
        }
    }

    pub fn get_builtin_variable(&self, key: &str) -> Option<LuciaValue> {
        match self.builtin_variables.get(key) {
            Some(v) => Some(*v),
            None => None,
        }
    }

    pub fn new_gc_value(&mut self, value: GCObjectKind) -> LuciaValue {
        LuciaValue::GCObject(self.new_gc_object(value))
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

    fn gc_mark_object(&self, ptr: *mut GCObject) {
        unsafe {
            (*ptr).gc_state = false;
            match &(*ptr).kind {
                GCObjectKind::Table(table) => {
                    for (_, v) in table.clone() {
                        if let LuciaValue::GCObject(ptr) = v {
                            self.gc_mark_object(ptr);
                        }
                    }
                }
                GCObjectKind::Closure(closure) => {
                    if let Some(ptr) = closure.base_closure {
                        self.gc_mark_object(ptr.as_ptr());
                    }
                    for v in &closure.variables {
                        if let LuciaValue::GCObject(ptr) = v {
                            self.gc_mark_object(*ptr);
                        }
                    }
                }
                GCObjectKind::Str(_) => (),
                GCObjectKind::ExtClosure(_) => (),
            }
        }
    }

    pub fn gc(&mut self) {
        unsafe {
            // mark
            for ptr in &self.heap {
                (**ptr).gc_state = true;
            }
            let mut frame = self.current_frame.as_ref();
            loop {
                self.gc_mark_object(frame.closure);
                for value in &frame.operate_stack {
                    if let LuciaValue::GCObject(ptr) = value {
                        self.gc_mark_object(*ptr);
                    }
                }
                match frame.prev_frame {
                    Some(t) => frame = t.as_ref(),
                    None => break,
                }
            }
            // sweep
            let mut t = Vec::new();
            for ptr in &self.heap {
                if (**ptr).gc_state {
                    ptr.drop_in_place();
                    dealloc(*ptr as *mut u8, self.mem_layout);
                } else {
                    t.push(*ptr);
                }
            }
            self.heap = t;
        }
    }
}
