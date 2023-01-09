#![allow(clippy::unnecessary_lazy_evaluations)]

use core::ptr::NonNull;
use std::alloc::{alloc, dealloc, Layout};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fs;
use std::path::PathBuf;

use crate::codegen::{ConstlValue, FunctionKind, JumpTarget, OPCode, Program};
use crate::errors::{LResult, LuciaError, RuntimeErrorKind};
use crate::{build_table_error, unsupported_operand_type};
use crate::{call_arguments_error, objects::*};
use crate::{libs, not_callable_error};

#[macro_export]
macro_rules! error {
    ($value:expr) => {{
        if let $crate::objects::LuciaValue::GCObject(v) = $value {
            unsafe { (*v).is_error = true }
        }
        $value
    }};
}

#[macro_export]
macro_rules! return_type_error {
    ($lvm:expr, $value:expr) => {{
        let mut error_table = $crate::objects::LuciaTable::new();
        error_table.set(
            &$lvm.get_builtin_str("type"),
            $lvm.new_str_value($value.error_type().to_string()),
        );
        error_table.set(
            &$lvm.get_builtin_str("msg"),
            $lvm.new_str_value($value.msg()),
        );
        let error_table_value = $lvm.new_table_value(error_table);
        return Ok($crate::error!(error_table_value));
    }};
}

#[macro_export]
macro_rules! try_error {
    ($lvm:expr, $expr:expr) => {
        match $expr {
            Ok(val) => val,
            Err(err) => return_type_error!($lvm, err.clone()),
        }
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
}

impl Frame {
    pub fn new(
        closure: *mut GCObject,
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

    pub fn run(&mut self, lvm: &mut Lvm) -> LResult<LuciaValue> {
        macro_rules! call {
            ($lvm:expr, $arg_num:expr) => {{
                let args = self
                    .operate_stack
                    .split_off(self.operate_stack.len() - $arg_num);
                let callee = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                lvm.call(callee, args)
            }};
        }

        macro_rules! run_default {
            ($lvm:expr, $block:expr, $arg1:ident, $arg2:ident, $special_name:expr, $operator:expr) => {
                if $arg1.value_type() == LuciaValueType::Table {
                    match <&LuciaTable>::try_from($arg1)
                        .expect("unexpect error")
                        .get(&$lvm.get_builtin_str($special_name))
                    {
                        Some(v) => {
                            self.operate_stack.push(v);
                            self.operate_stack.push($arg1);
                            self.operate_stack.push($arg2);
                            let return_value = call!(lvm, 2)?;
                            self.operate_stack.push(return_value);
                        }
                        None => return_type_error!(
                            lvm,
                            unsupported_operand_type!($operator, $arg1, $arg2)
                        ),
                    }
                } else {
                    $block
                }
            };
        }

        macro_rules! run_bin_op {
            ($lvm:expr, $op: tt, $special_name:expr, $operator:expr) => {{
                let arg2 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                run_default!($lvm, {
                    self.operate_stack.push(match (arg1, arg2) {
                        (LuciaValue::Int(v1), LuciaValue::Int(v2)) => LuciaValue::Int(v1 $op v2),
                        (LuciaValue::Float(v1), LuciaValue::Float(v2)) => LuciaValue::Float(v1 $op v2),
                        _ => return_type_error!(
                            lvm,
                            unsupported_operand_type!($operator, arg1, arg2)
                        ),
                    });
                }, arg1, arg2, $special_name, $operator)
            }};
        }

        macro_rules! run_eq_ne {
            ($lvm:expr, $op: tt, $special_name:expr, $operator:expr) => {{
                let arg2 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                run_default!($lvm, {
                    self.operate_stack.push(LuciaValue::Bool(arg1 $op arg2));
                }, arg1, arg2, $special_name, $operator)
            }};
        }

        macro_rules! run_compare {
            ($lvm:expr, $op: tt, $special_name:expr, $operator:expr) => {{
                let arg2 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                run_default!($lvm, {
                    self.operate_stack.push(LuciaValue::Bool(match (arg1, arg2) {
                        (LuciaValue::Int(v1), LuciaValue::Int(v2)) => v1 $op v2,
                        (LuciaValue::Float(v1), LuciaValue::Float(v2)) => v1 $op v2,
                        _ => return_type_error!(
                            lvm,
                            unsupported_operand_type!($operator, arg1, arg2)
                        ),
                    }));
                }, arg1, arg2, $special_name, $operator)
            }};
        }

        macro_rules! get_table {
            ($lvm:expr, $special_name:expr) => {{
                let arg2 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                let t = try_error!($lvm, <&mut LuciaTable>::try_from(arg1));
                match t.get(&$lvm.get_builtin_str($special_name)) {
                    Some(v) => {
                        self.operate_stack.push(v);
                        self.operate_stack.push(arg1);
                        self.operate_stack.push(arg2);
                        let return_value = call!(lvm, 2)?;
                        self.operate_stack.push(return_value);
                    }
                    None => self
                        .operate_stack
                        .push(t.get(&arg2).unwrap_or(LuciaValue::Null)),
                }
            }};
        }

        macro_rules! set_table {
            ($lvm:expr, $special_name:expr) => {{
                let arg3 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                let arg2 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                let t = try_error!($lvm, <&mut LuciaTable>::try_from(arg1));
                match t.get(&$lvm.get_builtin_str($special_name)) {
                    Some(v) => {
                        self.operate_stack.push(v);
                        self.operate_stack.push(arg1);
                        self.operate_stack.push(arg2);
                        self.operate_stack.push(arg3);
                        let return_value = call!(lvm, 3)?;
                        self.operate_stack.push(return_value);
                    }
                    None => t.set(&arg2, arg3),
                }
            }};
        }

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
                    self.operate_stack
                        .push(lvm.get_global_variable(t).unwrap_or_else(|| {
                            lvm.get_builtin_variable(t).unwrap_or(LuciaValue::Null)
                        }));
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
                    let t = lvm.modules[closure.module_id].const_list[*i].clone();
                    let v = match t {
                        ConstlValue::Null => LuciaValue::Null,
                        ConstlValue::Bool(v) => LuciaValue::Bool(v),
                        ConstlValue::Int(v) => LuciaValue::Int(v),
                        ConstlValue::Float(v) => LuciaValue::Float(v),
                        ConstlValue::Str(v) => lvm.new_str_value(v),
                        ConstlValue::Func(func_id) => {
                            let f = lvm.modules[closure.module_id].func_list[func_id].clone();
                            let base_closure = if f.kind == FunctionKind::Closure {
                                NonNull::new(self.closure)
                            } else {
                                None
                            };
                            lvm.new_closure_value(Closure::new(closure.module_id, f, base_closure))
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
                        lvm.modules[closure.module_id].const_list[*i].clone()
                    {
                        if let Some(module) = lvm.libs.get(&v) {
                            match <&LuciaTable>::try_from(*module) {
                                Ok(_) => self.operate_stack.push(*module),
                                Err(_) => return Err(IMPORT_ERROR),
                            }
                        } else {
                            let mut path = PathBuf::new();
                            if let Some(v) = lvm.get_global_variable("__module_path__") {
                                path.push(try_error!(lvm, String::try_from(v)));
                            }
                            path.push(v);
                            path.set_extension("lucia");
                            let input_file = fs::read_to_string(path).expect("Read file error!");
                            lvm.modules.push(Program::try_from(&input_file)?);
                            let module = lvm.run_module(lvm.modules.len() - 1)?;
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
                    let module = try_error!(
                        lvm,
                        <&LuciaTable>::try_from(
                            *self.operate_stack.last().ok_or_else(|| STACK_ERROR)?,
                        )
                    );
                    if let ConstlValue::Str(t) =
                        &lvm.modules[closure.module_id].const_list[*i].clone()
                    {
                        self.operate_stack.push(
                            module
                                .raw_get(&lvm.get_builtin_str(t))
                                .ok_or_else(|| IMPORT_ERROR)?,
                        );
                    } else {
                        return Err(IMPORT_ERROR);
                    }
                }
                OPCode::ImportGlob => {
                    let module = try_error!(
                        lvm,
                        <&LuciaTable>::try_from(
                            self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?,
                        )
                    );
                    for (k, v) in module.clone() {
                        lvm.set_global_variable(try_error!(lvm, String::try_from(k)), v);
                    }
                }
                OPCode::BuildTable(i) => {
                    let mut temp: Vec<LuciaValue> = Vec::new();
                    for _ in 0..(*i * 2) {
                        temp.push(self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?);
                    }
                    let mut table: LuciaTable = LuciaTable::new();
                    for _ in 0..*i {
                        let arg1 = temp.pop().expect("unexpect error");
                        let arg2 = temp.pop().expect("unexpect error");
                        if let LuciaValue::GCObject(_) = arg1 {
                            if String::try_from(arg1).is_err() {
                                return_type_error!(lvm, build_table_error!(arg1));
                            }
                        }
                        table.set(&arg1, arg2);
                    }
                    self.operate_stack.push(lvm.new_table_value(table));
                }
                OPCode::GetAttr => get_table!(lvm, "__getattr__"),
                OPCode::GetItem => get_table!(lvm, "__getitem__"),
                OPCode::SetAttr => set_table!(lvm, "__setattr__"),
                OPCode::SetItem => set_table!(lvm, "__setitem__"),
                OPCode::Neg => {
                    let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                    match <&LuciaTable>::try_from(arg1) {
                        Ok(v) => match v.get(&lvm.get_builtin_str("__neg__")) {
                            Some(v) => {
                                self.operate_stack.push(v);
                                self.operate_stack.push(arg1);
                                let return_value = call!(lvm, 1)?;
                                self.operate_stack.push(return_value);
                            }
                            None => return_type_error!(
                                lvm,
                                unsupported_operand_type!(code.clone(), arg1)
                            ),
                        },
                        Err(_) => self.operate_stack.push(match arg1 {
                            LuciaValue::Int(v) => LuciaValue::Int(-v),
                            LuciaValue::Float(v) => LuciaValue::Float(-v),
                            _ => return_type_error!(
                                lvm,
                                unsupported_operand_type!(code.clone(), arg1)
                            ),
                        }),
                    }
                }
                OPCode::Not => {
                    let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                    self.operate_stack
                        .push(LuciaValue::Bool(!try_error!(lvm, bool::try_from(arg1))));
                }
                OPCode::Add => {
                    let arg2 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                    let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                    match arg1.value_type() {
                        LuciaValueType::Str => {
                            match (String::try_from(arg1), String::try_from(arg2)) {
                                (Ok(v1), Ok(v2)) => {
                                    self.operate_stack.push(lvm.new_str_value(v1 + &v2))
                                }
                                _ => return_type_error!(
                                    lvm,
                                    unsupported_operand_type!(code.clone(), arg1, arg2)
                                ),
                            };
                        }
                        LuciaValueType::Table => {
                            match <&LuciaTable>::try_from(arg1)
                                .expect("unexpect error")
                                .get(&lvm.get_builtin_str("__add__"))
                            {
                                Some(v) => {
                                    self.operate_stack.push(v);
                                    self.operate_stack.push(arg1);
                                    self.operate_stack.push(arg2);
                                    let return_value = call!(lvm, 2)?;
                                    self.operate_stack.push(return_value);
                                }
                                None => return_type_error!(
                                    lvm,
                                    unsupported_operand_type!(code.clone(), arg1, arg2)
                                ),
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
                                _ => return_type_error!(
                                    lvm,
                                    unsupported_operand_type!(code.clone(), arg1, arg2)
                                ),
                            });
                        }
                    }
                }
                OPCode::Sub => run_bin_op!(lvm,-, "__sub__", code.clone()),
                OPCode::Mul => run_bin_op!(lvm,*, "__mul__", code.clone()),
                OPCode::Div => run_bin_op!(lvm,/, "__div__", code.clone()),
                OPCode::Mod => run_bin_op!(lvm,%, "__mod__", code.clone()),
                OPCode::Eq => run_eq_ne!(lvm,==, "__eq__", code.clone()),
                OPCode::Ne => run_eq_ne!(lvm,!=, "__ne__", code.clone()),
                OPCode::Gt => run_compare!(lvm,>, "__gt__", code.clone()),
                OPCode::Ge => run_compare!(lvm,>=, "__ge__", code.clone()),
                OPCode::Lt => run_compare!(lvm,<, "__lt__", code.clone()),
                OPCode::Le => run_compare!(lvm,<=, "__le__", code.clone()),
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
                    let return_value = lvm.call(
                        *self.operate_stack.last().ok_or_else(|| STACK_ERROR)?,
                        Vec::new(),
                    )?;
                    if return_value == LuciaValue::Null {
                        self.pc = *i;
                        continue;
                    } else {
                        self.operate_stack.push(return_value);
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
                    let return_value = call!(lvm, *i)?;
                    self.operate_stack.push(return_value);
                }
                OPCode::TryCall(i) => {
                    let return_value = call!(lvm, *i)?;
                    if return_value.is_error() {
                        return Ok(return_value);
                    }
                    self.operate_stack.push(return_value);
                }
                OPCode::Return => {
                    if closure.function.kind == FunctionKind::Do {
                        let mut temp = LuciaTable::new();
                        for i in 0..closure.function.local_names.len() {
                            temp.set(
                                &lvm.new_str_value(closure.function.local_names[i].clone()),
                                closure.variables[i],
                            )
                        }
                        return Ok(lvm.new_table_value(temp));
                    } else {
                        return self.operate_stack.pop().ok_or_else(|| STACK_ERROR);
                    }
                }
                OPCode::Throw => {
                    let arg1 = self.operate_stack.pop().ok_or_else(|| STACK_ERROR)?;
                    if let LuciaValue::GCObject(v) = arg1 {
                        unsafe { (*v).is_error = true }
                    }
                    return Ok(arg1);
                }
                OPCode::JumpTarget(_) => return Err(PROGRAM_ERROR),
            }
            self.pc += 1;
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lvm {
    pub modules: Vec<Program>,
    pub global_variables: HashMap<String, LuciaValue>,
    pub builtin_variables: HashMap<String, LuciaValue>,
    pub libs: HashMap<String, LuciaValue>,
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

    pub fn run(&mut self) -> LResult<LuciaValue> {
        self.run_module(0)
    }

    pub fn run_module(&mut self, module_id: usize) -> LResult<LuciaValue> {
        if module_id >= self.modules.len() {
            return Err(PROGRAM_ERROR);
        }
        let func = self.modules[module_id]
            .func_list
            .first()
            .ok_or_else(|| PROGRAM_ERROR)?
            .clone();
        let callee = self.new_closure_value(Closure::new(module_id, func, None));
        self.call(callee, Vec::new())
    }

    pub fn call(
        &mut self,
        mut callee: LuciaValue,
        mut args: Vec<LuciaValue>,
    ) -> LResult<LuciaValue> {
        if let Ok(t) = <&mut LuciaTable>::try_from(callee) {
            args.insert(0, callee);
            callee = match t.get(&self.get_builtin_str("__call__")) {
                Some(v) => v,
                None => return_type_error!(self, not_callable_error!(callee)),
            };
        }

        match callee {
            LuciaValue::ExtFunction(f) => f(args, self),
            LuciaValue::GCObject(gc_obj) => {
                match unsafe { &mut gc_obj.as_mut().expect("unexpect error").kind } {
                    GCObjectKind::Closure(v) => {
                        let params_num = v.function.params.len();
                        match args.len().cmp(&params_num) {
                            std::cmp::Ordering::Less => return_type_error!(
                                self,
                                call_arguments_error!(
                                    Some(Box::new(v.clone())),
                                    params_num,
                                    args.len()
                                )
                            ),
                            std::cmp::Ordering::Equal => {
                                v.variables[..params_num].copy_from_slice(&args[..]);
                                if v.function.variadic.is_some() {
                                    v.variables[params_num] =
                                        self.new_table_value(LuciaTable::new());
                                }
                            }
                            std::cmp::Ordering::Greater => {
                                if v.function.variadic.is_none() {
                                    return_type_error!(
                                        self,
                                        call_arguments_error!(
                                            Some(Box::new(v.clone())),
                                            params_num,
                                            args.len()
                                        )
                                    )
                                } else {
                                    let t = args.split_off(params_num);
                                    v.variables[..params_num].copy_from_slice(&args[..]);
                                    v.variables[params_num] = self.new_table_value(t.into());
                                }
                            }
                        }

                        let current_frame = self.current_frame;

                        let mut frame =
                            Frame::new(gc_obj, self.current_frame, v.function.stack_size);
                        self.current_frame = Some(NonNull::new(&mut frame).unwrap());
                        let return_value = frame.run(self);

                        self.current_frame = current_frame;
                        return_value
                    }
                    GCObjectKind::ExtClosure(f) => f(args, self),
                    _ => return_type_error!(self, not_callable_error!(callee)),
                }
            }
            _ => return_type_error!(self, not_callable_error!(callee)),
        }
    }

    pub fn set_global_variable(&mut self, key: String, value: LuciaValue) {
        self.global_variables.insert(key, value);
    }

    pub fn get_global_variable(&self, key: &str) -> Option<LuciaValue> {
        self.global_variables.get(key).copied()
    }

    pub fn get_builtin_variable(&self, key: &str) -> Option<LuciaValue> {
        self.builtin_variables.get(key).copied()
    }

    pub fn get_builtin_str(&mut self, key: &str) -> LuciaValue {
        LuciaValue::GCObject(match self.builtin_str_value.get(key) {
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
    pub fn new_gc_value(&mut self, value: GCObjectKind) -> LuciaValue {
        LuciaValue::GCObject(self.new_gc_object(value))
    }

    #[inline]
    pub fn new_str_value(&mut self, value: String) -> LuciaValue {
        self.new_gc_value(GCObjectKind::Str(value))
    }

    #[inline]
    pub fn new_table_value(&mut self, value: LuciaTable) -> LuciaValue {
        self.new_gc_value(GCObjectKind::Table(value))
    }

    #[inline]
    pub fn new_closure_value(&mut self, value: Closure) -> LuciaValue {
        self.new_gc_value(GCObjectKind::Closure(value))
    }

    #[inline]
    pub fn new_ext_closure_value(&mut self, value: Box<ExtClosure>) -> LuciaValue {
        self.new_gc_value(GCObjectKind::ExtClosure(value))
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
                    Self::gc_mark_object(frame.closure);
                    for value in &frame.operate_stack {
                        if let LuciaValue::GCObject(ptr) = value {
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
                if let LuciaValue::GCObject(ptr) = v {
                    Self::gc_mark_object(*ptr);
                }
            }
            for v in self.builtin_variables.values() {
                if let LuciaValue::GCObject(ptr) = v {
                    Self::gc_mark_object(*ptr);
                }
            }
            for v in self.builtin_str_value.values() {
                Self::gc_mark_object(*v);
            }
            // sweep
            let mut new_heap = Vec::new();
            for ptr in &self.heap {
                if (**ptr).gc_state {
                    ptr.drop_in_place();
                    dealloc(*ptr as *mut u8, self.mem_layout);
                } else {
                    new_heap.push(*ptr);
                }
            }
            self.heap = new_heap;
        }
    }

    fn gc_mark_object(ptr: *mut GCObject) {
        unsafe {
            (*ptr).gc_state = false;
            match &(*ptr).kind {
                GCObjectKind::Table(table) => {
                    for (_, v) in table.clone() {
                        if let LuciaValue::GCObject(ptr) = v {
                            Self::gc_mark_object(ptr);
                        }
                    }
                }
                GCObjectKind::Closure(closure) => {
                    if let Some(ptr) = closure.base_closure {
                        Self::gc_mark_object(ptr.as_ptr());
                    }
                    for v in &closure.variables {
                        if let LuciaValue::GCObject(ptr) = v {
                            Self::gc_mark_object(*ptr);
                        }
                    }
                }
                GCObjectKind::Str(_) => (),
                GCObjectKind::ExtClosure(_) => (),
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
