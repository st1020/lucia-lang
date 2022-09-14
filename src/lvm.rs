use core::ptr::NonNull;
use std::alloc::{alloc, dealloc, Layout};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fs;
use std::path::PathBuf;

use crate::codegen::{JumpTarget, LucylData, OPCode, Program};
use crate::errors::{LResult, LucyError, RuntimeErrorKind, TypeErrorKind};
use crate::libs;
use crate::object::*;

#[macro_export]
macro_rules! str_to_value {
    ($lvm:expr ,$name:expr) => {
        $lvm.new_gc_value(GCObjectKind::Str(String::from($name)))
    };
}

#[macro_export]
macro_rules! unsupported_operand_type {
    ($operator:expr, $arg1:expr) => {
        LucyError::TypeError(TypeErrorKind::OperatorError {
            operator: $operator,
            operand: (Some($arg1.value_type()), None),
        })
    };
    ($operator:expr, $arg1:expr, $arg2:expr) => {
        LucyError::TypeError(TypeErrorKind::OperatorError {
            operator: $operator,
            operand: (Some($arg1.value_type()), Some($arg2.value_type())),
        })
    };
}

#[macro_export]
macro_rules! type_convert_error {
    ($from:expr, $to:expr) => {
        LucyError::ConvertError {
            from: $from,
            to: $to,
        }
    };
}

macro_rules! stack_error {
    () => {
        LucyError::RuntimeError(RuntimeErrorKind::StackError)
    };
}

macro_rules! upvalue_error {
    () => {
        LucyError::RuntimeError(RuntimeErrorKind::UpvalueError)
    };
}

macro_rules! program_error {
    () => {
        LucyError::RuntimeError(RuntimeErrorKind::ProgramError)
    };
}

#[macro_export]
macro_rules! not_callable_error {
    ($value:expr) => {
        LucyError::TypeError(TypeErrorKind::NotCallableError($value.value_type()))
    };
}

#[derive(Debug, Clone)]
pub struct Frame {
    pc: usize,
    closuer: *mut GCObject,
    operate_stack: Vec<LucyValue>,
    prev_frame: Option<NonNull<Frame>>,
    lvm: *mut Lvm,
}

impl Frame {
    pub fn new(
        closuer: *mut GCObject,
        lvm: *mut Lvm,
        prev_frame: Option<NonNull<Frame>>,
        stack_size: usize,
    ) -> Self {
        Frame {
            pc: 0,
            closuer,
            operate_stack: Vec::with_capacity(stack_size),
            prev_frame,
            lvm,
        }
    }

    pub fn run(&mut self) -> LResult<LucyValue> {
        macro_rules! run_default {
            ($block:expr, $arg1:ident, $arg2:ident, $special_name:expr, $operator:expr) => {
                if $arg1.value_type() == LucyValueType::Table {
                    match <&LucyTable>::try_from($arg1)
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
            ($op: tt, $special_name:expr, $operator:expr) => {
                {
                    let arg2 = self.operate_stack.pop().ok_or_else(||stack_error!())?;
                    let arg1 = self.operate_stack.pop().ok_or_else(||stack_error!())?;
                    run_default!({
                        self.operate_stack.push(match (arg1, arg2) {
                            (LucyValue::Int(v1), LucyValue::Int(v2)) => LucyValue::Int(v1 $op v2),
                            (LucyValue::Float(v1), LucyValue::Float(v2)) => LucyValue::Float(v1 $op v2),
                            _ => return Err(unsupported_operand_type!($operator, arg1, arg2)),
                        });
                    }, arg1, arg2, $special_name, $operator)
                }
            };
        }

        macro_rules! run_eq_ne {
            ($op: tt, $special_name:expr, $operator:expr) => {
                {
                    let arg2 = self.operate_stack.pop().ok_or_else(||stack_error!())?;
                    let arg1 = self.operate_stack.pop().ok_or_else(||stack_error!())?;
                    run_default!({
                        self.operate_stack.push(LucyValue::Bool(arg1 $op arg2));
                    }, arg1, arg2, $special_name, $operator)
                }
            };
        }

        macro_rules! run_compare {
            ($op: tt, $special_name:expr, $operator:expr) => {
                {
                    let arg2 = self.operate_stack.pop().ok_or_else(||stack_error!())?;
                    let arg1 = self.operate_stack.pop().ok_or_else(||stack_error!())?;
                    run_default!({
                        self.operate_stack.push(LucyValue::Bool(match (arg1, arg2) {
                            (LucyValue::Int(v1), LucyValue::Int(v2)) => v1 $op v2,
                            (LucyValue::Float(v1), LucyValue::Float(v2)) => v1 $op v2,
                            _ => return Err(unsupported_operand_type!($operator, arg1, arg2)),
                        }));
                    }, arg1, arg2, $special_name, $operator)
                }
            };
        }

        let lvm = unsafe { self.lvm.as_mut().expect("unexpect error") };
        let closuer = unsafe {
            match &mut self.closuer.as_mut().unwrap().kind {
                GCObjectKind::Closuer(v) => v,
                _ => panic!(),
            }
        };
        loop {
            let code = closuer
                .function
                .code_list
                .get(self.pc)
                .ok_or_else(|| program_error!())?;
            match code {
                OPCode::Pop => {
                    self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                }
                OPCode::Dup => {
                    self.operate_stack
                        .push(*self.operate_stack.last().ok_or_else(|| stack_error!())?);
                }
                OPCode::DupTwo => {
                    let a = self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                    let b = self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                    self.operate_stack.push(b);
                    self.operate_stack.push(a);
                }
                OPCode::Rot => {
                    let a = self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                    let b = self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                    self.operate_stack.push(a);
                    self.operate_stack.push(b);
                }
                OPCode::LoadLocal(i) => {
                    self.operate_stack.push(closuer.variables[*i]);
                }
                OPCode::LoadGlobal(i) => {
                    let t = &closuer.function.global_names[*i];
                    self.operate_stack.push(
                        lvm.get_global_variable(t)
                            .unwrap_or(lvm.get_builtin_variable(t).unwrap_or(LucyValue::Null)),
                    );
                }
                OPCode::LoadUpvalue(i) => {
                    let (_, func_count, upvalue_id) = closuer.function.upvalue_names[*i];
                    let mut base_closuer = closuer.base_closuer;
                    for _ in 0..func_count {
                        base_closuer = match unsafe {
                            &base_closuer.ok_or_else(|| upvalue_error!())?.as_ref().kind
                        } {
                            GCObjectKind::Closuer(v) => v.base_closuer,
                            _ => return Err(upvalue_error!()),
                        };
                    }
                    match unsafe { &base_closuer.ok_or_else(|| upvalue_error!())?.as_ref().kind } {
                        GCObjectKind::Closuer(v) => {
                            self.operate_stack.push(v.variables[upvalue_id])
                        }
                        _ => return Err(upvalue_error!()),
                    };
                }
                OPCode::LoadConst(i) => {
                    let t = lvm.module_list[closuer.module_id].const_list[*i].clone();
                    let v = match t {
                        LucylData::Null => LucyValue::Null,
                        LucylData::Bool(v) => LucyValue::Bool(v),
                        LucylData::Int(v) => LucyValue::Int(v),
                        LucylData::Float(v) => LucyValue::Float(v),
                        LucylData::Str(v) => lvm.new_gc_value(GCObjectKind::Str(v)),
                        LucylData::Func(func_id) => {
                            let f = lvm.module_list[closuer.module_id].func_list[func_id].clone();
                            lvm.new_gc_value(GCObjectKind::Closuer(Closuer {
                                module_id: closuer.module_id,
                                base_closuer: if f.is_closure {
                                    NonNull::new(self.closuer)
                                } else {
                                    None
                                },
                                variables: {
                                    let mut temp: Vec<LucyValue> =
                                        Vec::with_capacity(f.local_names.len());
                                    for _ in 0..f.local_names.len() {
                                        temp.push(LucyValue::Null);
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
                    closuer.variables[*i] =
                        self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                }
                OPCode::StoreGlobal(i) => {
                    lvm.set_global_variable(
                        closuer.function.global_names[*i].clone(),
                        self.operate_stack.pop().ok_or_else(|| stack_error!())?,
                    );
                }
                OPCode::StoreUpvalue(i) => {
                    let (_, func_count, upvalue_id) = closuer.function.upvalue_names[*i];
                    let mut base_closuer = closuer.base_closuer;
                    for _ in 0..func_count {
                        base_closuer = match unsafe {
                            &base_closuer.ok_or_else(|| upvalue_error!())?.as_ref().kind
                        } {
                            GCObjectKind::Closuer(v) => v.base_closuer,
                            _ => return Err(upvalue_error!()),
                        };
                    }
                    match unsafe {
                        &mut base_closuer.ok_or_else(|| upvalue_error!())?.as_mut().kind
                    } {
                        GCObjectKind::Closuer(v) => {
                            v.variables[upvalue_id] =
                                self.operate_stack.pop().ok_or_else(|| stack_error!())?
                        }
                        _ => return Err(upvalue_error!()),
                    };
                }
                OPCode::Import(i) => {
                    if let LucylData::Str(v) =
                        lvm.module_list[closuer.module_id].const_list[*i].clone()
                    {
                        if let Some(v) = v.strip_prefix("std/") {
                            match lvm.std_libs.get(&String::from(v)) {
                                Some(module) => match <&LucyTable>::try_from(*module) {
                                    Ok(_) => self.operate_stack.push(*module),
                                    Err(_) => return Err(LucyError::ImportError),
                                },
                                None => return Err(LucyError::ImportError),
                            }
                        } else {
                            let mut path = PathBuf::new();
                            if let Some(v) = lvm.get_global_variable("__module_path__") {
                                path.push(String::try_from(v)?);
                            }
                            path.push(v);
                            path.set_extension("lucy");
                            let input_file = fs::read_to_string(path).expect("Read file error!");
                            lvm.module_list.push(Program::try_from(&input_file)?);
                            let module = lvm.run_module(lvm.module_list.len() - 1)?;
                            match <&LucyTable>::try_from(module) {
                                Ok(_) => self.operate_stack.push(module),
                                Err(_) => return Err(LucyError::ImportError),
                            }
                        }
                    } else {
                        return Err(program_error!());
                    }
                }
                OPCode::ImportFrom(i) => {
                    let module = <&LucyTable>::try_from(
                        *self.operate_stack.last().ok_or_else(|| stack_error!())?,
                    )?;
                    if let LucylData::Str(t) = &lvm.module_list[closuer.module_id].const_list[*i] {
                        self.operate_stack.push(
                            module
                                .raw_get_by_str(t)
                                .ok_or_else(|| LucyError::ImportError)?,
                        );
                    } else {
                        return Err(LucyError::ImportError);
                    }
                }
                OPCode::ImportGlob => {
                    let module = <&LucyTable>::try_from(
                        self.operate_stack.pop().ok_or_else(|| stack_error!())?,
                    )?;
                    for (k, v) in module.clone() {
                        lvm.set_global_variable(String::try_from(k)?, v);
                    }
                }
                OPCode::BuildTable(i) => {
                    let mut temp: Vec<LucyValue> = Vec::new();
                    for _ in 0..(*i * 2) {
                        temp.push(self.operate_stack.pop().ok_or_else(|| stack_error!())?);
                    }
                    let mut table: Vec<(LucyValue, LucyValue)> = Vec::new();
                    for _ in 0..*i {
                        let arg1 = temp.pop().expect("unexpect error");
                        let arg2 = temp.pop().expect("unexpect error");
                        if let LucyValue::GCObject(_) = arg1 {
                            if String::try_from(arg1).is_err() {
                                return Err(LucyError::BuildTableError);
                            }
                        }
                        table.push((arg1, arg2));
                    }
                    self.operate_stack
                        .push(lvm.new_gc_value(GCObjectKind::Table(LucyTable(table))));
                }
                OPCode::GetAttr | OPCode::GetItem => {
                    let arg2 = self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                    let arg1 = self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                    let t = <&mut LucyTable>::try_from(arg1)?;
                    match t.get_by_str(match code {
                        OPCode::GetAttr => "__getattr__",
                        OPCode::GetItem => "__getitem__",
                        _ => panic!("unexpect error"),
                    }) {
                        Some(v) => {
                            self.operate_stack.push(v);
                            self.operate_stack.push(arg1);
                            self.operate_stack.push(arg2);
                            self.call(2, true)?;
                        }
                        None => self
                            .operate_stack
                            .push(t.get(&arg2).unwrap_or(LucyValue::Null)),
                    }
                }
                OPCode::SetAttr | OPCode::SetItem => {
                    let arg3 = self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                    let arg2 = self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                    let arg1 = self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                    let t = <&mut LucyTable>::try_from(arg1)?;
                    match t.get_by_str(match code {
                        OPCode::SetAttr => "__setattr__",
                        OPCode::SetItem => "__setitem__",
                        _ => panic!("unexpect error"),
                    }) {
                        Some(v) => {
                            self.operate_stack.push(v);
                            self.operate_stack.push(arg1);
                            self.operate_stack.push(arg2);
                            self.operate_stack.push(arg3);
                            self.call(3, true)?;
                        }
                        None => t.set(&arg2, arg3),
                    }
                }
                OPCode::Neg => {
                    let arg1 = self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                    match <&LucyTable>::try_from(arg1) {
                        Ok(v) => match v.get_by_str("__neg__") {
                            Some(v) => {
                                self.operate_stack.push(v);
                                self.operate_stack.push(arg1);
                                self.call(1, true)?;
                            }
                            None => return Err(unsupported_operand_type!(code.clone(), arg1)),
                        },
                        Err(_) => self.operate_stack.push(match arg1 {
                            LucyValue::Int(v) => LucyValue::Int(-v),
                            LucyValue::Float(v) => LucyValue::Float(-v),
                            _ => return Err(unsupported_operand_type!(code.clone(), arg1)),
                        }),
                    }
                }
                OPCode::Not => {
                    let arg1 = self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                    self.operate_stack
                        .push(LucyValue::Bool(!bool::try_from(arg1)?));
                }
                OPCode::Add => {
                    let arg2 = self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                    let arg1 = self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                    match arg1.value_type() {
                        LucyValueType::Str => {
                            match (String::try_from(arg1), String::try_from(arg2)) {
                                (Ok(v1), Ok(v2)) => lvm.new_gc_value(GCObjectKind::Str(v1 + &v2)),
                                _ => {
                                    return Err(unsupported_operand_type!(code.clone(), arg1, arg2))
                                }
                            };
                        }
                        LucyValueType::Table => {
                            match <&LucyTable>::try_from(arg1)
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
                                (LucyValue::Int(v1), LucyValue::Int(v2)) => LucyValue::Int(v1 + v2),
                                (LucyValue::Float(v1), LucyValue::Float(v2)) => {
                                    LucyValue::Float(v1 + v2)
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
                    let arg2 = self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                    let arg1 = self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                    self.operate_stack.push(LucyValue::Bool(match (arg1, arg2) {
                        (LucyValue::Null, LucyValue::Null) => true,
                        (LucyValue::Bool(v1), LucyValue::Bool(v2)) => v1 == v2,
                        (LucyValue::Int(v1), LucyValue::Int(v2)) => v1 == v2,
                        (LucyValue::Float(v1), LucyValue::Float(v2)) => v1 == v2,
                        (LucyValue::GCObject(v1), LucyValue::GCObject(v2)) => v1 == v2,
                        _ => false,
                    }));
                }
                OPCode::For(JumpTarget(i)) => {
                    self.call(0, false)?;
                    if self.operate_stack.last().ok_or_else(|| stack_error!())? == &LucyValue::Null
                    {
                        self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                        self.pc = *i;
                        continue;
                    }
                }
                OPCode::Jump(JumpTarget(i)) => {
                    self.pc = *i;
                    continue;
                }
                OPCode::JumpIfFalse(JumpTarget(i)) => {
                    let arg1 = self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                    if let Ok(v) = bool::try_from(arg1) {
                        if !v {
                            self.pc = *i;
                            continue;
                        }
                    }
                }
                OPCode::JumpIfTureOrPop(JumpTarget(i)) => {
                    let arg1 = self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                    if let Ok(v) = bool::try_from(arg1) {
                        if v {
                            self.pc = *i;
                            continue;
                        } else {
                            self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                        }
                    }
                }
                OPCode::JumpIfFalseOrPop(JumpTarget(i)) => {
                    let arg1 = self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                    if let Ok(v) = bool::try_from(arg1) {
                        if !v {
                            self.pc = *i;
                            continue;
                        } else {
                            self.operate_stack.pop().ok_or_else(|| stack_error!())?;
                        }
                    }
                }
                OPCode::Call(i) => {
                    self.call(*i, true)?;
                }
                OPCode::Return => {
                    return Ok(self.operate_stack.pop().ok_or_else(|| stack_error!())?);
                }
                OPCode::JumpTarget(_) => return Err(program_error!()),
            }
            self.pc += 1;
        }
    }

    fn call(&mut self, arg_num: usize, pop: bool) -> LResult<()> {
        let lvm = unsafe { self.lvm.as_mut().expect("unexpect error") };

        let mut arguments = Vec::with_capacity(arg_num);
        for _ in 0..arg_num {
            arguments.push(self.operate_stack.pop().ok_or_else(|| stack_error!())?);
        }

        let callee = if pop {
            self.operate_stack.pop().ok_or_else(|| stack_error!())?
        } else {
            *self.operate_stack.last().ok_or_else(|| stack_error!())?
        };
        if let LucyValue::GCObject(gc_obj) = callee {
            let v = match unsafe { &mut gc_obj.as_mut().expect("unexpect error").kind } {
                GCObjectKind::Closuer(v) => v,
                GCObjectKind::Table(v) => <&mut Closuer>::try_from(
                    v.get_by_str("__call__")
                        .ok_or_else(|| not_callable_error!(callee))?,
                )
                .or_else(|_| Err(not_callable_error!(callee)))?,
                GCObjectKind::ExtClosuer(v) => {
                    arguments.reverse();
                    self.operate_stack.push(v(arguments, lvm));
                    return Ok(());
                }
                _ => return Err(not_callable_error!(callee)),
            };
            if v.function.params.len() != arg_num {
                return Err(LucyError::TypeError(TypeErrorKind::CallArgumentsError {
                    value: Box::new(v.clone()),
                    require: arg_num,
                    give: v.function.params.len(),
                }));
            }
            for i in 0..v.function.params.len() {
                v.variables[i] = arguments.pop().expect("unexpect error");
            }
            let mut frame = Frame::new(gc_obj, self.lvm, NonNull::new(self), v.function.stack_size);
            self.operate_stack.push(frame.run()?);
        } else if let LucyValue::ExtFunction(f) = callee {
            arguments.reverse();
            self.operate_stack.push(f(arguments, lvm));
        } else {
            return Err(not_callable_error!(callee));
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Lvm {
    pub module_list: Vec<Program>,
    pub global_variables: HashMap<String, LucyValue>,
    pub builtin_variables: HashMap<String, LucyValue>,
    pub std_libs: HashMap<String, LucyValue>,
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

    pub fn run(&mut self) -> LResult<LucyValue> {
        self.run_module(0)
    }

    pub fn run_module(&mut self, module_id: usize) -> LResult<LucyValue> {
        let func = self.module_list[module_id]
            .func_list
            .first()
            .ok_or_else(|| program_error!())?
            .clone();
        let stack_size = func.stack_size;
        let mut frame = Frame::new(
            self.new_gc_object(GCObjectKind::Closuer(Closuer {
                module_id,
                base_closuer: None,
                variables: {
                    let mut temp: Vec<LucyValue> = Vec::with_capacity(func.local_names.len());
                    for _ in 0..func.local_names.len() {
                        temp.push(LucyValue::Null);
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

    pub fn set_global_variable(&mut self, key: String, value: LucyValue) {
        self.global_variables.insert(key, value);
    }

    pub fn get_global_variable(&self, key: &str) -> Option<LucyValue> {
        match self.global_variables.get(key) {
            Some(v) => Some(*v),
            None => None,
        }
    }

    pub fn get_builtin_variable(&self, key: &str) -> Option<LucyValue> {
        match self.builtin_variables.get(key) {
            Some(v) => Some(*v),
            None => None,
        }
    }

    pub fn new_gc_value(&mut self, value: GCObjectKind) -> LucyValue {
        LucyValue::GCObject(self.new_gc_object(value))
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
                        if let LucyValue::GCObject(ptr) = v {
                            self.gc_mark_object(ptr);
                        }
                    }
                }
                GCObjectKind::Closuer(closuer) => {
                    if let Some(ptr) = closuer.base_closuer {
                        self.gc_mark_object(ptr.as_ptr());
                    }
                    for v in &closuer.variables {
                        if let LucyValue::GCObject(ptr) = v {
                            self.gc_mark_object(*ptr);
                        }
                    }
                }
                GCObjectKind::Str(_) => (),
                GCObjectKind::ExtClosuer(_) => (),
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
                self.gc_mark_object(frame.closuer);
                for value in &frame.operate_stack {
                    if let LucyValue::GCObject(ptr) = value {
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
