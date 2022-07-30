use core::ptr::NonNull;
use std::alloc::{alloc, dealloc, Layout};
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

use crate::codegen::{gen_code, JumpTarget, LucylData, OPCode, Program};
use crate::libs;
use crate::object::*;
use crate::{lexer, parser};

macro_rules! str_to_program {
    ($input:expr) => {
        parser::Parser::new(&mut lexer::tokenize($input)).parse()
    };
}

#[macro_export]
macro_rules! str_to_value {
    ($lvm:expr ,$name:expr) => {
        $lvm.new_gc_value(GCObjectKind::Str(String::from($name)))
    };
}

#[derive(Debug, Clone)]
pub struct Frame {
    pc: u32,
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
        stack_size: u32,
    ) -> Self {
        Frame {
            pc: 0,
            closuer,
            operate_stack: Vec::with_capacity(stack_size.try_into().unwrap()),
            prev_frame,
            lvm,
        }
    }

    pub fn run(&mut self) -> LucyValue {
        macro_rules! run_default {
            ($block:expr, $arg1:ident, $arg2:ident, $special_name:expr) => {
                match <&LucyTable>::try_from($arg1) {
                    Ok(v) => match v.get_by_str($special_name) {
                        Some(v) => {
                            self.operate_stack.push(v);
                            self.operate_stack.push($arg1);
                            self.operate_stack.push($arg2);
                            self.call(2, true);
                        }
                        None => panic!(),
                    },
                    Err(_) => $block,
                }
            };
        }

        macro_rules! run_bin_op {
            ($op: tt, $special_name:expr) => {
                let arg2 = self.operate_stack.pop().unwrap();
                let arg1 = self.operate_stack.pop().unwrap();
                run_default!({
                    self.operate_stack.push(match (arg1, arg2) {
                        (LucyValue::Int(v1), LucyValue::Int(v2)) => LucyValue::Int(v1 $op v2),
                        (LucyValue::Float(v1), LucyValue::Float(v2)) => LucyValue::Float(v1 $op v2),
                        _ => panic!(),
                    });
                }, arg1, arg2, $special_name)
            };
        }

        macro_rules! run_eq_ne {
            ($op: tt, $special_name:expr, $default:literal) => {
                let arg2 = self.operate_stack.pop().unwrap();
                let arg1 = self.operate_stack.pop().unwrap();
                run_default!({
                    self.operate_stack.push(LucyValue::Bool(match (arg1, arg2) {
                        (LucyValue::Null, LucyValue::Null) => !($default),
                        (LucyValue::Bool(v1), LucyValue::Bool(v2)) => v1 $op v2,
                        (LucyValue::Int(v1), LucyValue::Int(v2)) => v1 $op v2,
                        (LucyValue::Float(v1), LucyValue::Float(v2)) => v1 $op v2,
                        (LucyValue::GCObject(v1), LucyValue::GCObject(v2)) => unsafe { (*v1) $op (*v2) },
                        _ => $default,
                    }));
                }, arg1, arg2, $special_name)
            };
        }

        macro_rules! run_compare {
            ($op: tt, $special_name:expr) => {
                let arg2 = self.operate_stack.pop().unwrap();
                let arg1 = self.operate_stack.pop().unwrap();
                run_default!({
                    self.operate_stack.push(LucyValue::Bool(match (arg1, arg2) {
                        (LucyValue::Int(v1), LucyValue::Int(v2)) => v1 $op v2,
                        (LucyValue::Float(v1), LucyValue::Float(v2)) => v1 $op v2,
                        _ => panic!(),
                    }));
                }, arg1, arg2, $special_name)
            };
        }

        let lvm = unsafe { self.lvm.as_mut().unwrap() };
        let closuer = unsafe {
            match &mut self.closuer.as_mut().unwrap().kind {
                GCObjectKind::Closuer(v) => v,
                _ => panic!(),
            }
        };
        loop {
            let code = closuer.function.code_list.get(self.pc as usize).unwrap();
            match code {
                OPCode::Pop => {
                    self.operate_stack.pop();
                }
                OPCode::Dup => {
                    self.operate_stack.push(*self.operate_stack.last().unwrap());
                }
                OPCode::DupTwo => {
                    let a = self.operate_stack.pop().unwrap();
                    let b = self.operate_stack.pop().unwrap();
                    self.operate_stack.push(b);
                    self.operate_stack.push(a);
                }
                OPCode::Rot => {
                    let a = self.operate_stack.pop().unwrap();
                    let b = self.operate_stack.pop().unwrap();
                    self.operate_stack.push(a);
                    self.operate_stack.push(b);
                }
                OPCode::LoadLocal(i) => {
                    self.operate_stack.push(closuer.variables[*i as usize]);
                }
                OPCode::LoadGlobal(i) => {
                    let t = &closuer.function.global_names[*i as usize];
                    self.operate_stack.push(
                        lvm.get_global_variable(t)
                            .unwrap_or(lvm.get_builtin_variable(t).unwrap_or(LucyValue::Null)),
                    );
                }
                OPCode::LoadUpvalue(i) => {
                    let (_, func_count, upvalue_id) = closuer.function.upvalue_names[*i as usize];
                    let mut base_closuer = closuer.base_closuer;
                    for _ in 0..func_count {
                        base_closuer = match unsafe { &base_closuer.unwrap().as_ref().kind } {
                            GCObjectKind::Closuer(v) => v.base_closuer,
                            _ => panic!(),
                        };
                    }
                    match unsafe { &base_closuer.unwrap().as_ref().kind } {
                        GCObjectKind::Closuer(v) => {
                            self.operate_stack.push(v.variables[upvalue_id as usize])
                        }
                        _ => panic!(),
                    };
                }
                OPCode::LoadConst(i) => {
                    let t =
                        lvm.module_list[closuer.module_id as usize].const_list[*i as usize].clone();
                    let v = match t {
                        LucylData::Null => LucyValue::Null,
                        LucylData::Bool(v) => LucyValue::Bool(v),
                        LucylData::Int(v) => LucyValue::Int(v),
                        LucylData::Float(v) => LucyValue::Float(v),
                        LucylData::Str(v) => lvm.new_gc_value(GCObjectKind::Str(v)),
                        LucylData::Func(func_id) => {
                            let f = lvm.module_list[closuer.module_id as usize].func_list
                                [func_id as usize]
                                .clone();
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
                    closuer.variables[*i as usize] = self.operate_stack.pop().unwrap();
                }
                OPCode::StoreGlobal(i) => {
                    lvm.set_global_variable(
                        closuer.function.global_names[*i as usize].clone(),
                        self.operate_stack.pop().unwrap(),
                    );
                }
                OPCode::StoreUpvalue(i) => {
                    let (_, func_count, upvalue_id) = closuer.function.upvalue_names[*i as usize];
                    let mut base_closuer = closuer.base_closuer;
                    for _ in 0..func_count {
                        base_closuer = match unsafe { &base_closuer.unwrap().as_ref().kind } {
                            GCObjectKind::Closuer(v) => v.base_closuer,
                            _ => panic!(),
                        };
                    }
                    match unsafe { &mut base_closuer.unwrap().as_mut().kind } {
                        GCObjectKind::Closuer(v) => {
                            v.variables[upvalue_id as usize] = self.operate_stack.pop().unwrap()
                        }
                        _ => panic!(),
                    };
                }
                OPCode::Import(i) => {
                    if let LucylData::Str(mut v) =
                        lvm.module_list[closuer.module_id as usize].const_list[*i as usize].clone()
                    {
                        if v.starts_with("std/") {
                            v = String::from(v.strip_prefix("std/").unwrap());
                            match lvm.std_libs.get(&v) {
                                Some(module) => match <&LucyTable>::try_from(*module) {
                                    Ok(_) => self.operate_stack.push(*module),
                                    Err(_) => panic!("Import error"),
                                },
                                None => panic!("Import error"),
                            }
                        } else {
                            let mut path = PathBuf::new();
                            if let Some(v) = lvm.get_global_variable("__module_path__") {
                                path.push(String::try_from(v).unwrap());
                            }
                            path.push(v);
                            path.set_extension("lucy");
                            let input_file = fs::read_to_string(path).expect("Read file error!");
                            lvm.module_list.push(gen_code(str_to_program!(&input_file)));
                            let module =
                                lvm.run_module((lvm.module_list.len() - 1).try_into().unwrap());
                            match <&LucyTable>::try_from(module) {
                                Ok(_) => self.operate_stack.push(module),
                                Err(_) => panic!("Import error"),
                            }
                        }
                    } else {
                        panic!();
                    }
                }
                OPCode::ImportFrom(i) => {
                    let module: &LucyTable =
                        (*self.operate_stack.last().unwrap()).try_into().unwrap();
                    if let LucylData::Str(t) =
                        &lvm.module_list[closuer.module_id as usize].const_list[*i as usize]
                    {
                        self.operate_stack.push(module.raw_get_by_str(t).unwrap());
                    } else {
                        panic!("Import error")
                    }
                }
                OPCode::ImportGlob => {
                    let module: &LucyTable = self.operate_stack.pop().unwrap().try_into().unwrap();
                    for (k, v) in module.clone() {
                        lvm.set_global_variable(String::try_from(k).unwrap(), v);
                    }
                }
                OPCode::BuildTable(i) => {
                    let mut temp: Vec<LucyValue> = Vec::new();
                    for _ in 0..(*i * 2) {
                        temp.push(self.operate_stack.pop().unwrap());
                    }
                    let mut table: Vec<(LucyValue, LucyValue)> = Vec::new();
                    for _ in 0..*i {
                        let arg1 = temp.pop().unwrap();
                        let arg2 = temp.pop().unwrap();
                        if let LucyValue::GCObject(_) = arg1 {
                            if String::try_from(arg1).is_err() {
                                panic!()
                            }
                        }
                        table.push((arg1, arg2));
                    }
                    self.operate_stack
                        .push(lvm.new_gc_value(GCObjectKind::Table(LucyTable(table))));
                }
                OPCode::GetAttr | OPCode::GetItem => {
                    let arg2 = self.operate_stack.pop().unwrap();
                    let arg1 = self.operate_stack.pop().unwrap();
                    let t: &mut LucyTable = arg1.try_into().unwrap();
                    match t.get_by_str(match code {
                        OPCode::GetAttr => "__getattr__",
                        OPCode::GetItem => "__getitem__",
                        _ => panic!(),
                    }) {
                        Some(v) => {
                            self.operate_stack.push(v);
                            self.operate_stack.push(arg1);
                            self.operate_stack.push(arg2);
                            self.call(2, true);
                        }
                        None => self
                            .operate_stack
                            .push(t.get(&arg2).unwrap_or(LucyValue::Null)),
                    }
                }
                OPCode::SetAttr | OPCode::SetItem => {
                    let arg3 = self.operate_stack.pop().unwrap();
                    let arg2 = self.operate_stack.pop().unwrap();
                    let arg1 = self.operate_stack.pop().unwrap();
                    let t: &mut LucyTable = arg1.try_into().unwrap();
                    match t.get_by_str(match code {
                        OPCode::SetAttr => "__setattr__",
                        OPCode::SetItem => "__setitem__",
                        _ => panic!(),
                    }) {
                        Some(v) => {
                            self.operate_stack.push(v);
                            self.operate_stack.push(arg1);
                            self.operate_stack.push(arg2);
                            self.operate_stack.push(arg3);
                            self.call(3, true);
                        }
                        None => t.set(&arg2, arg3),
                    }
                }
                OPCode::Neg => {
                    let arg1 = self.operate_stack.pop().unwrap();
                    match <&LucyTable>::try_from(arg1) {
                        Ok(v) => match v.get_by_str("__neg__") {
                            Some(v) => {
                                self.operate_stack.push(v);
                                self.operate_stack.push(arg1);
                                self.call(1, true);
                            }
                            None => panic!(),
                        },
                        Err(_) => self.operate_stack.push(match arg1 {
                            LucyValue::Int(v) => LucyValue::Int(-v),
                            LucyValue::Float(v) => LucyValue::Float(-v),
                            _ => panic!(),
                        }),
                    }
                }
                OPCode::Not => {
                    let arg1 = self.operate_stack.pop().unwrap();
                    self.operate_stack
                        .push(LucyValue::Bool(!bool::try_from(arg1).unwrap()));
                }
                OPCode::Add => {
                    let arg2 = self.operate_stack.pop().unwrap();
                    let arg1 = self.operate_stack.pop().unwrap();
                    match <&LucyTable>::try_from(arg1) {
                        Ok(v) => match v.get_by_str("__add__") {
                            Some(v) => {
                                self.operate_stack.push(v);
                                self.operate_stack.push(arg1);
                                self.operate_stack.push(arg2);
                                self.call(2, true);
                            }
                            None => panic!(),
                        },
                        Err(_) => {
                            self.operate_stack.push(match (arg1, arg2) {
                                (LucyValue::Int(v1), LucyValue::Int(v2)) => LucyValue::Int(v1 + v2),
                                (LucyValue::Float(v1), LucyValue::Float(v2)) => {
                                    LucyValue::Float(v1 + v2)
                                }
                                (LucyValue::GCObject(v1), LucyValue::GCObject(v2)) => unsafe {
                                    match (&(*v1).kind, &(*v2).kind) {
                                        (GCObjectKind::Str(v1), GCObjectKind::Str(v2)) => {
                                            lvm.new_gc_value(GCObjectKind::Str(v1.clone() + v2))
                                        }
                                        _ => panic!(),
                                    }
                                },
                                _ => panic!(),
                            });
                        }
                    }
                }
                OPCode::Sub => {
                    run_bin_op!(-, "__sub__");
                }
                OPCode::Mul => {
                    run_bin_op!(*, "__mul__");
                }
                OPCode::Div => {
                    run_bin_op!(/, "__div__");
                }
                OPCode::Mod => {
                    run_bin_op!(%, "__mod__");
                }
                OPCode::Eq => {
                    run_eq_ne!(==, "__eq__", false);
                }
                OPCode::Ne => {
                    run_eq_ne!(!=, "__ne__", true);
                }
                OPCode::Gt => {
                    run_compare!(>, "__gt__");
                }
                OPCode::Ge => {
                    run_compare!(>=, "__ge__");
                }
                OPCode::Lt => {
                    run_compare!(<, "__lt__");
                }
                OPCode::Le => {
                    run_compare!(<=, "__le__");
                }
                OPCode::Is => {
                    let arg2 = self.operate_stack.pop().unwrap();
                    let arg1 = self.operate_stack.pop().unwrap();
                    self.operate_stack.push(LucyValue::Bool(arg1 == arg2));
                }
                OPCode::For(JumpTarget(i)) => {
                    self.call(0, false);
                    if self.operate_stack.last().unwrap() == &LucyValue::Null {
                        self.operate_stack.pop();
                        self.pc = *i;
                        continue;
                    }
                }
                OPCode::Jump(JumpTarget(i)) => {
                    self.pc = *i;
                    continue;
                }
                OPCode::JumpIfFalse(JumpTarget(i)) => {
                    let arg1 = self.operate_stack.pop().unwrap();
                    if let Ok(v) = bool::try_from(arg1) {
                        if !v {
                            self.pc = *i;
                            continue;
                        }
                    }
                }
                OPCode::JumpIfTureOrPop(JumpTarget(i)) => {
                    let arg1 = self.operate_stack.pop().unwrap();
                    if let Ok(v) = bool::try_from(arg1) {
                        if v {
                            self.pc = *i;
                            continue;
                        } else {
                            self.operate_stack.pop();
                        }
                    }
                }
                OPCode::JumpIfFalseOrPop(JumpTarget(i)) => {
                    let arg1 = self.operate_stack.pop().unwrap();
                    if let Ok(v) = bool::try_from(arg1) {
                        if !v {
                            self.pc = *i;
                            continue;
                        } else {
                            self.operate_stack.pop();
                        }
                    }
                }
                OPCode::Call(i) => {
                    self.call(*i, true);
                }
                OPCode::Return => {
                    return self.operate_stack.pop().unwrap();
                }
                OPCode::JumpTarget(_) => panic!(),
            }
            self.pc += 1;
        }
    }

    fn call(&mut self, arg_num: u32, pop: bool) {
        let mut arguments = Vec::with_capacity(arg_num.try_into().unwrap());
        for _ in 0..arg_num {
            arguments.push(self.operate_stack.pop().unwrap());
        }

        let callee = if pop {
            self.operate_stack.pop().unwrap()
        } else {
            *self.operate_stack.last().unwrap()
        };
        if let LucyValue::GCObject(gc_obj) = callee {
            let v = match unsafe { &mut gc_obj.as_mut().unwrap().kind } {
                GCObjectKind::Closuer(v) => v,
                GCObjectKind::Table(v) => v.get_by_str("__call__").unwrap().try_into().unwrap(),
                GCObjectKind::ExtClosuer(v) => {
                    arguments.reverse();
                    self.operate_stack
                        .push(v(arguments, unsafe { self.lvm.as_mut().unwrap() }));
                    return;
                }
                _ => panic!(),
            };
            if v.function.params.len() != arg_num.try_into().unwrap() {
                panic!()
            }
            for i in 0..v.function.params.len() {
                v.variables[i] = arguments.pop().unwrap();
            }
            let mut frame = Frame::new(gc_obj, self.lvm, NonNull::new(self), v.function.stack_size);
            self.operate_stack.push(frame.run());
        } else if let LucyValue::ExtFunction(f) = callee {
            arguments.reverse();
            self.operate_stack
                .push(f(arguments, unsafe { self.lvm.as_mut().unwrap() }));
        } else {
            panic!()
        }
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

    pub fn from_str(input: &str) -> Self {
        Lvm::new(gen_code(str_to_program!(input)))
    }

    pub fn run(&mut self) -> LucyValue {
        self.run_module(0)
    }

    pub fn run_module(&mut self, module_id: u32) -> LucyValue {
        let func = self.module_list[module_id as usize]
            .func_list
            .first()
            .unwrap()
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
        self.current_frame = NonNull::new(&mut frame).unwrap();
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

    fn gc_object(&self, ptr: *mut GCObject) {
        unsafe {
            (*ptr).gc_state = false;
            match &(*ptr).kind {
                GCObjectKind::Table(table) => {
                    for (_, v) in table.clone() {
                        if let LucyValue::GCObject(ptr) = v {
                            self.gc_object(ptr);
                        }
                    }
                }
                GCObjectKind::Closuer(closuer) => {
                    if let Some(ptr) = closuer.base_closuer {
                        self.gc_object(ptr.as_ptr());
                    }
                    for v in &closuer.variables {
                        if let LucyValue::GCObject(ptr) = v {
                            self.gc_object(*ptr);
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
            for ptr in &self.heap {
                (**ptr).gc_state = true;
            }
            let mut ptr = self.current_frame.as_ref();
            loop {
                self.gc_object(ptr.closuer);
                for value in &ptr.operate_stack {
                    if let LucyValue::GCObject(ptr) = value {
                        self.gc_object(*ptr);
                    }
                }
                match ptr.prev_frame {
                    Some(t) => ptr = t.as_ref(),
                    None => break,
                }
            }
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
