use core::ptr::NonNull;
use std::alloc::{alloc, dealloc, Layout};
use std::collections::HashMap;

use crate::codegen::{Function, JumpTarget, LucylValue, OPCode, Program};

#[derive(Debug, Clone, PartialEq)]
pub enum GCObjectKind {
    Str(String),
    Table(LucyTable),
    Closuer(Box<Closuer>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct GCObject {
    pub kind: GCObjectKind,
    pub gc_state: bool,
}

impl GCObject {
    pub fn new(kind: GCObjectKind) -> Self {
        Self {
            kind,
            gc_state: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum LucyData {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    GCObject(*mut GCObject),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LucyTable(Vec<(LucyData, LucyData)>);

impl LucyTable {
    pub fn get(&self, key: &LucyData) -> Option<LucyData> {
        for (k, v) in &self.0 {
            if k == key {
                return Some(v.clone());
            }
        }
        None
    }

    pub fn set(&mut self, key: &LucyData, value: LucyData) {
        for i in 0..self.0.len() {
            let (k, _) = &self.0[i];
            if k == key {
                self.0[i] = (key.clone(), value);
                return;
            }
        }
        self.0.push((key.clone(), value));
    }
}

impl IntoIterator for LucyTable {
    type Item = (LucyData, LucyData);
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug, Clone)]
pub struct Closuer {
    pub function: Function,
    pub base_closuer: Option<*mut GCObject>,
    pub variables: Vec<LucyData>,
}

impl PartialEq for Closuer {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
pub struct Frame {
    pc: u32,
    closuer: *mut GCObject,
    operate_stack: Vec<LucyData>,
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

    pub fn run(&mut self) -> LucyData {
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
                    self.operate_stack.push(
                        lvm.get_global_variable(&closuer.function.global_names[*i as usize])
                            .unwrap_or(LucyData::Null),
                    );
                }
                OPCode::LoadUpvalue(i) => {
                    let (_, func_count, upvalue_id) = closuer.function.upvalue_names[*i as usize];
                    let mut base_closuer = closuer.base_closuer;
                    for _ in 0..func_count {
                        base_closuer = match base_closuer {
                            Some(v) => unsafe {
                                match &(*v).kind {
                                    GCObjectKind::Closuer(v) => (*v).base_closuer,
                                    _ => panic!(),
                                }
                            },
                            None => panic!(),
                        };
                    }
                    match base_closuer {
                        Some(v) => unsafe {
                            match &(*v).kind {
                                GCObjectKind::Closuer(v) => {
                                    self.operate_stack.push(v.variables[upvalue_id as usize])
                                }
                                _ => panic!(),
                            }
                        },
                        None => panic!(),
                    }
                }
                OPCode::LoadConst(i) => {
                    let t = lvm.program.const_list[*i as usize].clone();
                    let v = match t {
                        LucylValue::Null => LucyData::Null,
                        LucylValue::Bool(v) => LucyData::Bool(v),
                        LucylValue::Int(v) => LucyData::Int(v),
                        LucylValue::Float(v) => LucyData::Float(v),
                        LucylValue::Str(v) => LucyData::GCObject(
                            lvm.new_gc_object(GCObject::new(GCObjectKind::Str(v))),
                        ),
                        LucylValue::Func(func_id) => {
                            let f = lvm.program.func_list[func_id as usize].clone();
                            LucyData::GCObject(lvm.new_gc_object(GCObject::new(
                                GCObjectKind::Closuer(Box::new(Closuer {
                                    base_closuer: Some(self.closuer),
                                    variables: {
                                        let mut temp: Vec<LucyData> =
                                            Vec::with_capacity(f.local_names.len());
                                        for _ in 0..f.local_names.len() {
                                            temp.push(LucyData::Null);
                                        }
                                        temp
                                    },
                                    function: f,
                                })),
                            )))
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
                        base_closuer = match base_closuer {
                            Some(v) => unsafe {
                                match &(*v).kind {
                                    GCObjectKind::Closuer(v) => (*v).base_closuer,
                                    _ => panic!(),
                                }
                            },
                            None => panic!(),
                        };
                    }
                    match base_closuer {
                        Some(v) => unsafe {
                            match &mut (*v).kind {
                                GCObjectKind::Closuer(v) => {
                                    v.variables[upvalue_id as usize] =
                                        self.operate_stack.pop().unwrap()
                                }
                                _ => panic!(),
                            }
                        },
                        None => panic!(),
                    }
                }
                OPCode::Import(_) => todo!(),
                OPCode::ImportFrom(_) => todo!(),
                OPCode::ImportGlob => todo!(),
                OPCode::BuildTable(i) => {
                    let mut temp: Vec<LucyData> = Vec::new();
                    for _ in 0..(*i * 2) {
                        temp.push(self.operate_stack.pop().unwrap());
                    }
                    let mut table: Vec<(LucyData, LucyData)> = Vec::new();
                    for _ in 0..*i {
                        let arg1 = temp.pop().unwrap();
                        let arg2 = temp.pop().unwrap();
                        if let LucyData::GCObject(_) = arg1 {
                            panic!()
                        }
                        table.push((arg1.clone(), arg2));
                    }
                    self.operate_stack.push(LucyData::GCObject(
                        lvm.new_gc_object(GCObject::new(GCObjectKind::Table(LucyTable(table)))),
                    ));
                }
                OPCode::GetAttr | OPCode::GetItem => {
                    let arg2 = self.operate_stack.pop().unwrap();
                    let arg1 = self.operate_stack.pop().unwrap();
                    if let LucyData::GCObject(t) = arg1 {
                        match unsafe { &(*t).kind } {
                            GCObjectKind::Table(v) => self
                                .operate_stack
                                .push(v.get(&arg2).unwrap_or(LucyData::Null)),
                            _ => panic!(),
                        }
                    } else {
                        panic!()
                    }
                }
                OPCode::SetAttr | OPCode::SetItem => {
                    let arg3 = self.operate_stack.pop().unwrap();
                    let arg2 = self.operate_stack.pop().unwrap();
                    let arg1 = self.operate_stack.pop().unwrap();
                    if let LucyData::GCObject(t) = arg1 {
                        match unsafe { &mut t.as_mut().unwrap().kind } {
                            GCObjectKind::Table(v) => v.set(&arg2, arg3),
                            _ => panic!(),
                        }
                    } else {
                        panic!()
                    }
                }
                OPCode::Neg => {
                    let arg1 = self.operate_stack.pop().unwrap();
                    let t = match arg1 {
                        LucyData::Int(v) => LucyData::Int(-v),
                        LucyData::Float(v) => LucyData::Float(-v),
                        _ => panic!(),
                    };
                    self.operate_stack.push(t);
                }
                OPCode::Not => {
                    let arg1 = self.operate_stack.pop().unwrap();
                    let t = match arg1 {
                        LucyData::Bool(v) => LucyData::Bool(!v),
                        _ => panic!(),
                    };
                    self.operate_stack.push(t);
                }
                OPCode::Add => {
                    let arg2 = self.operate_stack.pop().unwrap();
                    let arg1 = self.operate_stack.pop().unwrap();
                    let t = match (arg1, arg2) {
                        (LucyData::Int(v1), LucyData::Int(v2)) => LucyData::Int(v1 + v2),
                        (LucyData::Float(v1), LucyData::Float(v2)) => LucyData::Float(v1 + v2),
                        _ => panic!(),
                    };
                    self.operate_stack.push(t);
                }
                OPCode::Sub => {
                    let arg2 = self.operate_stack.pop().unwrap();
                    let arg1 = self.operate_stack.pop().unwrap();
                    let t = match (arg1, arg2) {
                        (LucyData::Int(v1), LucyData::Int(v2)) => LucyData::Int(v1 - v2),
                        (LucyData::Float(v1), LucyData::Float(v2)) => LucyData::Float(v1 - v2),
                        _ => panic!(),
                    };
                    self.operate_stack.push(t);
                }
                OPCode::Mul => {
                    let arg2 = self.operate_stack.pop().unwrap();
                    let arg1 = self.operate_stack.pop().unwrap();
                    let t = match (arg1, arg2) {
                        (LucyData::Int(v1), LucyData::Int(v2)) => LucyData::Int(v1 * v2),
                        (LucyData::Float(v1), LucyData::Float(v2)) => LucyData::Float(v1 * v2),
                        _ => panic!(),
                    };
                    self.operate_stack.push(t);
                }
                OPCode::Div => {
                    let arg2 = self.operate_stack.pop().unwrap();
                    let arg1 = self.operate_stack.pop().unwrap();
                    let t = match (arg1, arg2) {
                        (LucyData::Int(v1), LucyData::Int(v2)) => LucyData::Int(v1 / v2),
                        (LucyData::Float(v1), LucyData::Float(v2)) => LucyData::Float(v1 / v2),
                        _ => panic!(),
                    };
                    self.operate_stack.push(t);
                }
                OPCode::Mod => {
                    let arg2 = self.operate_stack.pop().unwrap();
                    let arg1 = self.operate_stack.pop().unwrap();
                    let t = match (arg1, arg2) {
                        (LucyData::Int(v1), LucyData::Int(v2)) => LucyData::Int(v1 % v2),
                        (LucyData::Float(v1), LucyData::Float(v2)) => LucyData::Float(v1 % v2),
                        _ => panic!(),
                    };
                    self.operate_stack.push(t);
                }
                OPCode::Eq => {
                    let arg2 = self.operate_stack.pop().unwrap();
                    let arg1 = self.operate_stack.pop().unwrap();
                    let t = LucyData::Bool(match (arg1, arg2) {
                        (LucyData::Null, LucyData::Null) => true,
                        (LucyData::Bool(v1), LucyData::Bool(v2)) => v1 == v2,
                        (LucyData::Int(v1), LucyData::Int(v2)) => v1 == v2,
                        (LucyData::Float(v1), LucyData::Float(v2)) => v1 == v2,
                        (LucyData::GCObject(v1), LucyData::GCObject(v2)) => unsafe {
                            match (&(*v1).kind, &(*v2).kind) {
                                (GCObjectKind::Str(v1), GCObjectKind::Str(v2)) => v1 == v2,
                                _ => false,
                            }
                        },
                        _ => false,
                    });
                    self.operate_stack.push(t);
                }
                OPCode::Ne => {
                    let arg2 = self.operate_stack.pop().unwrap();
                    let arg1 = self.operate_stack.pop().unwrap();
                    let t = LucyData::Bool(match (arg1, arg2) {
                        (LucyData::Null, LucyData::Null) => false,
                        (LucyData::Bool(v1), LucyData::Bool(v2)) => v1 != v2,
                        (LucyData::Int(v1), LucyData::Int(v2)) => v1 != v2,
                        (LucyData::Float(v1), LucyData::Float(v2)) => v1 != v2,
                        (LucyData::GCObject(v1), LucyData::GCObject(v2)) => unsafe {
                            match (&(*v1).kind, &(*v2).kind) {
                                (GCObjectKind::Str(v1), GCObjectKind::Str(v2)) => v1 != v2,
                                _ => true,
                            }
                        },
                        _ => true,
                    });
                    self.operate_stack.push(t);
                }
                OPCode::Gt => {
                    let arg2 = self.operate_stack.pop().unwrap();
                    let arg1 = self.operate_stack.pop().unwrap();
                    let t = LucyData::Bool(match (arg1, arg2) {
                        (LucyData::Int(v1), LucyData::Int(v2)) => v1 > v2,
                        (LucyData::Float(v1), LucyData::Float(v2)) => v1 > v2,
                        _ => panic!(),
                    });
                    self.operate_stack.push(t);
                }
                OPCode::Ge => {
                    let arg2 = self.operate_stack.pop().unwrap();
                    let arg1 = self.operate_stack.pop().unwrap();
                    let t = LucyData::Bool(match (arg1, arg2) {
                        (LucyData::Int(v1), LucyData::Int(v2)) => v1 >= v2,
                        (LucyData::Float(v1), LucyData::Float(v2)) => v1 >= v2,
                        _ => panic!(),
                    });
                    self.operate_stack.push(t);
                }
                OPCode::Lt => {
                    let arg2 = self.operate_stack.pop().unwrap();
                    let arg1 = self.operate_stack.pop().unwrap();
                    let t = LucyData::Bool(match (arg1, arg2) {
                        (LucyData::Int(v1), LucyData::Int(v2)) => v1 < v2,
                        (LucyData::Float(v1), LucyData::Float(v2)) => v1 < v2,
                        _ => panic!(),
                    });
                    self.operate_stack.push(t);
                }
                OPCode::Le => {
                    let arg2 = self.operate_stack.pop().unwrap();
                    let arg1 = self.operate_stack.pop().unwrap();
                    let t = LucyData::Bool(match (arg1, arg2) {
                        (LucyData::Int(v1), LucyData::Int(v2)) => v1 <= v2,
                        (LucyData::Float(v1), LucyData::Float(v2)) => v1 <= v2,
                        _ => panic!(),
                    });
                    self.operate_stack.push(t);
                }
                OPCode::Is => {
                    let arg2 = self.operate_stack.pop().unwrap();
                    let arg1 = self.operate_stack.pop().unwrap();
                    self.operate_stack.push(LucyData::Bool(arg1 == arg2));
                }
                OPCode::For(JumpTarget(i)) => {
                    self.call(0, false);
                    if self.operate_stack.last().unwrap() == &LucyData::Null {
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
                    match arg1 {
                        LucyData::Bool(v) => {
                            if !v {
                                self.pc = *i;
                                continue;
                            }
                        }
                        _ => (),
                    }
                }
                OPCode::JumpIfTureOrPop(JumpTarget(i)) => {
                    let arg1 = self.operate_stack.pop().unwrap();
                    match arg1 {
                        LucyData::Bool(v) => {
                            if v {
                                self.pc = *i;
                                continue;
                            } else {
                                self.operate_stack.pop();
                            }
                        }
                        _ => (),
                    }
                }
                OPCode::JumpIfFalseOrPop(JumpTarget(i)) => {
                    let arg1 = self.operate_stack.pop().unwrap();
                    match arg1 {
                        LucyData::Bool(v) => {
                            if !v {
                                self.pc = *i;
                                continue;
                            } else {
                                self.operate_stack.pop();
                            }
                        }
                        _ => (),
                    }
                }
                OPCode::Call(i) => {
                    self.call(*i, true);
                }
                OPCode::Goto(_) => todo!(),
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
        if let LucyData::GCObject(gc_obj) = callee {
            let v = unsafe {
                match &mut gc_obj.as_mut().unwrap().kind {
                    GCObjectKind::Closuer(v) => v,
                    _ => panic!(),
                }
            };
            if v.function.params.len() != arg_num.try_into().unwrap() {
                panic!()
            }
            for i in 0..v.function.params.len() {
                v.variables[i] = arguments.pop().unwrap();
            }
            let mut frame = Frame::new(gc_obj, self.lvm, NonNull::new(self), v.function.stack_size);
            self.operate_stack.push(frame.run());
        } else {
            panic!()
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lvm {
    pub program: Program,
    pub global_variables: HashMap<String, LucyData>,
    pub current_frame: NonNull<Frame>,
    mem_layout: Layout,
    heap: Vec<*mut GCObject>,
    last_heap_len: usize,
}

impl Lvm {
    pub fn new(program: Program) -> Self {
        Lvm {
            program,
            global_variables: HashMap::new(),
            current_frame: NonNull::dangling(),
            mem_layout: Layout::new::<GCObject>(),
            heap: Vec::with_capacity(0),
            last_heap_len: 100,
        }
    }

    pub fn run(&mut self) {
        let func = self.program.func_list.first().unwrap().clone();
        let stack_size = func.stack_size;
        let mut frame = Frame::new(
            self.new_gc_object(GCObject::new(GCObjectKind::Closuer(Box::new(Closuer {
                base_closuer: None,
                variables: {
                    let mut temp: Vec<LucyData> = Vec::with_capacity(func.local_names.len());
                    for _ in 0..func.local_names.len() {
                        temp.push(LucyData::Null);
                    }
                    temp
                },
                function: func,
            })))),
            self,
            None,
            stack_size,
        );
        self.current_frame = NonNull::new(&mut frame).unwrap();
        frame.run();
    }

    pub fn set_global_variable(&mut self, key: String, value: LucyData) {
        self.global_variables.insert(key, value);
    }

    pub fn get_global_variable(&self, key: &String) -> Option<LucyData> {
        match self.global_variables.get(key) {
            Some(v) => Some(v.clone()),
            None => None,
        }
    }

    pub fn new_gc_object(&mut self, value: GCObject) -> *mut GCObject {
        if self.heap.len() > self.last_heap_len * 2 {
            self.last_heap_len = self.heap.len();
            self.gc();
        }
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
            match (*ptr).kind.clone() {
                GCObjectKind::Table(table) => {
                    for (_, v) in table {
                        if let LucyData::GCObject(ptr) = v {
                            self.gc_object(ptr);
                        }
                    }
                }
                GCObjectKind::Closuer(closuer) => {
                    if let Some(ptr) = closuer.base_closuer {
                        self.gc_object(ptr);
                    }
                    for v in &(*closuer).variables {
                        if let LucyData::GCObject(ptr) = v {
                            self.gc_object(*ptr);
                        }
                    }
                }
                _ => (),
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
                    if let LucyData::GCObject(ptr) = value {
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
