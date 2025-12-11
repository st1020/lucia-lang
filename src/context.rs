use std::{panic, rc::Rc};

use crate::{
    compiler::{code::Code, compile, error::CompilerError, interning::CompactInterner},
    errors::Error,
    executor::ExecutorInner,
    frame::Frame,
    fuel::Fuel,
    libs,
    objects::{Closure, ClosureInner, Str, TableInner, Value},
};

#[derive(Debug, Clone)]
pub struct Context {
    /// Global variables.
    pub globals: TableInner,
    /// Builtin variables.
    pub builtins: TableInner,
    /// Importable libraries.
    pub libs: TableInner,
}

impl Context {
    pub fn empty() -> Self {
        Self {
            globals: TableInner::new(),
            builtins: TableInner::new(),
            libs: TableInner::new(),
        }
    }

    pub fn new() -> Self {
        let mut ctx = Self::empty();
        ctx.load_libs();
        ctx
    }

    pub fn load_libs(&mut self) {
        libs::load_builtin(self);
        self.libs.set("std::io", libs::io_lib());
        self.libs.set("std::string", libs::string_lib());
        self.libs.set("std::table", libs::table_lib());
    }

    pub fn compile(&mut self, input: &str) -> Result<Code<Str>, Vec<CompilerError>> {
        let interner = CompactInterner::default();
        compile(interner, input)
    }

    pub fn execute(&mut self, code: Code<Str>) -> Result<Value, Error> {
        const FUEL_PER_GC: i32 = 4096;

        let closure = Closure::new(ClosureInner::new(Rc::new(code), None));
        let mut executor = ExecutorInner::new();
        executor.start(closure.into(), Vec::new());

        loop {
            let mut fuel = Fuel::with(FUEL_PER_GC);
            if executor.step(self, &mut fuel) {
                break;
            }
        }

        match executor.frames.pop() {
            Some(Frame::Result { value }) => Ok(value),
            Some(Frame::Error { error }) => Err(error),
            Some(Frame::Effect { effect, args }) => Err(Error::UnhandledEffect { effect, args }),
            _ => panic!("executor finished without result"),
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}
