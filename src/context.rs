use std::{panic, rc::Rc};

use crate::{
    compiler::{code::Code, compile, error::CompilerError},
    errors::Error,
    executor::{Executor, ExecutorResult},
    fuel::Fuel,
    libs,
    objects::{Closure, RcStr, StrInterner, Table, Value},
};

#[derive(Debug, Clone)]
pub struct Context {
    /// Global variables.
    pub globals: Table,
    /// Builtin variables.
    pub builtins: Table,
    /// Importable libraries.
    pub libs: Table,
}

impl Context {
    pub fn empty() -> Self {
        Self {
            globals: Table::new(),
            builtins: Table::new(),
            libs: Table::new(),
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

    pub fn compile(&mut self, input: &str) -> Result<Code<RcStr>, Vec<CompilerError>> {
        let interner = StrInterner::default();
        compile(interner, input)
    }

    pub fn execute(&mut self, code: Code<RcStr>) -> Result<Value, Error> {
        const FUEL_PER_GC: i32 = 4096;

        let closure = Rc::new(Closure::new(Rc::new(code)));
        let mut executor = Executor::new();
        executor.start(closure.into(), Vec::new());

        loop {
            let mut fuel = Fuel::with(FUEL_PER_GC);
            if executor.step(self, &mut fuel) {
                break;
            }
        }

        match executor.result {
            Some(ExecutorResult::Value { value }) => Ok(value),
            Some(ExecutorResult::Error { error }) => Err(error),
            Some(ExecutorResult::Effect { effect, args }) => {
                Err(Error::UnhandledEffect { effect, args })
            }
            _ => panic!("executor finished without result"),
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}
