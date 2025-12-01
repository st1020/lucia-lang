use std::ops;

use gc_arena::{Arena, Collect, Mutation, Rootable};

use crate::{
    compiler::{compile, error::CompilerError},
    errors::{Error, ExternError},
    fuel::Fuel,
    libs,
    objects::{
        Closure, Registry, RuntimeCode, StashedRuntimeCode, StashedValue, StrInterner, Table,
    },
    thread::Thread,
};

#[derive(Clone, Collect)]
#[collect(no_drop)]
pub struct State<'gc> {
    /// Global variables.
    pub globals: Table<'gc>,
    /// Builtin variables.
    pub builtins: Table<'gc>,
    /// Importable libraries.
    pub libs: Table<'gc>,
    /// Registry of static values.
    pub registry: Registry<'gc>,
    /// All stack frames.
    pub thread: Thread<'gc>,
}

impl<'gc> State<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> State<'gc> {
        Self {
            globals: Table::new(mc),
            builtins: Table::new(mc),
            libs: Table::new(mc),
            registry: Registry::new(mc),
            thread: Thread::new(mc),
        }
    }

    pub fn ctx(&'gc self, mutation: &'gc Mutation<'gc>) -> Context<'gc> {
        Context {
            mutation,
            state: self,
        }
    }
}

#[derive(Clone, Copy)]
pub struct Context<'gc> {
    pub mutation: &'gc Mutation<'gc>,
    pub state: &'gc State<'gc>,
}

impl<'gc> ops::Deref for Context<'gc> {
    type Target = Mutation<'gc>;

    fn deref(&self) -> &Self::Target {
        self.mutation
    }
}

pub struct Lucia {
    arena: Arena<Rootable![State<'_>]>,
}

impl Lucia {
    pub fn empty() -> Self {
        #[expect(clippy::redundant_closure)]
        let arena = Arena::<Rootable![State<'_>]>::new(|mc| State::new(mc));
        Lucia { arena }
    }

    pub fn new() -> Self {
        let mut lucia = Lucia::empty();
        lucia.load_libs();
        lucia
    }

    pub fn load_libs(&mut self) {
        self.enter(|ctx| {
            libs::load_builtin(ctx);
            ctx.state.libs.set(ctx, "std::io", libs::io_lib(ctx));
            ctx.state
                .libs
                .set(ctx, "std::string", libs::string_lib(ctx));
            ctx.state.libs.set(ctx, "std::table", libs::table_lib(ctx));
        });
    }

    pub fn gc_collect(&mut self) {
        self.arena.collect_all();
    }

    pub fn enter<F, T>(&mut self, f: F) -> T
    where
        F: for<'gc> FnOnce(Context<'gc>) -> T,
    {
        const COLLECTOR_GRANULARITY: f64 = 1024.0;

        let r = self.arena.mutate(move |mc, state| f(state.ctx(mc)));
        if self.arena.metrics().allocation_debt() > COLLECTOR_GRANULARITY {
            self.arena.collect_debt();
        }
        r
    }

    pub fn try_enter<F, R>(&mut self, f: F) -> Result<R, ExternError>
    where
        F: for<'gc> FnOnce(Context<'gc>) -> Result<R, Error<'gc>>,
    {
        self.enter(move |ctx| f(ctx).map_err(Error::into_extern))
    }

    pub fn finish(&mut self) -> Result<(), ExternError> {
        const FUEL_PER_GC: i32 = 4096;

        loop {
            let mut fuel = Fuel::with(FUEL_PER_GC);
            if self.enter(|ctx| ctx.state.thread.step(ctx, &mut fuel))? {
                break;
            }
        }

        Ok(())
    }

    pub fn compile(&mut self, input: &str) -> Result<StashedRuntimeCode, Vec<CompilerError>> {
        self.enter(|ctx| {
            let interner = StrInterner::new(ctx);
            let code = compile(interner, input)?;
            let runtime_code = RuntimeCode::new(&ctx, code);
            Ok(ctx.state.registry.stash(&ctx, runtime_code))
        })
    }

    pub fn execute(&mut self, code: &StashedRuntimeCode) -> Result<StashedValue, ExternError> {
        self.enter(|ctx| {
            let runtime_code = ctx.state.registry.fetch(code);
            let closure = Closure::new(&ctx, runtime_code, None);
            ctx.state
                .thread
                .start(ctx, closure.into(), Vec::new())
                .unwrap();
        });
        self.finish()?;
        self.enter(|ctx| {
            if let Some(e) = &ctx.state.thread.into_inner().borrow().error {
                Err(e.kind.clone().into())
            } else {
                Ok(ctx
                    .state
                    .registry
                    .stash(&ctx, ctx.state.thread.into_inner().borrow().return_value))
            }
        })
    }
}

impl Default for Lucia {
    fn default() -> Self {
        Lucia::new()
    }
}
