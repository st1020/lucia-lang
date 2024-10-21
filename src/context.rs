use std::ops;

use gc_arena::{Arena, Collect, Mutation, Rootable};

use crate::{
    compiler::code::Code,
    frame::{FrameMode, Frames},
    libs,
    objects::{Closure, GcError, Registry, RuntimeCode, StaticError, StaticValue, Table},
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
    pub frames: Frames<'gc>,
}

impl<'gc> State<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> State<'gc> {
        Self {
            globals: Table::new(mc),
            builtins: Table::new(mc),
            libs: Table::new(mc),
            registry: Registry::new(mc),
            frames: Frames::new(mc),
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

const COLLECTOR_GRANULARITY: f64 = 1024.0;

impl Lucia {
    pub fn new() -> Lucia {
        #[allow(clippy::redundant_closure)]
        let arena = Arena::<Rootable![State<'_>]>::new(|mc| State::new(mc));
        let mut lucia = Lucia { arena };
        lucia.load_libs();
        lucia
    }

    pub fn load_libs(&mut self) {
        self.run(|ctx| {
            libs::load_builtin(ctx);
            ctx.state.libs.set(ctx, "std::io", libs::io_lib(ctx));
            ctx.state
                .libs
                .set(ctx, "std::string", libs::string_lib(ctx));
            ctx.state.libs.set(ctx, "std::table", libs::table_lib(ctx));
        })
    }

    pub fn gc_collect(&mut self) {
        self.arena.collect_all();
    }

    pub fn run<F, T>(&mut self, f: F) -> T
    where
        F: for<'gc> FnOnce(Context<'gc>) -> T,
    {
        let r = self.arena.mutate(move |mc, state| f(state.ctx(mc)));
        if self.arena.metrics().allocation_debt() > COLLECTOR_GRANULARITY {
            self.arena.collect_debt();
        }
        r
    }

    pub fn run_frame(&mut self) {
        loop {
            if self.run(|ctx| match ctx.state.frames.mode() {
                FrameMode::Normal => {
                    ctx.state.frames.step(ctx).unwrap();
                    false
                }
                _ => true,
            }) {
                break;
            }
        }
    }

    pub fn run_code(&mut self, code: Code) -> Result<StaticValue, StaticError> {
        self.run(|ctx| {
            let gc_code = RuntimeCode::new(&ctx, code);
            let closure = Closure::new(&ctx, gc_code, None);
            ctx.state
                .frames
                .start(ctx, closure.into(), Vec::new())
                .unwrap();
        });
        self.run_frame();
        self.run(|ctx| {
            if let Some(e) = &ctx.state.frames.0.borrow().error {
                Err(ctx
                    .state
                    .registry
                    .stash(&ctx, GcError::new(&ctx, e.clone())))
            } else {
                Ok(ctx
                    .state
                    .registry
                    .stash(&ctx, ctx.state.frames.0.borrow().return_value))
            }
        })
    }
}

impl Default for Lucia {
    fn default() -> Self {
        Lucia::new()
    }
}
