use core::fmt;
use std::cell::RefMut;

use compact_str::ToCompactString;
use gc_arena::{lock::RefLock, Collect, Gc, Mutation};

use crate::{
    errors::{Error, ErrorKind, LuciaError, RuntimeError},
    frame::{CallStatusKind, Frame, LuciaFrame},
    fuel::Fuel,
    objects::{define_object, CallbackReturn, Function, IntoValue, Table, Value},
    Context,
};

/// The current state of a [`Thread`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect(require_static)]
pub enum ThreadMode {
    /// No frames are on the thread and there are no available results, the thread can be started.
    Stopped,
    /// Thread has an active Lucia or Callback frame.
    Normal,
    /// A callback that this thread owns is currently being run.
    Running,
}

impl fmt::Display for ThreadMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt::Display::fmt(&self, f)
    }
}

pub type ThreadInner<'gc> = RefLock<ThreadState<'gc>>;

define_object!(Thread, ThreadInner<'gc>, ptr, "thread");

impl<'gc> Thread<'gc> {
    const VM_GRANULARITY: u32 = 64;

    const FUEL_PER_CALLBACK: i32 = 8;
    const FUEL_PER_STEP: i32 = 4;

    pub fn new(mc: &Mutation<'gc>) -> Self {
        Thread(Gc::new(
            mc,
            RefLock::new(ThreadState {
                frames: Vec::new(),
                return_value: Value::Null,
                error: None,
            }),
        ))
    }

    pub fn mode(self) -> ThreadMode {
        match self.0.try_borrow() {
            Ok(state) => state.mode(),
            Err(_) => ThreadMode::Running,
        }
    }

    /// If this thread is `Stopped`, start a new function with the given arguments.
    pub fn start(
        self,
        ctx: Context<'gc>,
        function: Function<'gc>,
        args: Vec<Value<'gc>>,
    ) -> Result<(), Error<'gc>> {
        let mut state = self
            .check_mode(&ctx, ThreadMode::Stopped)
            .map_err(Error::new)?;
        state.call_function(ctx, function, args, CallStatusKind::Normal)?;
        Ok(())
    }

    /// If the thread is in `Normal` mode, either run the Lucia VM for a while or step any callback
    /// that we are waiting on.
    pub fn step(self, ctx: Context<'gc>, fuel: &mut Fuel) -> Result<bool, RuntimeError> {
        Ok(loop {
            match self.mode() {
                ThreadMode::Stopped => break true,
                ThreadMode::Normal => (),
                ThreadMode::Running => panic!("thread is already running"),
            }

            let mut state: RefMut<'_, ThreadState<'gc>> = self.into_inner().borrow_mut(&ctx);
            let state = &mut *state;
            match state.frames.pop().expect("no frame to step") {
                Frame::Callback(callback, args) => {
                    fuel.consume(Self::FUEL_PER_CALLBACK);
                    match callback.call(ctx, args) {
                        Ok(CallbackReturn::Return(v)) => state.return_to(ctx, v),
                        Ok(CallbackReturn::TailCall(f, args)) => {
                            if let Err(e) =
                                state.call_function(ctx, f, args, CallStatusKind::Normal)
                            {
                                state.return_error(ctx, e);
                            }
                        }
                        Err(e) => state.return_error(ctx, e),
                    }
                }
                frame @ Frame::Lucia { .. } => {
                    state.frames.push(frame);
                    match state.run_vm(ctx, Self::VM_GRANULARITY) {
                        Ok(instructions_run) => fuel.consume(instructions_run.try_into().unwrap()),
                        Err(e) => state.return_error(ctx, e),
                    }
                }
            }

            fuel.consume(Self::FUEL_PER_STEP);

            if !fuel.should_continue() {
                break false;
            }
        })
    }

    fn check_mode<'a>(
        &'a self,
        mc: &Mutation<'gc>,
        expected: ThreadMode,
    ) -> Result<RefMut<'a, ThreadState<'gc>>, RuntimeError> {
        assert!(expected != ThreadMode::Running);
        let state = self
            .0
            .try_borrow_mut(mc)
            .map_err(|_| RuntimeError::BadThreadMode {
                expected,
                found: ThreadMode::Running,
            })?;

        let found = state.mode();
        if found != expected {
            Err(RuntimeError::BadThreadMode { expected, found })
        } else {
            Ok(state)
        }
    }
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct ThreadState<'gc> {
    pub frames: Vec<Frame<'gc>>,
    pub return_value: Value<'gc>,
    pub error: Option<Error<'gc>>,
}

impl<'gc> ThreadState<'gc> {
    fn mode(&self) -> ThreadMode {
        match self.frames.last() {
            None => ThreadMode::Stopped,
            Some(_) => ThreadMode::Normal,
        }
    }

    pub(crate) fn call_function(
        &mut self,
        ctx: Context<'gc>,
        function: Function<'gc>,
        args: Vec<Value<'gc>>,
        call_status: CallStatusKind,
    ) -> Result<(), Error<'gc>> {
        if let Some(Frame::Lucia(frame)) = self.frames.last_mut() {
            frame.call_status = call_status;
        }
        self.frames.push(match function {
            Function::Closure(closure) => Frame::Lucia(LuciaFrame::new(ctx, closure, args)?),
            Function::Callback(callback) => Frame::Callback(callback, args),
        });
        Ok(())
    }

    pub(crate) fn tail_call(
        &mut self,
        ctx: Context<'gc>,
        function: Function<'gc>,
        args: Vec<Value<'gc>>,
    ) -> Result<(), Error<'gc>> {
        *self.frames.last_mut().expect("top frame is not lua frame") = match function {
            Function::Closure(closure) => Frame::Lucia(LuciaFrame::new(ctx, closure, args)?),
            Function::Callback(callback) => Frame::Callback(callback, args),
        };
        Ok(())
    }

    pub(crate) fn return_to(&mut self, ctx: Context<'gc>, return_value: Value<'gc>) {
        match self.frames.last_mut() {
            Some(Frame::Lucia(LuciaFrame {
                stack, call_status, ..
            })) => match call_status {
                CallStatusKind::Try => {
                    let table = Table::new(&ctx);
                    table.set(ctx, 0, return_value);
                    table.set(ctx, 1, Value::Null);
                    stack.push(table.into_value(ctx));
                }
                CallStatusKind::IgnoreReturn => (),
                _ => stack.push(return_value),
            },
            None => self.return_value = return_value,
            _ => panic!("lua frame must be above a lua frame"),
        }
    }

    pub(crate) fn return_upper(&mut self, ctx: Context<'gc>) {
        match self.frames.pop() {
            Some(Frame::Lucia(LuciaFrame { mut stack, .. })) => {
                let return_value = stack.pop().expect("stack error");
                self.return_to(ctx, return_value);
            }
            _ => panic!("top frame is not lua frame"),
        }
    }

    pub(crate) fn return_error(&mut self, ctx: Context<'gc>, mut e: Error<'gc>) {
        if e.traceback.is_none() {
            e.traceback = Some(self.traceback());
        }
        if e.kind.recoverable() {
            for (c, f) in self.frames.iter_mut().enumerate().rev() {
                if let Frame::Lucia(LuciaFrame {
                    call_status, stack, ..
                }) = f
                {
                    match call_status {
                        CallStatusKind::Normal | CallStatusKind::IgnoreReturn => (),
                        CallStatusKind::Try => {
                            let table = Table::new(&ctx);
                            table.set(ctx, 0, Value::Null);
                            table.set(
                                ctx,
                                1,
                                if let ErrorKind::LuciaError(LuciaError::Error(v)) = e.kind {
                                    v
                                } else {
                                    e.to_compact_string().into_value(ctx)
                                },
                            );
                            stack.push(table.into_value(ctx));
                            self.frames.truncate(c + 1);
                            return;
                        }
                        CallStatusKind::TryOption => {
                            stack.push(Value::Null);
                            self.frames.truncate(c + 1);
                            return;
                        }
                        CallStatusKind::TryPanic => {
                            self.frames.clear();
                            self.error = Some(e);
                            return;
                        }
                    }
                }
            }
        }
        self.frames.clear();
        self.error = Some(e);
    }

    pub(crate) fn traceback(&self) -> Vec<Frame<'gc>> {
        self.frames.clone()
    }
}
