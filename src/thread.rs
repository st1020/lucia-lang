#![allow(clippy::multiple_inherent_impl)]

use std::rc::Rc;

use compact_str::ToCompactString;
use derive_more::Display;

use crate::{
    Context,
    errors::{Error, ErrorKind},
    frame::{CallStatusKind, Frame, LuciaFrame},
    fuel::Fuel,
    objects::{CallbackReturn, Function, TableInner, Value},
};

/// The current state of a [`Thread`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display)]
#[display("{self:?}")]
pub enum ThreadMode {
    /// No frames are on the thread and there are no available results, the thread can be started.
    Stopped,
    /// Thread has an active Lucia or Callback frame.
    Normal,
    /// A callback that this thread owns is currently being run.
    Running,
}

pub type Thread = Rc<ThreadState>;

impl ThreadState {
    const VM_GRANULARITY: u32 = 64;

    const FUEL_PER_CALLBACK: i32 = 8;
    const FUEL_PER_STEP: i32 = 4;

    pub fn new() -> Self {
        Self {
            frames: Vec::new(),
            return_value: Value::Null,
            error: None,
        }
    }

    /// If this thread is `Stopped`, start a new function with the given arguments.
    pub fn start(&mut self, function: Function, args: Vec<Value>) -> Result<(), Error> {
        self.call_function(function, args, CallStatusKind::Normal)?;
        Ok(())
    }

    /// If the thread is in `Normal` mode, either run the Lucia VM for a while or step any callback
    /// that we are waiting on.
    pub fn step(&mut self, ctx: &mut Context, fuel: &mut Fuel) -> Result<bool, Error> {
        Ok(loop {
            match self.mode() {
                ThreadMode::Stopped => break true,
                ThreadMode::Normal => (),
                ThreadMode::Running => panic!("thread is already running"),
            }

            match self.frames.pop().expect("no frame to step") {
                Frame::Callback { callback, args } => {
                    fuel.consume(Self::FUEL_PER_CALLBACK);
                    match callback.call(ctx, &args) {
                        Ok(CallbackReturn::Return(v)) => self.return_to(v),
                        Ok(CallbackReturn::TailCall(function, args)) => {
                            if let Err(e) =
                                self.call_function(function, args, CallStatusKind::Normal)
                            {
                                self.return_error(e);
                            }
                        }
                        Err(e) => self.return_error(e),
                    }
                }
                frame @ Frame::Lucia { .. } => {
                    self.frames.push(frame);
                    match self.run_vm(ctx, Self::VM_GRANULARITY) {
                        Ok(instructions_run) => {
                            fuel.consume(instructions_run.try_into().unwrap_or(i32::MAX));
                        }
                        Err(e) => self.return_error(e),
                    }
                }
            }

            fuel.consume(Self::FUEL_PER_STEP);

            if !fuel.should_continue() {
                break false;
            }
        })
    }
}

impl Default for ThreadState {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct ThreadState {
    pub frames: Vec<Frame>,
    pub return_value: Value,
    pub error: Option<Error>,
}

impl ThreadState {
    fn mode(&self) -> ThreadMode {
        match self.frames.last() {
            None => ThreadMode::Stopped,
            Some(_) => ThreadMode::Normal,
        }
    }

    pub(crate) fn call_function(
        &mut self,
        function: Function,
        args: Vec<Value>,
        call_status: CallStatusKind,
    ) -> Result<(), Error> {
        if let Some(Frame::Lucia(frame)) = self.frames.last_mut() {
            frame.call_status = call_status;
        }
        self.frames.push(match function {
            Function::Closure(closure) => Frame::Lucia(LuciaFrame::new(closure, args)?),
            Function::Callback(callback) => Frame::Callback { callback, args },
        });
        Ok(())
    }

    pub(crate) fn tail_call(&mut self, function: Function, args: Vec<Value>) -> Result<(), Error> {
        *self.frames.last_mut().expect("top frame is not lua frame") = match function {
            Function::Closure(closure) => Frame::Lucia(LuciaFrame::new(closure, args)?),
            Function::Callback(callback) => Frame::Callback { callback, args },
        };
        Ok(())
    }

    pub(crate) fn return_to(&mut self, return_value: Value) {
        match self.frames.last_mut() {
            Some(Frame::Lucia(LuciaFrame {
                stack, call_status, ..
            })) => match call_status {
                CallStatusKind::Try => {
                    let mut table = TableInner::new();
                    table.set(0_i64, return_value);
                    table.set(1_i64, Value::Null);
                    stack.push(table.into());
                }
                CallStatusKind::Normal | CallStatusKind::TryOption | CallStatusKind::TryPanic => {
                    stack.push(return_value);
                }
            },
            None => self.return_value = return_value,
            _ => panic!("lua frame must be above a lua frame"),
        }
    }

    pub(crate) fn return_upper(&mut self, return_value: Value) {
        self.frames.pop().expect("top frame is not lua frame");
        self.return_to(return_value);
    }

    pub(crate) fn return_error(&mut self, mut e: Error) {
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
                        CallStatusKind::Normal => (),
                        CallStatusKind::Try => {
                            let mut table = TableInner::new();
                            table.set(0_i64, Value::Null);
                            table.set(
                                1_i64,
                                if let ErrorKind::LuciaError(v) = e.kind {
                                    v
                                } else {
                                    e.to_compact_string().into()
                                },
                            );
                            stack.push(table.into());
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

    pub(crate) fn traceback(&self) -> Vec<Frame> {
        self.frames.clone()
    }
}
