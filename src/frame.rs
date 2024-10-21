use std::{
    cell::RefMut,
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
};

use gc_arena::{lock::RefLock, Collect, Gc, Mutation};
use smol_str::ToSmolStr;

use crate::{
    errors::{Error, ErrorKind},
    objects::{Callback, CallbackReturn, Closure, Function, IntoValue, Table, UpValue, Value},
    utils::Join,
    Context,
};

#[derive(Clone, Copy, Collect)]
#[collect(no_drop)]
pub struct Frames<'gc>(pub(crate) Gc<'gc, RefLock<FramesState<'gc>>>);

impl<'gc> fmt::Debug for Frames<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("Frames")
            .field(&(&self.0 as *const _))
            .finish()
    }
}

impl<'gc> PartialEq for Frames<'gc> {
    fn eq(&self, other: &Frames<'gc>) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }
}

impl<'gc> Eq for Frames<'gc> {}

impl<'gc> Hash for Frames<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect[require_static]]
pub enum FrameMode {
    // No frames are on the thread and there are no available results, the thread can be started.
    Stopped,
    // Frames has an active Lua frame or is waiting for a callback or sequence to finish.
    Normal,
    // Frames is currently inside its own `Thread::step` function.
    Running,
}

impl fmt::Display for FrameMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect[require_static]]
pub enum CatchErrorKind {
    None,
    Try,
    TryOption,
    TryPanic,
}

impl fmt::Display for CatchErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl<'gc> Frames<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Frames<'gc> {
        Frames(Gc::new(
            mc,
            RefLock::new(FramesState {
                frames: Vec::new(),
                return_value: Value::Null,
                error: None,
            }),
        ))
    }

    pub fn mode(self) -> FrameMode {
        if let Ok(state) = self.0.try_borrow() {
            state.mode()
        } else {
            FrameMode::Running
        }
    }

    /// If this thread is `Stopped`, start a new function with the given arguments.
    pub fn start(
        self,
        ctx: Context<'gc>,
        function: Function<'gc>,
        args: Vec<Value<'gc>>,
    ) -> Result<(), Error<'gc>> {
        let mut state = self.check_mode(&ctx, FrameMode::Stopped)?;
        state.call_function(ctx, function, args)?;
        Ok(())
    }

    /// If the thread is in `Normal` mode, either run the Lua VM for a while or step any callback
    /// that we are waiting on.
    pub fn step(self, ctx: Context<'gc>) -> Result<(), Error<'gc>> {
        let mut state = self.check_mode(&ctx, FrameMode::Normal)?;

        match state.frames.last().expect("no frame to step") {
            Frame::Callback(..) => {
                if let Frame::Callback(callback, args) = state.frames.pop().unwrap() {
                    state.frames.push(Frame::Calling);

                    drop(state);
                    let return_value = callback.call(ctx, args);
                    let mut state = self.0.borrow_mut(&ctx);

                    assert!(
                        matches!(state.frames.pop(), Some(Frame::Calling)),
                        "thread state has changed while callback was run"
                    );

                    match return_value {
                        Ok(CallbackReturn::Return(v)) => match state.frames.last_mut() {
                            Some(Frame::Lua(LuciaFrame {
                                stack, catch_error, ..
                            })) => match catch_error {
                                CatchErrorKind::Try => {
                                    let table = Table::new(&ctx);
                                    table.set(ctx, 0, v);
                                    table.set(ctx, 1, Value::Null);
                                    stack.push(table.into_value(ctx));
                                }
                                _ => stack.push(v),
                            },
                            _ => panic!("frame above callback must be lua frame"),
                        },
                        Ok(CallbackReturn::TailCall(f, args)) => {
                            if let Err(e) = state.call_function(ctx, f, args) {
                                state.return_error(ctx, e);
                            }
                        }
                        Err(e) => state.return_error(ctx, e),
                    }
                }
            }
            Frame::Lua { .. } => {
                const VM_GRANULARITY: u32 = 256;
                let mut instructions = VM_GRANULARITY;

                loop {
                    match state.run_vm(ctx, instructions) {
                        Ok(i) => {
                            if let Some(Frame::Lua { .. }) = state.frames.last() {
                                instructions = i;
                                if instructions == 0 {
                                    break;
                                }
                            } else {
                                break;
                            }
                        }
                        Err(e) => {
                            state.return_error(ctx, e);
                            break;
                        }
                    }
                }
            }
            _ => panic!("tried to step invalid frame type"),
        }

        Ok(())
    }

    fn check_mode<'a>(
        &'a self,
        mc: &Mutation<'gc>,
        expected: FrameMode,
    ) -> Result<RefMut<'a, FramesState<'gc>>, Error<'gc>> {
        assert!(expected != FrameMode::Running);
        let state = self.0.try_borrow_mut(mc).map_err(|_| {
            Error::new(ErrorKind::BadFrameMode {
                expected,
                found: FrameMode::Running,
            })
        })?;

        let found = state.mode();
        if found != expected {
            Err(Error::new(ErrorKind::BadFrameMode { expected, found }))
        } else {
            Ok(state)
        }
    }
}

#[derive(Collect)]
#[collect(no_drop)]
pub(crate) struct FramesState<'gc> {
    pub frames: Vec<Frame<'gc>>,
    pub return_value: Value<'gc>,
    pub error: Option<Error<'gc>>,
}

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct LuciaFrame<'gc> {
    pub pc: usize,
    pub closure: Closure<'gc>,
    pub locals: Vec<Value<'gc>>,
    pub upvalues: Vec<UpValue<'gc>>,
    pub stack: Vec<Value<'gc>>,
    pub catch_error: CatchErrorKind,
}

impl<'gc> fmt::Display for LuciaFrame<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "pc: {}", self.pc)?;
        writeln!(
            f,
            "upvalues: [{}]",
            self.closure
                .function
                .upvalue_names
                .iter()
                .zip(self.closure.upvalues.iter())
                .map(|((name, _), value)| format!("{name}: {value}"))
                .join(", ")
        )?;
        writeln!(
            f,
            "locals: [{}]",
            self.closure
                .function
                .local_names
                .iter()
                .zip(self.locals.iter())
                .map(|(name, value)| format!("{name}: {value}"))
                .join(", ")
        )?;
        writeln!(f, "stack: [{}]", self.stack.iter().join(", "))?;
        write!(f, "catch_error: {:?}", self.catch_error)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub enum Frame<'gc> {
    // An running Lua frame.
    Lua(LuciaFrame<'gc>),
    // A callback that has been queued but not called yet. Arguments will be in the external stack.
    Callback(Callback<'gc>, Vec<Value<'gc>>),
    // The thread must be unlocked during external calls to permit cross-thread upvalue handling,
    // but this presents a danger if methods on this thread were to be recursively called at this
    // time. This frame keeps the thread in the `Running` mode during external calls, ensuring the
    // thread cannot be mutated.
    Calling,
}

impl<'gc> LuciaFrame<'gc> {
    pub(crate) fn new(
        ctx: Context<'gc>,
        closure: Closure<'gc>,
        mut args: Vec<Value<'gc>>,
    ) -> Result<Self, Error<'gc>> {
        let function = &closure.function;
        let params_num = function.params.len();
        let mut stack = vec![Value::Null; params_num];
        match args.len().cmp(&params_num) {
            Ordering::Less => {
                return Err(Error::new(ErrorKind::CallArguments {
                    value: Some(closure),
                    required: if function.variadic.is_none() {
                        params_num.into()
                    } else {
                        (params_num, None).into()
                    },
                    given: args.len(),
                }));
            }
            Ordering::Equal => {
                stack[..params_num].copy_from_slice(&args[..]);
                if function.variadic.is_some() {
                    stack.push(Value::Table(Table::new(&ctx)));
                }
            }
            Ordering::Greater => {
                if function.variadic.is_none() {
                    return Err(Error::new(ErrorKind::CallArguments {
                        value: Some(closure),
                        required: params_num.into(),
                        given: args.len(),
                    }));
                } else {
                    let t = args.split_off(params_num);
                    stack[..params_num].copy_from_slice(&args[..]);
                    stack.push(t.into_value(ctx));
                }
            }
        }

        let mut upvalues = Vec::with_capacity(function.upvalue_names.len());
        for (_, base_closure_upvalue_id) in &function.upvalue_names {
            upvalues.push(base_closure_upvalue_id.map_or_else(
                || UpValue::new(&ctx, Value::Null),
                |id| closure.upvalues[id],
            ));
        }

        Ok(LuciaFrame {
            pc: 0,
            closure,
            locals: vec![Value::Null; function.local_names.len()],
            upvalues,
            stack,
            catch_error: CatchErrorKind::None,
        })
    }
}

impl<'gc> FramesState<'gc> {
    fn mode(&self) -> FrameMode {
        match self.frames.last() {
            None => FrameMode::Stopped,
            Some(frame) => match frame {
                Frame::Lua { .. } | Frame::Callback { .. } => FrameMode::Normal,
                Frame::Calling => FrameMode::Running,
            },
        }
    }

    pub(crate) fn call_function(
        &mut self,
        ctx: Context<'gc>,
        function: Function<'gc>,
        args: Vec<Value<'gc>>,
    ) -> Result<(), Error<'gc>> {
        self.frames.push(match function {
            Function::Closure(closure) => Frame::Lua(LuciaFrame::new(ctx, closure, args)?),
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
            Function::Closure(closure) => Frame::Lua(LuciaFrame::new(ctx, closure, args)?),
            Function::Callback(callback) => Frame::Callback(callback, args),
        };
        Ok(())
    }

    // Return to the upper frame with results starting at the given register index.
    pub(crate) fn return_upper(&mut self, ctx: Context<'gc>) {
        match self.frames.pop() {
            Some(Frame::Lua(LuciaFrame { mut stack, .. })) => {
                let return_value = stack.pop().expect("stack error");
                match self.frames.last_mut() {
                    Some(Frame::Lua(LuciaFrame {
                        stack, catch_error, ..
                    })) => match catch_error {
                        CatchErrorKind::Try => {
                            let table = Table::new(&ctx);
                            table.set(ctx, 0, return_value);
                            table.set(ctx, 1, Value::Null);
                            stack.push(table.into_value(ctx));
                        }
                        _ => stack.push(return_value),
                    },
                    None => self.return_value = return_value,
                    _ => panic!("lua frame must be above a lua frame"),
                }
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
                if let Frame::Lua(LuciaFrame {
                    catch_error, stack, ..
                }) = f
                {
                    match catch_error {
                        CatchErrorKind::None => (),
                        CatchErrorKind::Try => {
                            let table = Table::new(&ctx);
                            table.set(ctx, 0, Value::Null);
                            table.set(
                                ctx,
                                1,
                                if let ErrorKind::LuciaError(v) = e.kind {
                                    v
                                } else {
                                    e.to_smolstr().into_value(ctx)
                                },
                            );
                            stack.push(table.into_value(ctx));
                            self.frames.truncate(c + 1);
                            return;
                        }
                        CatchErrorKind::TryOption => {
                            stack.push(Value::Null);
                            self.frames.truncate(c + 1);
                            return;
                        }
                        CatchErrorKind::TryPanic => {
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
