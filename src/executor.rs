#![allow(clippy::multiple_inherent_impl)]

use std::rc::Rc;

use crate::{
    Context,
    errors::Error,
    frame::{Frame, LuciaFrame},
    fuel::Fuel,
    objects::{CallbackReturn, ContinuationInner, Effect, EffectInner, Function, Value},
};

pub type Executor = Rc<ExecutorInner>;

#[derive(Debug, Clone)]
pub struct ExecutorInner {
    pub frames: Vec<Frame>,
}

impl ExecutorInner {
    const VM_GRANULARITY: u32 = 64;

    const FUEL_PER_CALLBACK: i32 = 8;
    const FUEL_PER_STEP: i32 = 4;

    pub fn new() -> Self {
        Self { frames: Vec::new() }
    }

    /// If this continuation is `Stopped`, start a new function with the given arguments.
    pub fn start(&mut self, function: Function, args: Vec<Value>) {
        self.call_function(function, args);
    }

    /// If the continuation is in `Normal` mode, either run the Lucia VM for a while or step any callback
    /// that we are waiting on.
    pub fn step(&mut self, ctx: &mut Context, fuel: &mut Fuel) -> bool {
        loop {
            if self.frames.is_empty() {
                break true;
            }

            match self.frames.pop().expect("no frame to step") {
                Frame::Callback { callback, args } => {
                    fuel.consume(Self::FUEL_PER_CALLBACK);
                    match callback.call(ctx, &args) {
                        Ok(CallbackReturn::Return(v)) => {
                            self.return_current(v);
                        }
                        Ok(CallbackReturn::TailCall(function, args)) => {
                            self.call_function(function, args);
                        }
                        Ok(CallbackReturn::TailEffect(effect, args)) => {
                            self.perform_effect(effect, args);
                        }
                        Err(e) => {
                            self.return_error(e);
                        }
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
                frame @ (Frame::Result { .. } | Frame::Error { .. } | Frame::Effect { .. }) => {
                    self.frames.push(frame);
                    break true;
                }
            }

            fuel.consume(Self::FUEL_PER_STEP);

            if !fuel.should_continue() {
                break false;
            }
        }
    }

    pub(crate) fn call_function(&mut self, function: Function, args: Vec<Value>) {
        match function {
            Function::Closure(closure) => match LuciaFrame::new(closure, &args) {
                Ok(frame) => self.frames.push(Frame::Lucia(frame)),
                Err(error) => self.return_error(error),
            },
            Function::Callback(callback) => self.frames.push(Frame::Callback { callback, args }),
            Function::Continuation(continuation) => {
                let mut frames = Rc::unwrap_or_clone(continuation).frames;
                if args.len() != 1 {
                    return self.return_error(Error::CallArguments {
                        required: 1.into(),
                        given: args.len(),
                    });
                }
                if let Some(Frame::Lucia(LuciaFrame { stack, .. })) = frames.last_mut() {
                    stack.push(args[0].clone());
                } else {
                    panic!("continuation top frame is not lucia frame");
                }
                self.frames.append(&mut frames);
            }
        }
    }

    pub(crate) fn tail_call(&mut self, function: Function, args: Vec<Value>) {
        self.frames.pop().expect("top frame is not lua frame");
        self.call_function(function, args);
    }

    pub(crate) fn return_current(&mut self, value: Value) {
        match self.frames.last_mut() {
            Some(Frame::Lucia(LuciaFrame { stack, .. })) => stack.push(value),
            None => self.frames.push(Frame::Result { value }),
            _ => panic!("lua frame must be above a lua frame"),
        }
    }

    pub(crate) fn return_upper(&mut self, value: Value) {
        self.frames.pop().expect("top frame is not lua frame");
        self.return_current(value);
    }

    #[inline]
    fn perform_effect_with(&mut self, effect: &Effect, args: &[Value]) -> bool {
        if let Some(i) = self.frames.iter().rposition(|frame| {
            if let Frame::Lucia(LuciaFrame {
                effect_handlers, ..
            }) = frame
                && effect_handlers.contains_key(effect)
            {
                true
            } else {
                false
            }
        }) {
            let continuation = ContinuationInner::new(self.frames.split_off(i + 1));
            if let Frame::Lucia(LuciaFrame {
                pc,
                stack,
                effect_handlers,
                ..
            }) = &mut self.frames[i]
                && let Some(effect_handler_info) = effect_handlers.get(effect).cloned()
            {
                stack.truncate(effect_handler_info.stack_size);
                stack.push(continuation.into());
                if let Err(e) = effect.params_info().parse_args_to_stack(stack, args) {
                    self.return_error(e);
                    return true;
                }
                *pc = effect_handler_info.jump_target;
                self.frames.truncate(i + 1);
                return true;
            }
        }
        false
    }

    pub(crate) fn perform_effect(&mut self, effect: Effect, args: Vec<Value>) {
        if !self.perform_effect_with(&effect, &args) {
            self.frames.push(Frame::Effect { effect, args });
        }
    }

    pub(crate) fn return_error(&mut self, error: Error) {
        if !self.perform_effect_with(
            #[expect(clippy::wildcard_enum_match_arm)]
            &match error {
                Error::LuciaPanic(_) => EffectInner::Panic,
                Error::LuciaAssert(_) => EffectInner::Assert,
                _ => EffectInner::Error,
            }
            .into(),
            &[],
        ) {
            self.frames.push(Frame::Error { error });
        }
    }
}

impl Default for ExecutorInner {
    fn default() -> Self {
        Self::new()
    }
}
