#![allow(clippy::multiple_inherent_impl)]

use std::{cmp::Ordering, rc::Rc};

use ordermap::{OrderMap, OrderSet};
use oxc_index::index_vec;

use crate::{
    Context,
    compiler::{code::CodeParamsInfo, index::CodeId},
    errors::Error,
    frame::Frame,
    fuel::Fuel,
    objects::{
        BuiltinEffect, CallbackReturn, Continuation, Effect, Function, RcEffect, Table, Value,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExecutorResult {
    /// The result value of the executor.
    Value { value: Value },
    /// An error that has occurred.
    Error { error: Error },
    /// An effect that has been performed but not handled.
    Effect { effect: RcEffect, args: Vec<Value> },
}

#[derive(Debug, Clone)]
pub struct Executor {
    pub frames: Vec<Frame>,
    pub result: Option<ExecutorResult>,
}

impl Executor {
    const VM_GRANULARITY: u32 = 64;

    const FUEL_PER_CALLBACK: i32 = 8;
    const FUEL_PER_STEP: i32 = 4;

    const MAX_STACK_SIZE: usize = 256;

    pub fn new() -> Self {
        Self {
            frames: Vec::new(),
            result: None,
        }
    }

    /// Start a new function with the given arguments.
    pub fn start(&mut self, function: Function, args: Vec<Value>) {
        self.call_function(function, args);
    }

    /// Run the Lucia VM for a while or step any callback that we are waiting on.
    pub fn step(&mut self, ctx: &mut Context, fuel: &mut Fuel) -> bool {
        loop {
            if self.frames.is_empty() || self.result.is_some() {
                break true;
            }

            match self.frames.last_mut().expect("no frame to step") {
                Frame::Lucia { .. } => match self.run_vm(ctx, Self::VM_GRANULARITY) {
                    Ok(instructions_run) => {
                        fuel.consume(instructions_run.try_into().unwrap_or(i32::MAX));
                    }
                    Err(error) => self.throw_error(error),
                },
                Frame::Callback {
                    callback,
                    stack,
                    effect_handlers,
                } => {
                    fuel.consume(Self::FUEL_PER_CALLBACK);
                    match callback.call(ctx, stack) {
                        Ok(CallbackReturn::Call {
                            function,
                            args,
                            effect_handlers: new_effect_handlers,
                        }) => {
                            *effect_handlers = new_effect_handlers;
                            self.call_function(function, args);
                        }
                        Ok(CallbackReturn::Perform { effect, args }) => {
                            self.perform_effect(effect, args);
                        }
                        Ok(CallbackReturn::TailCall { function, args }) => {
                            self.tail_call(function, args);
                        }
                        Ok(CallbackReturn::TailPerform { effect, args }) => {
                            self.tail_perform(effect, args);
                        }
                        Ok(CallbackReturn::ReturnValue { value }) => {
                            self.return_value(value);
                        }
                        Err(error) => {
                            self.throw_error(error);
                        }
                    }
                }
            }

            fuel.consume(Self::FUEL_PER_STEP);

            if !fuel.should_continue() {
                break false;
            }
        }
    }

    pub(crate) fn return_value(&mut self, value: Value) {
        self.frames.pop().expect("no frame to pop for return value");
        match self.frames.last_mut() {
            Some(Frame::Lucia {
                stack,
                effect_handlers,
                ..
            }) => {
                effect_handlers.clear();
                stack.push(value);
            }
            Some(Frame::Callback {
                stack,
                effect_handlers,
                ..
            }) => {
                effect_handlers.clear();
                stack.clear();
                stack.push(value);
            }
            None => self.result = Some(ExecutorResult::Value { value }),
        }
    }

    pub(crate) fn call_function(&mut self, function: Function, args: Vec<Value>) {
        match function {
            Function::Closure(closure) => {
                let params_info = closure.code.params.info();
                if let Err(error) = params_info.check_args(&args) {
                    return self.throw_error(error);
                }
                let mut stack =
                    Vec::with_capacity(closure.code.stack_size.min(Self::MAX_STACK_SIZE));
                params_info.parse_args_to_stack(&mut stack, &args);
                self.frames.push(Frame::Lucia {
                    pc: CodeId::new(0),
                    locals: index_vec![Value::Null; closure.code.local_names.len()],
                    stack,
                    closure,
                    effect_handlers: OrderMap::new(),
                });
            }
            Function::Callback(callback) => self.frames.push(Frame::Callback {
                callback: Rc::unwrap_or_clone(callback),
                stack: args,
                effect_handlers: OrderSet::new(),
            }),
            Function::Continuation(continuation) => {
                if args.len() != 1 {
                    return self.throw_error(Error::CallArguments {
                        required: 1.into(),
                        given: args.len(),
                    });
                }
                let resume_value = args[0].clone();
                let mut frames = Rc::unwrap_or_clone(continuation).frames;
                match frames.last_mut() {
                    Some(Frame::Lucia { stack, .. }) => stack.push(resume_value),
                    Some(Frame::Callback { stack, .. }) => {
                        stack.clear();
                        stack.push(resume_value);
                    }
                    None => unreachable!("continuation has no frames"),
                }
                self.frames.append(&mut frames);
            }
        }
    }

    pub(crate) fn tail_call(&mut self, function: Function, args: Vec<Value>) {
        self.frames.pop().expect("no frame to pop for tail call");
        self.call_function(function, args);
    }

    #[inline]
    fn perform_effect_with(&mut self, effect: RcEffect, args: &[Value]) -> bool {
        let params_info = effect.params_info();
        if let Err(error) = params_info.check_args(args) {
            self.throw_error(error);
            return true;
        }
        if let Some(i) = self
            .frames
            .iter()
            .take(self.frames.len().saturating_sub(1))
            .rposition(|frame| match frame {
                Frame::Lucia {
                    effect_handlers, ..
                } => effect_handlers.contains_key(&effect),
                Frame::Callback {
                    effect_handlers, ..
                } => effect_handlers.contains(&effect),
            })
        {
            let continuation = Continuation::new(self.frames.split_off(i + 1));
            match &mut self.frames[i] {
                Frame::Lucia {
                    pc,
                    stack,
                    effect_handlers,
                    ..
                } => {
                    let effect_handler_info = effect_handlers.get(&effect).cloned().unwrap();
                    effect_handlers.clear();
                    stack.push(continuation.into());
                    params_info.parse_args_to_stack(stack, args);
                    stack.push(effect.into());
                    *pc = effect_handler_info.jump_target;
                }
                Frame::Callback {
                    stack,
                    effect_handlers,
                    ..
                } => {
                    effect_handlers.clear();
                    stack.clear();
                    stack.push(effect.into());
                    stack.push(continuation.into());
                    params_info.parse_args_to_stack(stack, args);
                }
            }
            self.frames.truncate(i + 1);
            return true;
        }
        false
    }

    pub(crate) fn perform_effect(&mut self, effect: RcEffect, args: Vec<Value>) {
        if !self.perform_effect_with(Rc::clone(&effect), &args) {
            self.result = Some(ExecutorResult::Effect { effect, args });
        }
    }

    pub(crate) fn tail_perform(&mut self, effect: RcEffect, args: Vec<Value>) {
        self.frames.pop().expect("no frame to pop for tail perform");
        self.perform_effect(effect, args);
    }

    pub(crate) fn throw_error(&mut self, error: Error) {
        #[expect(clippy::wildcard_enum_match_arm)]
        let (effect, value) = match &error {
            Error::LuciaError(value) => (BuiltinEffect::Error, value.clone()),
            Error::LuciaPanic(value) => (BuiltinEffect::Panic, value.clone()),
            _ => (BuiltinEffect::Error, error.to_string().into()),
        };
        if !self.perform_effect_with(Effect::Builtin(effect).into(), &[value]) {
            self.result = Some(ExecutorResult::Error { error });
        }
    }
}

impl Default for Executor {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeParamsInfo {
    pub(crate) fn check_args(&self, args: &[Value]) -> Result<(), Error> {
        match args.len().cmp(&self.params_count) {
            Ordering::Less => Err(Error::CallArguments {
                required: if self.has_variadic {
                    (self.params_count, None).into()
                } else {
                    self.params_count.into()
                },
                given: args.len(),
            }),
            Ordering::Equal => Ok(()),
            Ordering::Greater => {
                if self.has_variadic {
                    Ok(())
                } else {
                    Err(Error::CallArguments {
                        required: self.params_count.into(),
                        given: args.len(),
                    })
                }
            }
        }
    }

    pub(crate) fn parse_args_to_stack(&self, stack: &mut Vec<Value>, args: &[Value]) {
        if self.has_variadic {
            if let Some((params, variadic)) = args.split_at_checked(self.params_count) {
                stack.extend_from_slice(params);
                stack.push(variadic.into());
            } else {
                stack.extend_from_slice(args);
                stack.push(Table::new().into());
            }
        } else {
            stack.extend_from_slice(args);
        }
    }
}
