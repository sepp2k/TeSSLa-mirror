use std::any::Any;
use std::collections::VecDeque;
use std::ops::Not;
use crate::{TesslaOption, TesslaType};
use crate::TesslaValue;
use crate::TesslaValue::*;

// Explanation:
//  value: Err()             - there may be an event (†)
//  value: Ok(Some(Err()))   - failed to determine event value (♢)
//  value: Ok(Some(Value())) - there is an event with a value
//  value: Ok(None)          - there is no event
pub struct EventContainer<T: TesslaType> {
    value: Result<Option<T>, &'static str>,
    last: Result<Option<T>, &'static str>,
}

type Events<T> = EventContainer<TesslaValue<T>>;

#[inline]
pub fn init<T>() -> Events<T> {
    EventContainer {
        value: Ok(None),
        last: Ok(None),
    }
}

#[inline]
pub fn init_with_value<T>(value: TesslaValue<T>) -> Events<T> {
    EventContainer {
        value: Ok(None),
        last: Ok(Some(value)),
    }
}

impl<T> Events<T> {
    #[inline]
    pub fn has_changed(&self) -> bool {
        match &self.value {
            &Err(_) => true,
            Ok(Some(_)) => true,
            Ok(None) => false,
        }
    }

    #[inline]
    pub fn is_initialised(&self) -> bool {
        self.has_changed()
            || match &self.last {
            &Err(_) => true,
            Ok(Some(_)) => true,
            Ok(None) => false,
        }
    }

    pub fn update_last(&mut self) {
        if self.has_changed() {
            match &mut self.value {
                Ok(value) => self.last = Ok(value.take()),
                &mut Err(error) => {
                    self.last = Err(error);
                    self.value = Ok(None);
                }
            }
        }
    }

    #[inline]
    pub fn set_value(&mut self, value: TesslaValue<T>) {
        self.value = Ok(Some(value));
    }

    #[inline]
    pub fn set_unknown(&mut self, error: &'static str) {
        self.value = Err(error);
    }

    #[inline]
    pub fn set_error(&mut self, error: &'static str) {
        self.value = Ok(Some(Error(error)))
    }
}

impl<T: Copy> Events<T> {
    pub fn get_value(&self) -> T {
        match &self.value {
            &Err(error) => panic!("Tried to use † event caused by: {}", error),
            Ok(None) => panic!("Tried to use event when there is none"),
            Ok(Some(value)) => value.get_value(),
        }
    }

    // TODO do we need this?
    pub fn get_last(&self) -> T {
        match &self.last {
            &Err(error) => panic!("Tried to use † event caused by: {}", error),
            Ok(None) => panic!("Tried to use event when there is none"),
            Ok(Some(value)) => value.get_value(),
        }
    }

    // TODO do we need this?
    pub fn get_value_or_last(&self) -> T {
        if self.has_changed() {
            self.get_value()
        } else {
            self.get_last()
        }
    }
}

impl<T: Clone> Events<T> {
    pub fn clone_value_from(&mut self, other: &Events<T>) {
        match &other.value {
            Ok(value) => {
                self.value = Ok(value.clone());
            }
            &Err(error) => {
                self.value = Err(error);
            }
        }
    }

    pub fn clone_value(&self) -> TesslaValue<T> {
        match &self.value {
            &Err(error) => panic!("Tried to use † event caused by: {}", error),
            Ok(None) => panic!("Tried to use event when there is none"),
            Ok(Some(value)) => value.clone(),
        }
    }

    pub fn clone_last(&self) -> TesslaValue<T> {
        match &self.last {
            &Err(error) => panic!("Tried to use † event caused by: {}", error),
            Ok(None) => panic!("Tried to use event when there is none"),
            Ok(Some(last)) => last.clone(),
        }
    }

    pub fn clone_value_or_last(&self) -> TesslaValue<T> {
        if self.has_changed() {
            self.clone_value()
        } else {
            self.clone_last()
        }
    }

    pub fn clone_value_or_none(&self) -> TesslaValue<Option<TesslaValue<T>>> {
        if self.has_changed() {
            Value(Some(self.clone_value()))
        } else {
            Value(None)
        }
    }

    pub fn call_output(&self, output_function: Option<fn(TesslaValue<T>, i64)>, current_ts: i64) {
        if self.has_changed() {
            if let Some(out_fn) = output_function {
                out_fn(self.clone_value(), current_ts)
            }
        }
    }
}

// -- STEP FUNCTIONS --

pub fn default<T>(output: &mut Events<T>, input: &Events<T>, timestamp: i64)
where T: Clone {
    if input.has_changed() {
        output.clone_value_from(input);
    } else if timestamp == 0 {
        output.set_value(output.clone_last())
    }
}

pub fn default_from<T>(output: &mut Events<T>, input: &Events<T>, default: &Events<T>)
where T: Clone {
    if input.has_changed() {
        output.clone_value_from(input);
    } else if !input.is_initialised() && default.has_changed() {
        output.clone_value_from(default);
    }
}

pub fn time<T>(output: &mut Events<i64>, input: &Events<T>, timestamp: i64) {
    if input.has_changed() {
        output.set_value(Value(timestamp));
    }
}

pub fn last<T, U>(output: &mut Events<T>, values: &Events<T>, trigger: &Events<U>)
where T: Clone {
    if trigger.has_changed() {
        output.clone_value_from(values);
    }
}

pub fn delay(output: &mut Events<()>, delays: &Events<i64>, resets: &Events<T>, nextDelay: &mut i64, timestamp: i64) {

    if nextDelay == timestamp {
        output.set_value(Value(()));
    }
    if output.has_changed() || resets.has_changed() {
        if delays.has_changed() {
            if delays.clone_value() > 0 {
                nextDelay = timestamp + delays.clone_value();
            }else{
                panic!("Tried to set a delay lower than one.");
            }
        }else{
            nextDelay = timestamp;
        }
    }
}

pub fn lift<T, U, F>(output: &mut Events<T>, inputs: Vec<&Events<U>>, function: F)
where U: Any + Sized + Clone, F: FnOnce(&mut VecDeque<TesslaOption<TesslaValue<U>>>) -> TesslaOption<TesslaValue<T>> {
    let mut any_changed = false;
    let mut values = VecDeque::with_capacity(inputs.capacity());
    for input in inputs {
        if input.has_changed() {
            any_changed = true;
        }
        values.push_back(input.clone_value_or_none())
    }
    if any_changed {
        let res = function(&mut values);
        if let Value(Some(value)) = res {
            output.set_value(value);
        }
    }
}

pub fn slift<T, U, F>(output: &mut Events<T>, inputs: Vec<&Events<U>>, function: F)
    where U: Any + Sized + Clone, F: FnOnce(&mut VecDeque<TesslaValue<U>>) -> TesslaValue<T> {
    let mut any_changed = false;
    let mut all_initialised = true;
    let mut values = VecDeque::with_capacity(inputs.capacity());
    for input in inputs {
        if !input.is_initialised() {
            all_initialised = false;
            break;
        } else if input.has_changed() {
            any_changed = true;
        }
        values.push_back(input.clone_value_or_last())
    }
    if any_changed && all_initialised {
        output.set_value(function(&mut values));
    }
}

pub fn merge<T>(output: &mut Events<T>, streams: Vec<&Events<T>>)
where T: Clone {
    for i in 0..streams.len() {
        let stream = streams[i];
        if stream.has_changed() {
            output.clone_value_from(&stream);
            // TODO output is a known value iff all changed streams are as well
            if output.value.is_ok() {
                return;
            }
        }
    }
}

pub fn count<T>(output: &mut Events<i64>, trigger: &Events<T>)
    where T: Clone {
    if trigger.has_changed() {
        output.set_value(output.clone_value() + Value(1_i64));
    }
}

// const
pub fn constant<T, U>(output: &mut Events<T>, value: TesslaValue<T>, trigger: &Events<U>) {
    if trigger.has_changed() {
        output.set_value(value);
    }
}

pub fn filter<T>(output: &mut Events<T>, values: &Events<T>, condition: &Events<bool>)
    where T: Clone {
    if values.has_changed() && condition.get_value_or_last() {
        output.clone_value_from(&values);
    }
}

pub fn fold<T, U>(output: &mut Events<T>, stream: &Events<U>, function: fn(TesslaValue<T>, TesslaValue<U>) -> TesslaValue<T>)
    where T: Clone, U: Clone {
    if stream.has_changed() {
        output.set_value(function(output.clone_last(), stream.clone_value()));
    }
}

pub fn reduce<T>(output: &mut Events<T>, input: &Events<T>, function: fn(TesslaValue<T>, TesslaValue<T>) -> TesslaValue<T>)
    where T: Clone {
    if input.has_changed() {
        if output.is_initialised() {
            output.set_value(function(output.clone_last(), input.clone_value()));
        } else {
            output.clone_value_from(&input);
        }
    }
}

pub fn unitIf(output: &mut Events<()>, cond: &Events<bool>) {
    if cond.get_value() {
        output.set_value(Value(()));
    }
}

pub fn pure<T>(output: &mut Events<T>, stream: &Events<T>)
    where T: Clone + PartialEq {
    if stream.has_changed() {
        if !output.is_initialised() || stream.value != output.value {
            output.clone_value_from(&stream);
        }
    }
}
