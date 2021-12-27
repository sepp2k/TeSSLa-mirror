use crate::{TesslaOption, TesslaType, Unit};
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
pub fn init_with_value<T: Clone>(value: TesslaValue<T>) -> Events<T> {
    EventContainer {
        value: Ok(Some(value.clone())),
        last: Ok(Some(value)),
    }
}

impl<T> Events<T> {
    #[inline]
    pub fn set_error(&mut self, error: &'static str) {
        self.value = Err(error);
    }

    #[inline]
    pub fn has_error(&self) -> bool {
        match &self.value {
            Err(_) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn set_event(&mut self, value: TesslaValue<T>) {
        self.value = Ok(Some(value));
    }

    #[inline]
    pub fn has_event(&self) -> bool {
        match &self.value {
            Ok(Some(_)) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_initialised(&self) -> bool {
        match (&self.value, &self.last) {
            (Ok(None), Ok(None)) => false,
            _ => true,
        }
    }

    pub fn update_last(&mut self) {
        match &mut self.value {
            Ok(event) => if event.is_some() {
                self.last = Ok(event.take())
            },
            &mut Err(error) => {
                self.last = Err(error);
                self.value = Ok(None);
            }
        }
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
        if self.has_event() {
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

    pub fn call_output(&self, output_function: Option<fn(TesslaValue<T>, i64)>, current_ts: i64) {
        if self.has_event() {
            if let Some(out_fn) = output_function {
                out_fn(self.clone_value(), current_ts)
            }
        }
    }
}

// -- STEP FUNCTIONS --

pub fn default<T>(output: &mut Events<T>, input: &Events<T>)
    where T: Clone {
    if input.has_event() {
        output.clone_value_from(input);
    }
}

pub fn default_from<T>(output: &mut Events<T>, input: &Events<T>, default: &Events<T>)
    where T: Clone {
    if input.has_event() {
        output.clone_value_from(input);
    } else if !input.is_initialised() && default.has_event() {
        output.clone_value_from(default);
    }
}

pub fn time<T>(output: &mut Events<i64>, input: &Events<T>, timestamp: i64) {
    if input.has_event() {
        output.set_event(Value(timestamp));
    }
}

pub fn last<T, U>(output: &mut Events<T>, values: &Events<T>, trigger: &Events<U>)
    where T: Clone {
    if values.is_initialised() {
        match trigger.value {
            Err(error) => output.set_error(error),
            Ok(_) => match values.last {
                Err(error) => output.set_error(error),
                Ok(Some(_)) => output.set_event(values.clone_last()),
                Ok(None) => {}
            }
        }
    }
}

// FIXME: I think this does not quite cover all possible specified Error states
pub fn delay<T>(output: &mut Events<Unit>, delays: &Events<i64>, resets: &Events<T>, next_delay: &mut i64, timestamp: i64) {
    if *next_delay == timestamp {
        output.set_event(Value(Unit::Unit));
    }
    if output.has_event() || resets.has_event() {
        if delays.has_event() {
            let delay_value = delays.get_value();
            if delay_value > 0 {
                *next_delay = timestamp + delay_value;
            } else {
                panic!("Tried to set a delay lower than one.");
            }
        }
    }
}

impl<T: Clone> Events<T> {
    fn clone_value_for_lift(&self) -> TesslaOption<TesslaValue<T>> {
        match &self.value {
            &Err(error) => Error(error),
            Ok(None) => Value(None),
            Ok(Some(event)) => Value(Some(event.clone())),
        }
    }
}

pub fn lift1<T, U0, F>(output: &mut Events<T>, arg0: &Events<U0>, function: F)
    where U0: Clone,
          F: FnOnce(TesslaOption<TesslaValue<U0>>) -> TesslaOption<TesslaValue<T>> {
    let any_error = arg0.has_error();
    let any_event = arg0.has_event();
    if any_event {
        let res = function(arg0.clone_value_for_lift());
        match res {
            Error(error) => output.set_error(error),
            Value(Some(event)) => output.set_event(event),
            Value(None) => {},
        }
    } else if any_error {
        output.set_error("Lift couldn't determine if there should be an event");
    }
}

pub fn lift2<T, U0, U1, F>(output: &mut Events<T>, arg0: &Events<U0>, arg1: &Events<U1>, function: F)
    where U0: Clone, U1: Clone,
          F: FnOnce(TesslaOption<TesslaValue<U0>>, TesslaOption<TesslaValue<U1>>) -> TesslaOption<TesslaValue<T>> {
    let any_error = arg0.has_error() || arg1.has_error();
    let any_event = arg0.has_event() || arg1.has_event();
    if any_event {
        let res = function(arg0.clone_value_for_lift(), arg1.clone_value_for_lift());
        match res {
            Error(error) => output.set_error(error),
            Value(Some(event)) => output.set_event(event),
            Value(None) => {},
        }
    } else if any_error {
        output.set_error("Lift couldn't determine if there should be an event");
    }
}

pub fn lift3<T, U0, U1, U2, F>(output: &mut Events<T>, arg0: &Events<U0>, arg1: &Events<U1>, arg2: &Events<U2>, function: F)
    where U0: Clone, U1: Clone, U2: Clone,
          F: FnOnce(TesslaOption<TesslaValue<U0>>, TesslaOption<TesslaValue<U1>>, TesslaOption<TesslaValue<U2>>) -> TesslaOption<TesslaValue<T>> {
    let any_error = arg0.has_error() || arg1.has_error() || arg2.has_error();
    let any_event = arg0.has_event() || arg1.has_event() || arg2.has_event();
    if any_event {
        let res = function(arg0.clone_value_for_lift(), arg1.clone_value_for_lift(), arg2.clone_value_for_lift());
        match res {
            Error(error) => output.set_error(error),
            Value(Some(event)) => output.set_event(event),
            Value(None) => {},
        }
    } else if any_error {
        output.set_error("Lift couldn't determine if there should be an event");
    }
}

pub fn lift4<T, U0, U1, U2, U3, F>(output: &mut Events<T>, arg0: &Events<U0>, arg1: &Events<U1>, arg2: &Events<U2>, arg3: &Events<U3>, function: F)
    where U0: Clone, U1: Clone, U2: Clone, U3: Clone,
          F: FnOnce(TesslaOption<TesslaValue<U0>>, TesslaOption<TesslaValue<U1>>, TesslaOption<TesslaValue<U2>>, TesslaOption<TesslaValue<U3>>) -> TesslaOption<TesslaValue<T>> {
    let any_error = arg0.has_error() || arg1.has_error() || arg2.has_error() || arg3.has_error();
    let any_event = arg0.has_event() || arg1.has_event() || arg2.has_event() || arg3.has_event();
    if any_event {
        let res = function(arg0.clone_value_for_lift(), arg1.clone_value_for_lift(),
                           arg2.clone_value_for_lift(), arg3.clone_value_for_lift());
        match res {
            Error(error) => output.set_error(error),
            Value(Some(event)) => output.set_event(event),
            Value(None) => {},
        }
    } else if any_error {
        output.set_error("Lift couldn't determine if there should be an event");
    }
}

pub fn lift5<T, U0, U1, U2, U3, U4, F>(output: &mut Events<T>, arg0: &Events<U0>, arg1: &Events<U1>, arg2: &Events<U2>, arg3: &Events<U3>, arg4: &Events<U4>, function: F)
    where U0: Clone, U1: Clone, U2: Clone, U3: Clone, U4: Clone,
          F: FnOnce(TesslaOption<TesslaValue<U0>>, TesslaOption<TesslaValue<U1>>, TesslaOption<TesslaValue<U2>>, TesslaOption<TesslaValue<U3>>, TesslaOption<TesslaValue<U4>>) -> TesslaOption<TesslaValue<T>> {
    let any_error = arg0.has_error() || arg1.has_error() || arg2.has_error() || arg3.has_error() || arg4.has_error();
    let any_event = arg0.has_event() || arg1.has_event() || arg2.has_event() || arg3.has_event() || arg4.has_event();
    if any_event {
        let res = function(arg0.clone_value_for_lift(), arg1.clone_value_for_lift(),
                           arg2.clone_value_for_lift(), arg3.clone_value_for_lift(), arg4.clone_value_for_lift());
        match res {
            Error(error) => output.set_error(error),
            Value(Some(event)) => output.set_event(event),
            Value(None) => {},
        }
    } else if any_error {
        output.set_error("Lift couldn't determine if there should be an event");
    }
}

impl<T: Clone> Events<T> {
    fn clone_value_for_slift(&self) -> TesslaValue<T> {
        if self.has_event() {
            self.clone_value()
        } else {
            self.clone_last()
        }
    }
}

pub fn slift1<T, U0, F>(output: &mut Events<T>, arg0: &Events<U0>, function: F)
    where U0: Clone,
          F: FnOnce(TesslaValue<U0>) -> TesslaValue<T> {
    let any_event = arg0.has_event();
    let all_initialised = arg0.is_initialised();
    if any_event && all_initialised {
        output.set_event(function(arg0.clone_value_for_slift()));
    }
}

pub fn slift2<T, U0, U1, F>(output: &mut Events<T>, arg0: &Events<U0>, arg1: &Events<U1>, function: F)
    where U0: Clone, U1: Clone,
          F: FnOnce(TesslaValue<U0>, TesslaValue<U1>) -> TesslaValue<T> {
    let any_event = arg0.has_event() || arg1.has_event();
    let all_initialised = arg0.is_initialised() && arg1.is_initialised();
    if any_event && all_initialised {
        output.set_event(function(arg0.clone_value_for_slift(), arg1.clone_value_for_slift()));
    }
}

pub fn slift3<T, U0, U1, U2, F>(output: &mut Events<T>, arg0: &Events<U0>, arg1: &Events<U1>, arg2: &Events<U2>, function: F)
    where U0: Clone, U1: Clone, U2: Clone,
          F: FnOnce(TesslaValue<U0>, TesslaValue<U1>, TesslaValue<U2>) -> TesslaValue<T> {
    let any_event = arg0.has_event() || arg1.has_event() || arg2.has_event();
    let all_initialised = arg0.is_initialised() && arg1.is_initialised() && arg2.is_initialised();
    if any_event && all_initialised {
        output.set_event(function(arg0.clone_value_for_slift(), arg1.clone_value_for_slift(), arg2.clone_value_for_slift()));
    }
}

pub fn slift4<T, U0, U1, U2, U3, F>(output: &mut Events<T>, arg0: &Events<U0>, arg1: &Events<U1>, arg2: &Events<U2>, arg3: &Events<U3>, function: F)
    where U0: Clone, U1: Clone, U2: Clone, U3: Clone,
          F: FnOnce(TesslaValue<U0>, TesslaValue<U1>, TesslaValue<U2>, TesslaValue<U3>) -> TesslaValue<T> {
    let any_event = arg0.has_event() || arg1.has_event() || arg2.has_event() || arg3.has_event();
    let all_initialised = arg0.is_initialised() && arg1.is_initialised() && arg2.is_initialised() && arg3.is_initialised();
    if any_event && all_initialised {
        output.set_event(function(arg0.clone_value_for_slift(), arg1.clone_value_for_slift(), arg2.clone_value_for_slift(), arg3.clone_value_for_slift()));
    }
}

pub fn slift5<T, U0, U1, U2, U3, U4, F>(output: &mut Events<T>, arg0: &Events<U0>, arg1: &Events<U1>, arg2: &Events<U2>, arg3: &Events<U3>, arg4: &Events<U4>, function: F)
    where U0: Clone, U1: Clone, U2: Clone, U3: Clone, U4: Clone,
          F: FnOnce(TesslaValue<U0>, TesslaValue<U1>, TesslaValue<U2>, TesslaValue<U3>, TesslaValue<U4>) -> TesslaValue<T> {
    let any_event = arg0.has_event() || arg1.has_event() || arg2.has_event() || arg3.has_event() || arg4.has_event();
    let all_initialised = arg0.is_initialised() && arg1.is_initialised() && arg2.is_initialised() && arg3.is_initialised() && arg4.is_initialised();
    if any_event && all_initialised {
        output.set_event(function(arg0.clone_value_for_slift(), arg1.clone_value_for_slift(), arg2.clone_value_for_slift(), arg3.clone_value_for_slift(), arg4.clone_value_for_slift()));
    }
}

pub fn merge<T>(output: &mut Events<T>, streams: Vec<&Events<T>>)
    where T: Clone {
    for i in 0..streams.len() {
        let stream = streams[i];
        if stream.has_event() {
            output.clone_value_from(&stream);
            // TODO output is a known value iff all changed streams are as well
            if output.value.is_ok() {
                return;
            }
        }
    }
}

pub fn count<T>(output: &mut Events<i64>, trigger: &Events<T>) {
    if trigger.has_event() {
        output.set_event(output.clone_last() + Value(1_i64));
    }
}

// const
pub fn constant<T, U>(output: &mut Events<T>, value: TesslaValue<T>, trigger: &Events<U>) {
    if trigger.has_event() {
        output.set_event(value);
    }
}

pub fn filter<T>(output: &mut Events<T>, values: &Events<T>, condition: &Events<bool>)
    where T: Clone {
    if values.has_event() && condition.is_initialised() && condition.get_value_or_last() {
        output.clone_value_from(&values);
    }
}

pub fn fold<T, U>(output: &mut Events<T>, stream: &Events<U>, function: fn(TesslaValue<T>, TesslaValue<U>) -> TesslaValue<T>)
    where T: Clone, U: Clone {
    if stream.has_event() {
        output.set_event(function(output.clone_last(), stream.clone_value()));
    }
}

pub fn reduce<T>(output: &mut Events<T>, input: &Events<T>, function: fn(TesslaValue<T>, TesslaValue<T>) -> TesslaValue<T>)
    where T: Clone {
    if input.has_event() {
        if output.is_initialised() {
            output.set_event(function(output.clone_last(), input.clone_value()));
        } else {
            output.clone_value_from(&input);
        }
    }
}

pub fn unitIf(output: &mut Events<Unit>, cond: &Events<bool>) {
    if cond.has_event() && cond.get_value() {
        output.set_event(Value(Unit::Unit));
    }
}

pub fn pure<T>(output: &mut Events<T>, stream: &Events<T>)
    where T: Clone + PartialEq {
    if stream.has_event() {
        if !output.is_initialised() || stream.clone_value().ne(&output.clone_last()).get_value() {
            output.clone_value_from(&stream);
        }
    }
}
