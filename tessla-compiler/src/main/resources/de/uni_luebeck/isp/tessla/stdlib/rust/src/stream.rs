use crate::TesslaType;
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

// const
pub fn constant<T>(output: &mut Events<T>, value: T, trigger: &Events<T>)
    where T: Clone{
    if trigger.has_changed() {
        output.set_value(value);
    }
}

pub fn filter<T>(output: &mut Events<T>, values: &Events<U1>, condition: &Events<bool>)
where T: Clone{
    if values.has_changed() && condition.get_value_or_last() {
        output.set_value(values.get_value());
    }
}

pub fn pure<T>(output: &mut Events<T>, stream: &Events<U1>)
    where U1: PartialEq ,T: Clone{
    if stream.has_changed() && stream.get_value() != output.get_last() {
        output.set_value(stream.get_value());
    }
}

pub fn fold<T, U>(output: &mut Events<T>, stream: &Events<U>, function: fn(T, U) -> T)
    where U: Clone, T: Clone {
    if stream.has_changed() {
        output.set_value(function(output.get_last(), stream.get_value()));
    }
}

pub fn unitIf(output: &mut Events<T>, cond: &Events<bool>) where T: From<()> {
    if cond.get_value() {
        output.set_value(T::from(()));
    }
}

pub fn reduce<T>(output: &mut Events<T>, input: &Events<T>, function: fn(T,T) -> T)
    where T: Clone {
    if input.has_changed() {
        if output.last.is_some() {
            output.set_value(function(output.get_last(), input.get_value()));
        }else{
            output.set_value(input.get_value());
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

pub fn delay<T>(output: &mut Events<()>, delays: &Events<i64>, resets: &Events<T>) {}

macro_rules! lift {
    ($output:ident, $func:ident, $($arg:ident),+) => {
        if $($arg.has_changed())||+ {
            let res = $func($($arg.get_value_or()),+);
            if let Some(value) = res{
                $output.set_value(value);
            }
        }
    }
}

macro_rules! slift {
    ($output:ident, $func:ident, $($arg:ident),+) => {
        if $($arg.has_changed())||+ && $(($arg.value.is_some() || $arg.last.is_some()))&&+ {
            let res = $func($($arg.get_value_or_last()),+);
            if let Some(value) = res{
                $output.set_value(value);
            }
        }
    }
}

macro_rules! merge {
    ($output:ident, $($arg:ident),+) => {${
        if $arg.has_changed() {
            $output.clone_value_from(&$arg);
            if let Ok(_) = $output.value {
                return;
            }
        }
    }+}
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
        output.set_value(output.get_value() + Value(1_i64));
    }
}