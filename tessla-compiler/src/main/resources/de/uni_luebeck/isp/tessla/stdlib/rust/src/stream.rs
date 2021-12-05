use crate::TesslaType;
use crate::TesslaValue;
use crate::TesslaValue::*;

/*
pub enum TesslaOption<T> {
    Some(T),
    Error(&'static str),
    None,
}

impl<T> TesslaOption<T> {
    #[inline]
    fn is_some(&self) -> bool {
        matches!(*self, TesslaOption::Some(_))
    }

    #[inline]
    fn is_error(&self) -> bool {
        matches!(*self, TesslaOption::Error(_))
    }

    #[inline]
    fn is_none(&self) -> bool {
        matches!(*self, TesslaOption::None)
    }

    #[inline]
    fn is_event(&self) -> bool {
        self.is_some() || self.is_error()
    }

    #[inline]
    fn take(&mut self) -> Self {
        mem::take(self)
    }
}

impl<T: Clone> TesslaOption<T> {
    fn get_value(&self) -> T {
        if let TesslaOption::Some(value) = self {
            value.clone()
        } else if let TesslaOption::Error(error) = self {
            panic!("Expected a value, got an error: {}", error)
        } else {
            panic!("Expected a value, got a none")
        }
    }
}

impl<T> TesslaOption<TesslaOption<T>> {
    fn tessla_is_none(&self) -> TesslaOption<bool> {
        use TesslaOption::*;
        match &self {
            Some(_) => Some(false),
            Error(error) => Error(error),
            None => Some(true),
        }
    }

    fn tessla_is_some(&self) -> TesslaOption<bool> {
        use TesslaOption::*;
        match &self {
            Some(_) => Some(true),
            Error(error) => Error(error),
            None => Some(false),
        }
    }
}

impl<T: Clone> TesslaOption<TesslaOption<T>> {
    fn tessla_get_some(&self) -> TesslaOption<T> {
        use TesslaOption::*;
        match self {
            Some(value) => value.clone(),
            Error(error) => Error(error),
            None => Error("Tried to getSome(None)"),
        }
    }
}

impl<T> Default for TesslaOption<T> {
    #[inline]
    fn default() -> Self {
        TesslaOption::None
    }
}

impl<T: Clone> Clone for TesslaOption<T> {
    fn clone(&self) -> Self {
        match self {
            TesslaOption::Some(value) => TesslaOption::Some(value.clone()),
            TesslaOption::Error(error) => TesslaOption::Error(error),
            TesslaOption::None => TesslaOption::None,
        }
    }
}*/

/*
impl<T> Deref for TesslaOption<T> {
    type Target = Option<Result<T, &'static str>>;

    fn deref(&self) -> &Self::Target {
        match self {
            &TesslaOption::Some(value) => &Some(Ok(&value)),
            &TesslaOption::Error(error) => &Some(Err(error)),
            TesslaOption::None => None,
        }
    }
}*/

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

    // TODO replace with proper failing functions..
    pub fn get_value(&self) -> T where T: Clone {
        match &self.value {
            &Err(error) => panic!("Tried to use † event caused by: {}", error),
            Ok(None) => panic!("Tried to use event when there is none"),
            Ok(Some(value)) => value.clone_value(),
        }
    }

    pub fn get_last(&self) -> T where T: Clone {
        match &self.last {
            &Err(error) => panic!("Tried to use † event caused by: {}", error),
            Ok(None) => panic!("Tried to use event when there is none"),
            Ok(Some(value)) => value.clone_value(),
        }
    }

    pub fn get_value_or_last(&self) -> T where T: Clone {
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
            &Err(error) => panic!(""),
            Ok(None) => panic!(""),
            Ok(Some(value)) => value.clone(),
        }
    }

    pub fn clone_last(&self) -> TesslaValue<T> {
        match &self.last {
            &Err(error) => panic!(""),
            Ok(None) => panic!(""),
            Ok(Some(last)) => last.clone(),
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


macro_rules! lift{
    ($output:ident, $func:ident, $($arg:ident),+) => {
        if $($arg.has_changed())||+ {
            let res = $func($($arg.get_value_or()),+);
            if let Some(value) = res{
                $output.set_value(value);
            }
        }
    }
}


impl<T: TesslaType> EventContainer<T> {

    // -- STEP FUNCTIONS --
    /*
    // TODO I think we need a single slift for each number of arguments...
    pub fn slift1<U0>(&mut self, arg0: &Stream<U0>, function: fn(U0) -> T)
        where U0: Clone {
        if arg0.has_changed() {
            self.set_value(function(arg0.get_value_or_last()));
        }
    }
    pub fn slift2<U0, U1>(&mut self, arg0: &Stream<U0>, arg1: &Stream<U1>, function: fn(U0, U1) -> T)
        where U0: Clone, U1: Clone {
        if arg0.has_changed() || arg1.has_changed() && (arg0.value.is_some() || arg0.last.is_some()) && (arg1.value.is_some() || arg1.last.is_some()) {
            self.set_value(function(arg0.get_value_or_last(), arg1.get_value_or_last()));
        }
    }

    pub fn slift3<U0, U1, U2>(&mut self, arg0: &Stream<U0>, arg1: &Stream<U1>, arg2: &Stream<U2>, function: fn(U0, U1, U2) -> T)
        where U0: Clone, U1: Clone, U2: Clone {
        if arg0.has_changed() || arg1.has_changed() || arg2.has_changed() && (arg0.value.is_some() || arg0.last.is_some()) && (arg1.value.is_some() || arg1.last.is_some()) && (arg2.value.is_some() || arg2.last.is_some()) {
            self.set_value(function(arg0.get_value_or_last(),arg1.get_value_or_last(),arg2.get_value_or_last()));
        }
    }

    pub fn slift4<U0, U1, U2, U3>(&mut self, arg0: &Stream<U0>, arg1: &Stream<U1>, arg2: &Stream<U2>, arg3: &Stream<U3>, function: fn(U0, U1, U2, U3) -> T)
        where U0: Clone, U1: Clone, U2: Clone, U3: Clone {
        if arg0.has_changed() || arg1.has_changed() || arg2.has_changed() || arg3.has_changed() && (arg0.value.is_some() || arg0.last.is_some()) && (arg1.value.is_some() || arg1.last.is_some()) && (arg2.value.is_some() || arg2.last.is_some()) && (arg3.value.is_some() || arg3.last.is_some()) {
            self.set_value(function(arg0.get_value_or_last(),arg1.get_value_or_last(),arg2.get_value_or_last(),arg3.get_value_or_last()));
        }
    }

    pub fn slift5<U0, U1, U2, U3, U4>(&mut self, arg0: &Stream<U0>, arg1: &Stream<U1>, arg2: &Stream<U2>, arg3: &Stream<U3>, arg4: &Stream<U4>, function: fn(U0, U1, U2, U3, U4) -> T)
        where U0: Clone, U1: Clone, U2: Clone, U3: Clone, U4: Clone {
        if arg0.has_changed() || arg1.has_changed() || arg2.has_changed() || arg3.has_changed() || arg4.has_changed() && (arg0.value.is_some() || arg0.last.is_some()) && (arg1.value.is_some() || arg1.last.is_some()) && (arg2.value.is_some() || arg2.last.is_some()) && (arg3.value.is_some() || arg3.last.is_some()) && (arg4.value.is_some() || arg4.last.is_some()) {
            self.set_value(function(arg0.get_value_or_last(),arg1.get_value_or_last(),arg2.get_value_or_last(),arg3.get_value_or_last(),arg4.get_value_or_last()));
        }
    }

    // const
    pub fn constant(&mut self, value: T, trigger: &Stream<T>) {
        if trigger.has_changed() {
            self.set_value(value);
        }
    }

    pub fn filter(&mut self, values: &Stream<T>, condition: &Stream<bool>) {
        if values.has_changed() && condition.get_value_or_last() {
            self.set_value(values.get_value());
        }
    }

    pub fn pure(&mut self, stream: &Stream<T>)
        where T: PartialEq {
        if stream.has_changed() && stream.get_value() != self.get_last() {
            self.set_value(stream.get_value());
        }
    }

    pub fn count(&mut self, trigger: &Stream<T>)
        where T: Add<i64> + Into<i64> + From<i64> {
        if trigger.has_changed() {
            self.set_value(T::from(self.get_value().into() + 1_i64));
        }
    }

    pub fn fold<U>(&mut self, stream: &Stream<U>, function: fn(T, U) -> T)
        where U: Clone {
        if stream.has_changed() {
            self.set_value(function(self.get_last(), stream.get_value()));
        }
    }

    pub fn unitIf(&mut self, cond: &Stream<bool>) where T: From<()> {
        if cond.get_value() {
            self.set_value(T::from(())); //TODO need help understanding unit
        }
    }
    */
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