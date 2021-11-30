use std::ops::Add;

pub type StreamValue<T> = Option<Result<T, &'static str>>;

pub struct Stream<T> where T: Clone {
    value : StreamValue<T>,
    last : StreamValue<T>,
    // changed: bool, // If we use this: value stores the last value, unless changed is true
    unknown: bool, // TODO could be Err() ??
}

// TODO StandardStream, LastStream, DelayStream

#[inline]
pub fn init<T>() -> Stream<T> where T: Clone {
    Stream {
        value: None,
        last: None,
        unknown: false
    }
}

#[inline]
pub fn init_with_value<T>(value: T) -> Stream<T> where T: Clone {
    Stream {
        value: None,
        last: Some(Ok(value)),
        unknown: false
    }
}

impl<T> Stream<T> where T: Clone {

    // -- HELPERS --

    pub fn has_changed(&self) -> bool {
        self.value.is_some()
    }

    pub fn clone_value(&mut self, other: &Stream<T>) {
        self.value = other.value.clone();
        self.unknown = other.unknown;
    }

    pub fn set_result(&mut self, result: Result<T, &'static str>) {
        self.value = Some(result);
        self.unknown = false;
    }

    pub fn set_value(&mut self, value: T) {
        self.value = Some(Ok(value));
        self.unknown = false;
    }

    pub fn set_unknown(&mut self, error: &'static str) {
        self.value = Some(Err(error));
        self.unknown = true;
    }

    pub fn update_last(&mut self) {
        if self.has_changed() {
            self.last = self.value.clone(); // TODO use move
            self.value = None;
        }
    }

    pub fn unwrap_value(&self) -> T {
        self.value.clone().unwrap().unwrap()
    }

    pub fn unwrap_last(&self) -> T {
        self.last.clone().unwrap().unwrap()
    }

    pub fn unwrap_value_or(&self) -> Option<T> {
        if self.value.is_some() {
            Some(self.unwrap_value())
        } else {
            None
        }
    }

    pub fn unwrap_value_or_last(&self) -> T {
        if self.value.is_some() {
            self.unwrap_value()
        } else {
            self.unwrap_last()
        }
    }

    pub fn call_output(&self, output_function: Option<fn(T, i64)>, current_ts: i64)
        where T: Clone {
        if self.value.is_some() {
            match output_function {
                Some(out_fn) => out_fn(self.unwrap_value(), current_ts),
                None => {}
            }
        }
    }

// -- STEP FUNCTIONS --

    pub fn default(&mut self, input: &Stream<T>) {
        if input.has_changed() {
            self.clone_value(input);
        }
    }

    pub fn merge(&mut self, streams: Vec<&Stream<T>>) {
        for i in 0..streams.len() {
            let stream = streams[i];
            if stream.has_changed() {
                self.clone_value(&stream);
                // TODO out.unknown is true iff all changed streams are as well
                if !self.unknown {
                    return;
                }
            }
        }
    }

    pub fn lift1<U0>(&mut self, arg0: &Stream<U0>, function: fn(Option<U0>) -> T)
        where U0: Clone {
        if arg0.has_changed() {
            self.set_value(function(arg0.unwrap_value_or()));
        }
    }

    pub fn lift2<U0, U1>(&mut self, arg0: &Stream<U0>, arg1: &Stream<U1>, function: fn(Option<U0>, Option<U1>) -> T)
        where U0: Clone, U1: Clone {
        if arg0.has_changed() || arg1.has_changed() {
            self.set_value(function(arg0.unwrap_value_or(), arg1.unwrap_value_or()));
        }
    }

    pub fn lift3<U0, U1, U2>(&mut self, arg0: &Stream<U0>, arg1: &Stream<U1>, arg2: &Stream<U2>, function: fn(Option<U0>, Option<U1>, Option<U2>) -> T)
        where U0: Clone, U1: Clone, U2: Clone {
        if arg0.has_changed() || arg1.has_changed() || arg2.has_changed() {
            self.set_value(function(arg0.unwrap_value_or(), arg1.unwrap_value_or(), arg2.unwrap_value_or()));
        }
    }

    pub fn lift4<U0, U1, U2, U3>(&mut self, arg0: &Stream<U0>, arg1: &Stream<U1>, arg2: &Stream<U2>, arg3: &Stream<U3>, function: fn(Option<U0>, Option<U1>, Option<U2>, Option<U3>) -> T)
        where U0: Clone, U1: Clone, U2: Clone, U3: Clone {
        if arg0.has_changed() || arg1.has_changed() || arg2.has_changed() || arg3.has_changed() {
            self.set_value(function(arg0.unwrap_value_or(), arg1.unwrap_value_or(), arg2.unwrap_value_or(), arg3.unwrap_value_or()));
        }
    }

    pub fn lift5<U0, U1, U2, U3, U4>(&mut self, arg0: &Stream<U0>, arg1: &Stream<U1>, arg2: &Stream<U2>, arg3: &Stream<U3>, arg4: &Stream<U4>, function: fn(Option<U0>, Option<U1>, Option<U2>, Option<U3>, Option<U4>) -> T)
        where U0: Clone, U1: Clone, U2: Clone, U3: Clone, U4: Clone {
        if arg0.has_changed() || arg1.has_changed() || arg2.has_changed() || arg3.has_changed() || arg4.has_changed() {
            self.set_value(function(arg0.unwrap_value_or(), arg1.unwrap_value_or(), arg2.unwrap_value_or(), arg3.unwrap_value_or(), arg4.unwrap_value_or()));
        }
    }

    // TODO I think we need a single slift for each number of arguments...
    pub fn slift1<U0>(&mut self, arg0: &Stream<U0>, function: fn(U0) -> T)
        where U0: Clone {
        if arg0.has_changed() {
            self.set_value(function(arg0.unwrap_value_or_last()));
        }
    }
    pub fn slift2<U0, U1>(&mut self, arg0: &Stream<U0>, arg1: &Stream<U1>, function: fn(U0, U1) -> T)
        where U0: Clone, U1: Clone {
        if arg0.has_changed() || arg1.has_changed() && (arg0.value.is_some() || arg0.last.is_some()) && (arg1.value.is_some() || arg1.last.is_some()) {
            self.set_value(function(arg0.unwrap_value_or_last(), arg1.unwrap_value_or_last()));
        }
    }

    pub fn slift3<U0, U1, U2>(&mut self, arg0: &Stream<U0>, arg1: &Stream<U1>, arg2: &Stream<U2>, function: fn(U0, U1, U2) -> T)
        where U0: Clone, U1: Clone, U2: Clone {
        if arg0.has_changed() || arg1.has_changed() || arg2.has_changed() && (arg0.value.is_some() || arg0.last.is_some()) && (arg1.value.is_some() || arg1.last.is_some()) && (arg2.value.is_some() || arg2.last.is_some()) {
            self.set_value(function(arg0.unwrap_value_or_last(),arg1.unwrap_value_or_last(),arg2.unwrap_value_or_last()));
        }
    }

    pub fn slift4<U0, U1, U2, U3>(&mut self, arg0: &Stream<U0>, arg1: &Stream<U1>, arg2: &Stream<U2>, arg3: &Stream<U3>, function: fn(U0, U1, U2, U3) -> T)
        where U0: Clone, U1: Clone, U2: Clone, U3: Clone {
        if arg0.has_changed() || arg1.has_changed() || arg2.has_changed() || arg3.has_changed() && (arg0.value.is_some() || arg0.last.is_some()) && (arg1.value.is_some() || arg1.last.is_some()) && (arg2.value.is_some() || arg2.last.is_some()) && (arg3.value.is_some() || arg3.last.is_some()) {
            self.set_value(function(arg0.unwrap_value_or_last(),arg1.unwrap_value_or_last(),arg2.unwrap_value_or_last(),arg3.unwrap_value_or_last()));
        }
    }

    pub fn slift5<U0, U1, U2, U3, U4>(&mut self, arg0: &Stream<U0>, arg1: &Stream<U1>, arg2: &Stream<U2>, arg3: &Stream<U3>, arg4: &Stream<U4>, function: fn(U0, U1, U2, U3, U4) -> T)
        where U0: Clone, U1: Clone, U2: Clone, U3: Clone, U4: Clone {
        if arg0.has_changed() || arg1.has_changed() || arg2.has_changed() || arg3.has_changed() || arg4.has_changed() && (arg0.value.is_some() || arg0.last.is_some()) && (arg1.value.is_some() || arg1.last.is_some()) && (arg2.value.is_some() || arg2.last.is_some()) && (arg3.value.is_some() || arg3.last.is_some()) && (arg4.value.is_some() || arg4.last.is_some()) {
            self.set_value(function(arg0.unwrap_value_or_last(),arg1.unwrap_value_or_last(),arg2.unwrap_value_or_last(),arg3.unwrap_value_or_last(),arg4.unwrap_value_or_last()));
        }
    }

    pub fn last<U>(&mut self, values: &Stream<T>, trigger: &Stream<U>)
        where U: Clone {
        if trigger.has_changed() {
            self.clone_value(values);
        }
    }

    pub fn time(&mut self, arg0: &Stream<T>, timestamp: i64)
        where T: From<i64> {
        if arg0.has_changed() {
            self.set_value(T::from(timestamp));
        }
    }

    // const
    pub fn constant(&mut self, value: T, trigger: &Stream<T>) {
        if trigger.has_changed() {
            self.set_value(value);
        }
    }

    pub fn filter(&mut self, values: &Stream<T>, condition: &Stream<bool>) {
        if values.has_changed() && condition.unwrap_value_or_last() {
            self.set_value(values.unwrap_value());
        }
    }

    pub fn pure(&mut self, stream: &Stream<T>)
        where T: PartialEq {
        if stream.has_changed() && stream.unwrap_value() != self.unwrap_last() {
            self.set_value(stream.unwrap_value());
        }
    }

    pub fn count(&mut self, trigger: &Stream<T>)
        where T: Add<i64> + Into<i64> + From<i64> {
        if trigger.has_changed() {
            self.set_value(T::from(self.unwrap_value().into() + 1_i64));
        }
    }

    pub fn fold<U>(&mut self, stream: &Stream<U>, function: fn(T, U) -> T)
        where U: Clone {
        if stream.has_changed() {
            self.set_value(function(self.unwrap_last(), stream.unwrap_value()));
        }
    }

    pub fn unitIf(&mut self, cond: &Stream<bool>) where T: From<()> {
        if cond.unwrap_value() {
            self.set_value(T::from(())); //TODO need help understanding unit
        }
    }
}