use std::ops::{Add, Deref};

pub struct TesslaOption<T> {
    value: Option<Result<T, &'static str>>,
}

// TODO Deref<Option<T>> https://doc.rust-lang.org/std/ops/trait.Deref.html

impl<T> TesslaOption<T> where T: Clone {
    fn Some(value: T) -> TesslaOption<T> {
        TesslaOption { value: Some(Ok(value)) }
    }
    fn None() -> TesslaOption<T> {
        TesslaOption { value: None }
    }
    fn Err(error: &'static str) -> TesslaOption<T> {
        TesslaOption { value: Some(Err(error)) }
    }

    fn get_some(&self) -> Result<T, &'static str> {
        // TODO use unwrap_infallible/.into_ok
        self.value.ok_or("Tried to get value even though there was no event")?
    }

    fn take(&mut self) -> TesslaOption<T> {
        TesslaOption { value: self.value.take() }
    }

    fn clone(&self) -> TesslaOption<T> {
        TesslaOption { value: self.value.clone() }
    }
}

impl<T> Deref for TesslaOption<T> {
    type Target = Option<Result<T, &'static str>>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

// Explanation:
//  value: Err()      - there may be an event (†)
//  value: Ok(Err())  - failed to determine event value (♢)
//  value: Ok(Some()) - there is an event with a value
//  value: Ok(None()) - there is no event
pub struct Stream<T> where T: Clone {
    value : Result<TesslaOption<T>, &'static str>,
    last : Result<TesslaOption<T>, &'static str>,
}

#[inline]
pub fn init<T>() -> Stream<T> where T: Clone {
    Stream {
        value: Ok(TesslaOption::None()),
        last: Ok(TesslaOption::None()),
    }
}

#[inline]
pub fn init_with_value<T>(value: T) -> Stream<T> where T: Clone {
    Stream {
        value: Ok(TesslaOption::None()),
        last: Ok(TesslaOption::Some(value)),
    }
}

impl<T> Stream<T> where T: Clone {

    // -- HELPERS --

    pub fn has_changed(&self) -> bool {
        match &self.value {
            Ok(value) => value.is_some(),
            &Err(_) => true
        }
    }

    pub fn update_last(&mut self) {
        if self.has_changed() {
            match &mut self.value {
                Ok(value) => {
                    self.last = Ok(value.take())
                },
                &mut Err(error) => {
                    self.last = Err(error);
                    self.value = Ok(TesslaOption::None());
                }
            }
        }
    }

    pub fn clone_value_from(&mut self, other: &Stream<T>) {
        match &other.value {
            Ok(value) => {
                self.value = Ok(value.clone());
            },
            &Err(error) => {
                self.value = Err(error);
            }
        }
    }

    pub fn set_value(&mut self, value: T) {
        self.value = Ok(TesslaOption::Some(value));
    }

    pub fn set_error(&mut self, error: &'static str) {
        self.value = Ok(TesslaOption::Err(error))
    }

    pub fn set_unknown(&mut self, error: &'static str) {
        self.value = Err(error);
    }

    // TODO replace with proper failing functions..
    pub fn get_value(&self) -> T {
        match &self.value {
            Ok(value) => value.get_some(),
            &Err(error) => panic!("Tried to use † event caused by: {}", error)
        }
    }

    pub fn get_last(&self) -> T {
        match &self.last {
            Ok(value) => value.get_some(),
            &Err(error) => panic!("Tried to use † event caused by: {}", error)
        }
    }

    pub fn get_value_or(&self) -> TesslaOption<T> {
        match &self.value {
            Ok(value) => value.clone(),
            &Err(error) => panic!("")
        }
    }

    pub fn get_value_or_last(&self) -> T {
        if self.has_changed() {
            self.get_value()
        } else {
            self.get_last()
        }
    }

    pub fn call_output(&self, output_function: Option<fn(T, i64)>, current_ts: i64) {
        if self.has_changed() {
            if let Some(out_fn) = output_function {
                out_fn(self.get_value(), current_ts)
            }
        }
    }

// -- STEP FUNCTIONS --


/*
    pub fn lift1<U0>(&mut self, arg0: &Stream<U0>, function: fn(Option<U0>) -> Option<T>)
        where U0: Clone {
        if arg0.has_changed() {
            let res = function(arg0.get_value_or());
            if let Some(value) = res {
                self.set_value(value);
            }
        }
    }
    /*
        pub fn lift2<U0, U1>(&mut self, arg0: &Stream<U0>, arg1: &Stream<U1>, function: fn(Option<U0>, Option<U1>) -> Option<T>)
            where U0: Clone, U1: Clone {
            if arg0.has_changed() || arg1.has_changed() {
                self.set_value(function(arg0.get_value_or(), arg1.get_value_or()));
            }
        }

        pub fn lift3<U0, U1, U2>(&mut self, arg0: &Stream<U0>, arg1: &Stream<U1>, arg2: &Stream<U2>, function: fn(Option<U0>, Option<U1>, Option<U2>) -> Option<T>)
            where U0: Clone, U1: Clone, U2: Clone {
            if arg0.has_changed() || arg1.has_changed() || arg2.has_changed() {
                self.set_value(function(arg0.get_value_or(), arg1.get_value_or(), arg2.get_value_or()));
            }
        }

        pub fn lift4<U0, U1, U2, U3>(&mut self, arg0: &Stream<U0>, arg1: &Stream<U1>, arg2: &Stream<U2>, arg3: &Stream<U3>, function: fn(Option<U0>, Option<U1>, Option<U2>, Option<U3>) -> Option<T>)
            where U0: Clone, U1: Clone, U2: Clone, U3: Clone {
            if arg0.has_changed() || arg1.has_changed() || arg2.has_changed() || arg3.has_changed() {
                self.set_value(function(arg0.get_value_or(), arg1.get_value_or(), arg2.get_value_or(), arg3.get_value_or()));
            }
        }

        pub fn lift5<U0, U1, U2, U3, U4>(&mut self, arg0: &Stream<U0>, arg1: &Stream<U1>, arg2: &Stream<U2>, arg3: &Stream<U3>, arg4: &Stream<U4>, function: fn(Option<U0>, Option<U1>, Option<U2>, Option<U3>, Option<U4>) -> Option<T>)
            where U0: Clone, U1: Clone, U2: Clone, U3: Clone, U4: Clone {
            if arg0.has_changed() || arg1.has_changed() || arg2.has_changed() || arg3.has_changed() || arg4.has_changed() {
                self.set_value(function(arg0.get_value_or(), arg1.get_value_or(), arg2.get_value_or(), arg3.get_value_or(), arg4.get_value_or()));
            }
        }

    */

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

    pub fn last<U>(&mut self, values: &Stream<T>, trigger: &Stream<U>)
        where U: Clone {
        if trigger.has_changed() {
            self.clone_value_from(values);
        }
    }

    pub fn time<U>(&mut self, arg0: &Stream<U>, timestamp: i64)
        where T: From<i64>, U: Clone {
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

pub fn default<T>(output: &mut Stream<T>, input: &Stream<T>, timestamp: i64)
    where T: Clone {
    if input.has_changed() {
        output.clone_value_from(input);
    } else if timestamp == 0 {
        output.set_value(output.get_last())
    }
}

pub fn merge<T>(output: &mut Stream<T>, streams: Vec<&Stream<T>>)
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