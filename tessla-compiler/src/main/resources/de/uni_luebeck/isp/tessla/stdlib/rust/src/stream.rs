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
        value: Some(Ok(value)),
        last: None,
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

    pub fn set_value(&mut self, value: T) {
        self.value = Some(Ok(value));
        self.unknown = false;
    }

    pub fn set_unknown(&mut self, error: &'static str) {
        self.value = Some(Err(error));
        self.unknown = true;
    }

    // TODO do this first: parse input blah blah...:
    // if (get_input = stream_name) {
    //     stream.set_value
    // } else {
    //     stream.set_none
    // }

    pub fn update_last(&mut self) { // DO THIS AT THE END
        if self.has_changed() {
            self.last = self.value.clone(); // TODO use move
            self.value = None;
        }
    }

    pub fn unwrap_value(&self) -> T {
        self.value.clone().unwrap().unwrap()
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

    // TODO I think we need a single slift for each number of arguments...
    pub fn slift1<U0>(&mut self, arg0: &Stream<U0>, function: fn(U0) -> T)
        where U0: Clone {

    }
    pub fn slift2<U0, U1>(&mut self, arg0: &Stream<U0>, arg1: &Stream<U1>, function: fn(U0, U1) -> T)
        where U0: Clone, U1: Clone {
    }
}