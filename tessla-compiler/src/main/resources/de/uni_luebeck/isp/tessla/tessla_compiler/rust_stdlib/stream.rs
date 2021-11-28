pub type StreamValue<T> where T : Clone = Option<Result<T, &'static str>>;

pub struct Stream<T> where T : Clone {
    value : StreamValue<T>,
    last : StreamValue<T>,
    // changed: bool, // If we use this: value stores the last value, unless changed is true
    unknown: bool, // TODO could be Err() ??
}

// TODO StandardStream, LastStream, DelayStream

impl Stream<T> {
    #[inline]
    pub const fn init() -> Stream<T> {
        Stream {
            value: None,
            last: None,
            unknown: false
        }
    }

    #[inline]
    pub const fn init_with_value(value: T) -> Stream<T> {
        Stream {
            value: Some(Ok(value)),
            last: None,
            unknown: false
        }
    }

    // -- STEP FUNCTIONS --

    pub const fn default(output: &Stream<T>, input: &Stream<T>) {
        if (input.has_changed()) {
            output.set_value(input);
        }
    }

    pub const fn merge(streams: Vec<&Stream<T>>, out: &mut Stream<T>) {
        for stream in streams {
            if stream.has_changed() {
                out.set_value(&stream);
                // TODO out.unknown is true iff all changed streams are as well
                if !out.unknown {
                    return;
                }
            }
        }
    }

    // -- HELPERS --

    pub fn has_changed(&self) {
        self.value.is_some();
    }

    pub fn set_value(&mut self, other: &Stream<T>) {
        self.value = other.value.clone();
        self.unknown = other.unknown;
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
}