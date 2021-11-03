pub type StreamValue<T> where T : Clone = Option<Result<T, &'static str>>;

pub struct Stream<T> where T : Clone {
    value : StreamValue<T>,
    last : StreamValue<T>,
    // value_init : bool, // TODO None should be the same??
    // last_init : bool, // TODO None should be the same??
    // changed: bool, // TODO None should be the same??
    unknown: bool, // TODO could be Err() ??
}

impl Stream<T> {
    #[inline]
    pub const fn new() -> Stream<T> {
        Stream {
            value: None,
            last: None,
            unknown: false
        }
    }

    #[inline]
    pub const fn default(value : T) -> Stream<T> {
        Stream {
            value: Some(Ok(value)),
            last: None,
            unknown: false
        }
    }

    pub const fn merge(streams : Vec<&Stream<T>>, out : &mut Stream<T>) {
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

    pub fn has_changed(&self) {
        self.value.is_some();
    }

    pub fn set_value(&mut self, other : &Stream<T>) {
        self.changed = true;
        self.value_init = true;
        self.value = other.value.clone();
        self.unknown = other.unknown;
    }

    pub fn update_last(&mut self) {
        if self.has_changed() {
            self.changed = false;
            self.last_init = true;
            self.last = self.value.clone();
            self.value = None;
        }
    }
}