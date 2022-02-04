use std::hash::Hash;
use std::str::FromStr;
use crate::{TesslaParse, TesslaValue, Error, Value};

fn find_end(string: &str, delim: &str, start: usize) -> usize {
    if (start + delim.len()) <= string.len() && delim == &string[start..(start + delim.len())] {
        start
    } else if start >= (string.len() - 1) {
        string.len()
    } else {
        find_end(&string, delim, match &string[start..(start + 1)] {
            "(" if delim != "\""  => find_end(&string, ")", start + 1) + 1,
            "{" if delim != "\""  => find_end(&string, "}", start + 1) + 1,
            "\""                  => find_end(&string, "\"", start + 1) + 1,
            "\\" if delim == "\"" => start + 2,
            _                     => start + 1
        })
    }
}

fn find_num_boundary(s: &str) -> usize {
    let mut boundary = 0_usize;
    for char in s.chars() {
        match char {
            '+' | '-' | '.' => {},
            _ if char.is_alphanumeric() => {},
            _ => break
        }
        boundary += 1;
    }
    boundary
}

impl TesslaParse for bool {
    fn tessla_parse(s: &str) -> (Result<Self, &'static str>, &str) {
        match s.strip_prefix("true") {
            Some(rest) => (Ok(true), rest.trim_start()),
            None => match s.strip_prefix("false") {
                Some(rest) => (Ok(false), rest.trim_start()),
                None => (Err("Failed to parse Bool from String"), s)
            }
        }
    }
}

impl TesslaParse for i64 {
    fn tessla_parse(s: &str) -> (Result<Self, &'static str>, &str) {
        match s.split_at(find_num_boundary(s)) {
            (number, rest) => (match i64::from_str(number) {
                Ok(value) => Ok(value),
                Err(_) => Err("Failed to parse Int from String"),
            }, rest)
        }
    }
}

impl TesslaParse for f64 {
    fn tessla_parse(s: &str) -> (Result<Self, &'static str>, &str) {
        match s.split_at(find_num_boundary(s)) {
            (number, rest) => (match f64::from_str(number) {
                Ok(value) => Ok(value),
                Err(_) => Err("Failed to parse Float from String"),
            }, rest)
        }
    }
}

impl TesslaParse for String {
    fn tessla_parse(string: &str) -> (Result<Self, &'static str>, &str) {
        let end = find_end(&string, "\"", 1);
        (if &string[0..1] == "\"" && &string[end..(end + 1)] == "\"" {
            Ok(string[1..end]
                .replace("\\\\n", "\n")
                .replace("\\\\r", "\r")
                .replace("\\\\t", "\t")
                .replace("\\\\\"", "\"")
                .replace("\\\\\\\\", "\\"))
        } else {
            Err("Failed to parse String value")
        }, &string[(end + 1)..])
    }
}

impl<T: TesslaParse> TesslaParse for Option<TesslaValue<T>> {
    fn tessla_parse(s: &str) -> (Result<Self, &'static str>, &str) {
        match s.strip_prefix("None") {
            Some(rest) => (Ok(None), rest.trim_start()),
            None => match s.strip_prefix("Some(") {
                Some(rest) => match T::tessla_parse(rest.trim_start()) {
                    (inner, rest) => match rest.strip_prefix(")") {
                        Some(rest) => match inner {
                            Ok(value) => (Ok(Some(Value(value))), rest.trim_start()),
                            Err(error) => (Ok(Some(Error(error))), rest.trim_start())
                        }
                        None => (Err("Failed to parse Option from String"), rest.trim_start())
                    }
                },
                None => (Err("Failed to parse Option from String"), s),
            },
        }
    }
}

impl TesslaParse for () {
    fn tessla_parse(s: &str) -> (Result<Self, &'static str>, &str) {
        match s.strip_prefix("()") {
            Some(rest) => (Ok(()), rest.trim_start()),
            None => (Err("Failed to parse Unit from String"), s)
        }
    }
}

/*
impl<T: ParseRecord> From<&str> for TesslaValue<T> {
    fn from(s: &str) -> Self {
        match s.split_once("{") {
            Some(("", rest)) => match T::parse_inner(rest) {
                (value_or_error, "}") => value_or_error,
                (_, _) => Error("Failed to parse record")
            },
            _ => Error("Failed to parse record")
        }
    }
}*/

/*impl TesslaParse for StructſXþIntſYþInt {
    fn tessla_parse(s: &str) -> (Result<Self, &'static str>, &str) where Self: Sized {
        match s.strip_prefix("{") {
            Some(rest) => {
                let mut inner = rest.trim_start();
                let mut result = StructſXþIntſYþInt {
                    x: Error("Value not assigned while parsing"),
                    y: Error("Value not assigned while parsing")
                };
                loop {
                    match inner.split_once(":") {
                        Some((lhs, rhs)) => match lhs.trim() {
                            "x" => match i64::tessla_parse(rhs.trim_start()) {
                                (Ok(value), rest) => {
                                    result.x = Value(value);
                                    match rest.trim_start().strip_prefix(",") {
                                        Some(next) => {
                                            inner = next.trim_start();
                                        }
                                        None => {
                                            inner = rest.trim_start();
                                            break;
                                        }
                                    }
                                },
                                (Err(error), rest) => {
                                    result.x = Error(error);
                                    inner = rest.trim_start();
                                }
                            },
                            "y" => match i64::tessla_parse(rhs.trim_start()) {
                                (Ok(value), rest) => {
                                    result.y = Value(value);
                                    match rest.trim_start().strip_prefix(",") {
                                        Some(next) => {
                                            inner = next.trim_start();
                                        }
                                        None => {
                                            inner = rest.trim_start();
                                            break;
                                        }
                                    }
                                },
                                (Err(error), rest) => {
                                    result.y = Error(error);
                                    inner = rest.trim_start();
                                }
                            },
                            _ => return (Err("Encountered invalid key while parsing Struct"), inner)
                        }
                        None => break
                    }
                }
                match inner.strip_prefix("}") {
                    Some(rest) => (Ok(result), rest.trim_start()),
                    None => (Err("Failed to parse Struct from String"), inner.trim_start())
                }

            },
            None => (Err("Failed to parse Struct from String"), s)
        }
    }
}*/

fn parse_set_inner<'a, T: TesslaParse + Clone + Eq + Hash>(set: &mut im::HashSet<TesslaValue<T>>, string: &'a str) -> Result<&'a str, &'static str> {
    match T::tessla_parse(string) {
        (Ok(elem), rest) => {
            set.insert(Value(elem));
            Ok(rest)
        },
        (Err(error), _) => Err(error)
    }
}

impl<T: TesslaParse + Clone + Eq + Hash> TesslaParse for im::HashSet<TesslaValue<T>> {
    fn tessla_parse(s: &str) -> (Result<Self, &'static str>, &str) where Self: Sized {
        match s.strip_prefix("Set(") {
            Some(rest) => {
                let mut set = im::HashSet::new();
                let mut inner = rest.trim_start();
                loop {
                    match parse_set_inner(&mut set, inner) {
                        Ok(rest) => match rest.trim_start().strip_prefix(",") {
                            Some(next) => inner = next.trim_start(),
                            None => {
                                inner = rest.trim_start();
                                break
                            }
                        },
                        Err(error) => return (Err(error), inner)
                    }
                }
                match inner.strip_prefix(")") {
                    Some(rest) => (Ok(set), rest.trim_start()),
                    None => (Err("Failed to parse Set from String"), rest.trim_start())
                }
            },
            None => (Err("Failed to parse Set from String"), s),
        }
    }
}

fn parse_map_inner<'a, T: TesslaParse + Clone + Eq + Hash, U: TesslaParse + Clone>(map: &mut im::HashMap<TesslaValue<T>, TesslaValue<U>>, string: &'a str) -> Result<&'a str, &'static str> {
    match T::tessla_parse(string) {
        (Ok(key), rest) => match rest.trim_start().strip_prefix("->") {
            Some(rest) => match U::tessla_parse(rest.trim_start()) {
                (Ok(value), rest) => {
                    map.insert(Value(key), Value(value));
                    Ok(rest.trim_start())
                },
                (Err(error), _) => Err(error)
            }
            None => Err("Failed to parse Map from String")
        },
        (Err(error), _) => Err(error)
    }
}

impl<T: TesslaParse + Clone + Eq + Hash, U: TesslaParse + Clone> TesslaParse for im::HashMap<TesslaValue<T>, TesslaValue<U>> {
    fn tessla_parse(s: &str) -> (Result<Self, &'static str>, &str) where Self: Sized {
        match s.strip_prefix("Map(") {
            Some(rest) => {
                let mut map = im::HashMap::new();
                let mut inner = rest.trim_start();
                loop {
                    match parse_map_inner(&mut map, inner) {
                        Ok(rest) => match rest.trim_start().strip_prefix(",") {
                            Some(next) => inner = next.trim_start(),
                            None => {
                                inner = rest.trim_start();
                                break
                            }
                        },
                        Err(error) => return (Err(error), inner)
                    }
                }
                match inner.strip_prefix(")") {
                    Some(rest) => (Ok(map), rest.trim_start()),
                    None => (Err("Failed to parse Map from String"), rest.trim_start())
                }
            },
            None => (Err("Failed to parse Map from String"), s),
        }
    }
}

fn parse_list_inner<'a, T: TesslaParse + Clone>(list: &mut im::Vector<TesslaValue<T>>, string: &'a str) -> Result<&'a str, &'static str> {
    match T::tessla_parse(string) {
        (Ok(elem), rest) => {
            list.push_back(Value(elem));
            Ok(rest)
        },
        (Err(error), _) => Err(error)
    }
}

impl<T: TesslaParse + Clone> TesslaParse for im::Vector<TesslaValue<T>> {
    fn tessla_parse(s: &str) -> (Result<Self, &'static str>, &str) where Self: Sized {
        match s.strip_prefix("List(") {
            Some(rest) => {
                let mut list = im::Vector::new();
                let mut inner = rest.trim_start();
                loop {
                    match parse_list_inner(&mut list, inner) {
                        Ok(rest) => match rest.trim_start().strip_prefix(",") {
                            Some(next) => inner = next.trim_start(),
                            None => {
                                inner = rest.trim_start();
                                break
                            }
                        },
                        Err(error) => return (Err(error), inner)
                    }
                }
                match inner.strip_prefix(")") {
                    Some(rest) => (Ok(list), rest.trim_start()),
                    None => (Err("Failed to parse List from String"), rest.trim_start())
                }
            },
            None => (Err("Failed to parse List from String"), s),
        }
    }
}