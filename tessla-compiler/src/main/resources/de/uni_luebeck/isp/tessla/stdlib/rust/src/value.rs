use std::fmt::{Display, Formatter};
use std::hash::{Hash};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Not, Rem, Shl, Shr, Sub};
use std::str::FromStr;
use im::HashSet;
use im::HashMap;
use im::Vector;

use crate::{find_end, find_num_boundary};

use TesslaValue::*;

#[derive(Hash, Eq)]
pub enum TesslaValue<T> {
    Error(&'static str),
    Value(T),
}

pub trait TesslaType {
    fn is_value(&self) -> bool;
    fn is_error(&self) -> bool;
}
impl<T> TesslaType for TesslaValue<T> {
    #[inline]
    fn is_value(&self) -> bool {
        matches!(*self, Value(_))
    }

    #[inline]
    fn is_error(&self) -> bool {
        matches!(*self, Error(_))
    }
}

impl<T: Copy> TesslaValue<T> {
    #[inline]
    pub fn get_value(&self) -> T {
        match self {
            &Error(error) => panic!("Expected a value, got an error: {}", error),
            Value(value) => *value,
        }
    }
}

impl<T: Clone> TesslaValue<T> {
    #[inline]
    pub fn clone_value(&self) -> T {
        match self {
            &Error(error) => panic!("Expected a value, got an error: {}", error),
            Value(value) => value.clone(),
        }
    }
}

impl<T: Clone> Clone for TesslaValue<T> {
    #[inline]
    fn clone(&self) -> Self {
        match self {
            &Error(error) => Error(error),
            Value(value) => Value(value.clone()),
        }
    }
}

impl<T: PartialEq> PartialEq<Self> for TesslaValue<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Error(error), _) | (_, Error(error)) => false,
            (Value(lvalue), Value(rvalue)) => lvalue.eq(rvalue),
        }
    }
}


pub trait TesslaDisplay {
    fn tessla_fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result;
}

impl<T: TesslaDisplay> Display for TesslaValue<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            &Error(error) => write!(f, "Error: {}", error),
            Value(value) => value.tessla_fmt(f),
        }
    }
}

pub trait TesslaParse {
    fn tessla_parse(s: &str) -> (Result<Self, &'static str>, &str) where Self: Sized;
}

impl<T: TesslaParse> From<&str> for TesslaValue<T> {
    fn from(s: &str) -> Self {
        match T::tessla_parse(s.trim()) {
            (Ok(result), "") => Value(result),
            (Ok(_), _) => Error("Failed to parse value, match not exhaustive"),
            (Err(error), _) => Error(error),
        }
    }
}

// ---------- OPERATOR TRAITS ----------

macro_rules! impl_binary_op {
    ($trait:ident, $function:ident) => {
        impl<T: $trait<Output = T>> $trait for $crate::TesslaValue<T> {
            type Output = Self;

            #[inline]
            fn $function(self, rhs: Self) -> Self::Output {
                use $crate::TesslaValue::*;
                match (self, rhs) {
                    (Error(error), _) | (_, Error(error)) => Error(error),
                    (Value(lvalue), Value(rvalue)) => Value(lvalue.$function(rvalue)),
                }
            }
        }
    }
}

impl_binary_op!(Add, add); // lhs + rhs
impl_binary_op!(BitAnd, bitand); // lhs & rhs
impl_binary_op!(BitOr, bitor); // lhs | rhs
impl_binary_op!(BitXor, bitxor); // lhs ^ rhs
impl_binary_op!(Mul, mul); // lhs * rhs
impl_binary_op!(Shl, shl); // lhs << rhs
impl_binary_op!(Shr, shr); // lhs >> rhs
impl_binary_op!(Sub, sub); // lhs - rhs

// lhs / rhs
impl Div for TesslaInt {
    type Output = Self;

    #[inline]
    fn div(self, rhs: Self) -> Self::Output {
        use TesslaValue::*;
        match (self, rhs) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (_, Value(value)) if value == 0_i64 => Error("Division by zero"),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.div(rvalue)),
        }
    }
}
impl Div for TesslaFloat {
    type Output = Self;

    #[inline]
    fn div(self, rhs: Self) -> Self::Output {
        use TesslaValue::*;
        match (self, rhs) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.div(rvalue)),
        }
    }
}

// lhs % rhs
impl Rem for TesslaInt {
    type Output = Self;

    #[inline]
    fn rem(self, rhs: Self) -> Self::Output {
        use TesslaValue::*;
        match (self, rhs) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (_, Value(value)) if value == 0_i64 => Error("Division by zero"),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.rem(rvalue)),
        }
    }
}
impl Rem for TesslaFloat {
    type Output = Self;

    #[inline]
    fn rem(self, rhs: Self) -> Self::Output {
        use TesslaValue::*;
        match (self, rhs) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.rem(rvalue)),
        }
    }
}

// -val
impl<T: Neg<Output = T>> Neg for TesslaValue<T> {
    type Output = Self;

    #[inline]
    fn neg(self) -> Self::Output {
        match self {
            Error(error) => Error(error),
            Value(value) => Value(value.neg()),
        }
    }
}

// !val
impl<T: Not<Output = T>> Not for TesslaValue<T> {
    type Output = Self;

    #[inline]
    fn not(self) -> Self::Output {
        match self {
            Error(error) => Error(error),
            Value(value) => Value(value.not()),
        }
    }
}

// 5.1 Bool

pub type TesslaBool = TesslaValue<bool>;

impl TesslaDisplay for bool {
    #[inline]
    fn tessla_fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.fmt(f)
    }
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

// 5.2 Comparison

impl<T: PartialEq> TesslaValue<T> {
    // lhs == rhs
    #[inline]
    pub fn eq(&self, other: &Self) -> TesslaBool {
        match (self, other) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.eq(rvalue)),
        }
    }

    // lhs != rhs
    #[inline]
    pub fn ne(&self, other: &Self) -> TesslaBool {
        match (self, other) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.ne(rvalue)),
        }
    }
}

impl<T: PartialOrd> TesslaValue<T> {
    // lhs < rhs
    #[inline]
    pub fn lt(&self, other: &Self) -> TesslaBool {
        match (self, other) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.lt(rvalue)),
        }
    }

    // lhs <= rhs
    #[inline]
    pub fn le(&self, other: &Self) -> TesslaBool {
        match (self, other) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.le(rvalue)),
        }
    }

    // lhs > rhs
    #[inline]
    pub fn gt(&self, other: &Self) -> TesslaBool {
        match (self, other) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.gt(rvalue)),
        }
    }

    // lhs >= rhs
    #[inline]
    pub fn ge(&self, other: &Self) -> TesslaBool {
        match (self, other) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.ge(rvalue)),
        }
    }
}

// 5.3 Integer

pub type TesslaInt = TesslaValue<i64>;

impl TesslaDisplay for i64 {
    #[inline]
    fn tessla_fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.fmt(f)
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

impl From<TesslaFloat> for TesslaInt {
    #[inline]
    fn from(value: TesslaFloat) -> Self {
        match value {
            Error(error) => Error(error),
            Value(value) => Value(value as i64),
        }
    }
}

// 5.4 Float

pub type TesslaFloat = TesslaValue<f64>;

impl TesslaDisplay for f64 {
    #[inline]
    fn tessla_fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_infinite() {
            f.write_str("Infinity")
        } else if self.is_nan() {
            f.write_str("NaN")
        } else {
            write!(f, "{:?}", self)
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

impl From<TesslaInt> for TesslaFloat {
    #[inline]
    fn from(value: TesslaInt) -> Self {
        match value {
            Error(error) => Error(error),
            Value(value) => Value(value as f64),
        }
    }
}

impl TesslaFloat {
    #[inline]
    pub fn powf(&self, exponent: TesslaFloat) -> TesslaFloat {
        match (self, exponent) {
            (&Error(error), _) | (_, Error(error)) => Error(error),
            (Value(base), Value(exponent)) => Value(base.powf(exponent)),
        }
    }

    #[inline]
    pub fn log(&self, base: TesslaFloat) -> TesslaFloat {
        match (self, base) {
            (&Error(error), _) | (_, Error(error)) => Error(error),
            (Value(value), Value(base)) => Value(value.log(base)),
        }
    }

    #[inline]
    pub fn sin(&self) -> TesslaFloat {
        match self {
            &Error(error) => Error(error),
            Value(value) => Value(value.sin()),
        }
    }

    #[inline]
    pub fn cos(&self) -> TesslaFloat {
        match self {
            &Error(error) => Error(error),
            Value(value) => Value(value.cos()),
        }
    }

    #[inline]
    pub fn tan(&self) -> TesslaFloat {
        match self {
            &Error(error) => Error(error),
            Value(value) => Value(value.tan()),
        }
    }

    #[inline]
    pub fn atan(&self) -> TesslaFloat {
        match self {
            &Error(error) => Error(error),
            Value(value) => Value(value.atan()),
        }
    }
}

// 5.5 String

pub type TesslaString = TesslaValue<String>;

impl TesslaDisplay for String {
    #[inline]
    fn tessla_fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.fmt(f)
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

impl<T: ToString> TesslaValue<T> {
    #[inline]
    pub fn to_string(&self) -> TesslaString {
        match self {
            &Error(error) => Error(error),
            Value(value) => Value(value.to_string()),
        }
    }
}

struct FormatSpec {
    flag_left_justify: bool,      // '-'
    flag_plus_sign: bool,         // '+', doesn't work for x, o
    flag_pad_sign: bool,          // ' ', doesn't work with '+'
    flag_alternate_form: bool,    // '#', doesn't work for d
    flag_zero_pad: bool,          // '0'
    flag_locale_separators: bool, // ',', not implemented
    flag_enclose_negatives: bool, // '(', doesn't work for x, o

    width: Option<usize>,
    precision: Option<usize>,

    uppercase: bool,
    format_type: char,
}

fn parse_format_string(format_string: &String) -> Result<FormatSpec, &'static str> {
    let mut spec = FormatSpec {
        flag_left_justify: false,
        flag_plus_sign: false,
        flag_pad_sign: false,
        flag_alternate_form: false,
        flag_zero_pad: false,
        flag_locale_separators: false,
        flag_enclose_negatives: false,

        width: None,
        precision: None,

        uppercase: false,
        format_type: '?',
    };

    let format_chars: Vec<char> = format_string.chars().collect();

    if format_chars[0] != '%' {
        return Err("Format string does not start with %")
    }

    let mut i = 1;

    // extract flags, each only allowed to occur once
    loop {
        match format_chars[i] {
            '-' if !spec.flag_left_justify => spec.flag_left_justify = true,
            '+' if !spec.flag_plus_sign => spec.flag_plus_sign = true,
            ' ' if !spec.flag_pad_sign => spec.flag_pad_sign = true,
            '#' if !spec.flag_alternate_form => spec.flag_alternate_form = true,
            '0' if !spec.flag_zero_pad => spec.flag_zero_pad = true,
            ',' if !spec.flag_locale_separators => spec.flag_locale_separators = true,
            '(' if !spec.flag_enclose_negatives => spec.flag_enclose_negatives = true,
            _ => break
        }
        i += 1;
    }

    // extract width, don't allow leading zeroes
    if format_chars[i] != '0' && format_chars[i].is_ascii_digit() {
        let j = i;

        while format_chars[i].is_ascii_digit() {
            i += 1;
        }

        match format_string[j..i].parse::<usize>() {
            Ok(width) => spec.width = Some(width),
            Err(_) => return Err("Failed to parse format width")
        }
    }

    // extract precision
    if format_chars[i] == '.' {
        i += 1;
        let j = i;

        while format_chars[i].is_ascii_digit() {
            i += 1;
        }

        match format_string[j..i].parse::<usize>() {
            Ok(precision) => spec.precision = Some(precision),
            Err(_) => return Err("Failed to parse format precision")
        }
    }

    spec.format_type = format_chars[i];

    if spec.format_type.is_ascii_uppercase() {
        spec.uppercase = true;
        spec.format_type = spec.format_type.to_ascii_lowercase()
    }

    if i + 1 == format_string.len() {
        Ok(spec)
    } else {
        Err("Invalid format string")
    }
}

impl TesslaString {
    pub fn concat(&self, other: &TesslaString) -> TesslaString {
        match (self, other) {
            (&Error(error), _) | (_, &Error(error)) => Error(error),
            (Value(lhs), Value(rhs)) => Value(lhs.to_owned() + rhs),
        }
    }

    #[inline]
    pub fn format<T: Display>(&self, value: &TesslaValue<T>) -> TesslaString {
        match (self, value) {
            (&Error(error), _) | (_, &Error(error)) => Error(error),
            (Value(format_string), Value(value)) => match parse_format_string(format_string) {
                Err(error) => Error(error),
                Ok(spec) => match spec.format_type {
                    's' => format_to_type_s(spec, &value),
                    _ => return Error("Invalid format type"),
                }
            }
        }
    }

    pub fn format_int(&self, value: &TesslaInt) -> TesslaString {
        match (self, value) {
            (&Error(error), _) | (_, &Error(error)) => Error(error),
            (Value(format_string), &Value(value)) => match parse_format_string(format_string) {
                Err(error) => Error(error),
                Ok(spec) => {
                    if spec.precision.is_some() {
                        return Error("Precision is not applicable for integers.");
                    }
                    if spec.flag_locale_separators {
                        return Error("Locale-specific grouping separators are not yet supported.");
                    }

                    match spec.format_type {
                        'd' => format_int_d(spec, value),
                        'o' => format_int_o(spec, value),
                        'x' => format_int_x(spec, value),
                        _ => return Error("Invalid format specifier.")
                    }
                }
            }
        }
    }

    pub fn format_float(&self, value: &TesslaFloat) -> TesslaString {

        // FIXME: Technically we don't handle NaN / Infinity correctly, since rust prints them differently

        // FIXME: alignment and '(' do not play well together

        match (self, value) {
            (&Error(error), _) | (_, &Error(error)) => Error(error),
            (Value(format_string), &Value(value)) => match parse_format_string(format_string) {
                Err(error) => Error(error),
                Ok(spec) => match spec.format_type {
                    's' => format_to_type_s(spec, &value),
                    'e' => format_to_type_e(spec, value),
                    'f' => format_to_type_f(spec, value),
                    'g' => format_to_type_g(spec, value),
                    'a' => format_to_type_a(spec, value),
                    _ => return Error("Invalid format type for float value")
                }
            }
        }
    }
}

fn format_int_o(spec: FormatSpec, value: i64) -> TesslaString {
    Value("Not implemented.".to_string())
}

fn format_int_x(spec: FormatSpec, value: i64) -> TesslaString {
    Value("Not implemented.".to_string())
}

fn format_int_d(spec: FormatSpec, value: i64) -> TesslaString {
    if spec.flag_alternate_form {
        return Error("Alternative form can't be applied to decimal integers.");
    }

    let lj = spec.flag_left_justify;
    let pl = spec.flag_plus_sign;
    let pa = spec.flag_pad_sign;
    let zp = spec.flag_zero_pad;

    // This is bäh but anything else is worse
    let formatted = if let Some(width) = spec.width {
        if value < 0_i64 && spec.flag_enclose_negatives {
            // Enclose negative numbers with () and remove the sign
            match (lj, pl, pa, zp) {
                (false, false, false, false) => format!("({0:>1$})", -1 * value, width),
                (false, false, false, true) => format!("({0:0>1$})", -1 * value, width),
                (false, false, true, false) => format!("({0:>1$})", -1 * value, width),
                (false, false, true, true) => format!("({0:0>1$})", -1 * value, width),
                (false, true, false, false) => format!("({0:+>1$})", -1 * value, width),
                (false, true, false, true) => format!("({0:0>+1$})", -1 * value, width),
                (_, true, true, _) => return Error("Sign and sign padding can't be applied at the same time."),
                (true, false, false, false) => format!("({0:<1$})", -1 * value, width),
                (true, false, false, true) => format!("({0:0<1$})", -1 * value, width),
                (true, false, true, false) => format!("({0:<1$})", -1 * value, width),
                (true, false, true, true) => format!("({0:0<1$})", -1 * value, width),
                (true, true, false, false) => format!("({0:+<1$})", -1 * value, width),
                (true, true, false, true) => format!("({0:0<+1$})", -1 * value, width)
            }
        } else if value < 0_i64 {
            match (lj, pl, pa, zp) {
                (false, false, false, false) => format!("{0:>1$}", value, width),
                (false, false, false, true) => format!("{0:0>1$}", value, width),
                (false, false, true, false) => format!("{0:>1$}", value, width),
                (false, false, true, true) => format!("{0:0>1$}", value, width),
                (false, true, false, false) => format!("{0:+>1$}", value, width),
                (false, true, false, true) => format!("{0:0>+1$}", value, width),
                (_, true, true, _) => return Error("Sign and sign padding can't be applied at the same time."),
                (true, false, false, false) => format!("{0:<1$}", value, width),
                (true, false, false, true) => format!("{0:0<1$}", value, width),
                (true, false, true, false) => format!("{0:<1$}", value, width),
                (true, false, true, true) => format!("{0:0<1$}", value, width),
                (true, true, false, false) => format!("{0:+<1$}", value, width),
                (true, true, false, true) => format!("{0:0<+1$}", value, width),
            }
        } else /* value >= 0 */ {
            match (lj, pl, pa, zp) {
                (false, false, false, false) => format!("{0:>1$}", value, width),
                (false, false, false, true) => format!("{0:0>1$}", value, width),
                (false, false, true, false) => format!(" {0:>1$}", value, width),
                (false, false, true, true) => format!(" {0:0>1$}", value, width),
                (false, true, false, false) => format!("{0:+>1$}", value, width),
                (false, true, false, true) => format!("{0:0>+1$}", value, width),
                (_, true, true, _) => return Error("Sign and sign padding can't be applied at the same time."),
                (true, _, _, _) => return Error("Can't format left justified without format width specified.")
            }
        }
    } else /* spec.width is none */ {
        if value < 0_i64 && spec.flag_enclose_negatives {
            // Enclose negative numbers with () and remove the sign
            match (lj, pl, pa, zp) {
                (false, false, false, false) => format!("({0:>})", -1 * value),
                (false, false, true, false) => format!("({0:>})", -1 * value),
                (false, true, false, false) => format!("({0:+>})", -1 * value),
                (_, _, _, true) => return Error("Can't pad with zeroes without format width specified."),
                (_, true, true, _) => return Error("Sign and sign padding can't be applied at the same time."),
                (true, _, _, _) => return Error("Can't format left justified without format width specified.")
            }
        } else if value < 0_i64 {
            match (lj, pl, pa, zp) {
                (false, false, false, false) => format!("{0:>}", value),
                (false, false, true, false) => format!("{0:>}", value),
                (false, true, false, false) => format!("{0:+>}", value),
                (_, _, _, true) => return Error("Can't pad with zeroes without format width specified."),
                (_, true, true, _) => return Error("Sign and sign padding can't be applied at the same time."),
                (true, _, _, _) => return Error("Can't format left justified without format width specified.")
            }
        } else {
            match (lj, pl, pa, zp) {
                (false, false, false, false) => format!("{0:>}", value),
                (false, false, true, false) => format!(" {0:>}", value),
                (false, true, false, false) => format!("{0:+>}", value),
                (_, _, _, true) => return Error("Can't pad with zeroes without format width specified."),
                (_, true, true, _) => return Error("Sign and sign padding can't be applied at the same time."),
                (true, _, _, _) => return Error("Can't format left justified without format width specified.")
            }
        }
    };

    if spec.uppercase {
        Value(formatted.to_uppercase())
    } else {
        Value(formatted)
    }
}

fn format_to_type_s<T: Display>(spec: FormatSpec, value: &T) -> TesslaString {
    if spec.flag_alternate_form || spec.flag_plus_sign || spec.flag_pad_sign || spec.flag_zero_pad || spec.flag_locale_separators || spec.flag_enclose_negatives {
        return Error("Invalid format flags specified for %s")
    }

    let formatted = match (spec.flag_left_justify, spec.width, spec.precision) {
        (true, Some(width), Some(precision)) => format!("{0:<1$.2$}", value, width, precision),
        (true, Some(width), None) => format!("{0:<1$}", value, width),
        (true, None, _) => return Error("Failed to format left justify, no specified width"),
        (false, Some(width), Some(precision)) => format!("{0:>1$.2$}", value, width, precision),
        (false, Some(width), None) => format!("{0:>1$}", value, width),
        (false, None, Some(precision)) => format!("{0:.1$}", value, precision),
        (false, None, None) => format!("{}", value),
    };

    if spec.uppercase {
        Value(formatted.to_uppercase())
    } else {
        Value(formatted)
    }
}

fn format_to_type_e(spec: FormatSpec, value: f64) -> TesslaString {
    if spec.flag_locale_separators {
        return Error("Invalid format flag ',' specified for %e")
    }
    // alternate form (#) seems to be ignored by java for %e

    let precision = spec.precision.unwrap_or(6);

    let formatted = match (spec.flag_plus_sign, spec.flag_pad_sign, spec.flag_enclose_negatives,
                           spec.width, spec.flag_left_justify, spec.flag_zero_pad) {
        (true, true, _, _, _, _) => return Error("Invalid format flag combination ' +' for %e"),
        (_, _, _, None, true, _) | (_, _, _, None, _, true) => return Error("Missing format width for %e"),

        (_, true, _, None, false, _) if value >= 0_f64 => format!(" {0:.1$e}", value, precision),
        (_, _, true, None, false, _) if value < 0_f64 => format!("({0:.1$e})", -value, precision),
        (true, _, _, None, false, _) => format!("{0:+.1$e}", value, precision),
        (_, _, _, None, false, _) => format!("{0:.1$e}", value, precision),

        (_, true, _, Some(width), _, true) if value >= 0_f64 => format!(" {0:01$.2$e}", value, width, precision),
        (_, _, true, Some(width), _, true) if value < 0_f64 => format!("({0:01$.2$e})", -value, width, precision),
        (true, _, _, Some(width), _, true) => format!("{0:+01$.2$e}", value, width, precision),
        (_, _, _, Some(width), _, true) => format!("{0:01$.2$e}", value, width, precision),

        (_, true, _, Some(width), true, false) if value >= 0_f64 => format!(" {0:<1$.2$e}", value, width, precision),
        (_, _, true, Some(width), true, false) if value < 0_f64 => format!("({0:<1$.2$e})", -value, width, precision),
        (true, _, _, Some(width), true, false) => format!("{0:+<1$.2$e}", value, width, precision),
        (_, _, _, Some(width), true, false) => format!("{0:<1$.2$e}", value, width, precision),

        (_, true, _, Some(width), false, false) if value >= 0_f64 => format!(" {0:>1$.2$e}", value, width, precision),
        (_, _, true, Some(width), false, false) if value < 0_f64 => format!("({0:>1$.2$e})", -value, width, precision),
        (true, _, _, Some(width), false, false) => format!("{0:+>1$.2$e}", value, width, precision),
        (_, _, _, Some(width), false, false) => format!("{0:>1$.2$e}", value, width, precision),
    };

    match formatted.rsplit_once('e') {
        None => return Error("Failed to format float with %e"),
        Some((value, exp)) => match (i64::from_str(exp), spec.uppercase) {
            (Err(_), _) => Error("Failed to parse exponent while formatting %e"),
            (Ok(exp), true) => Value(format!("{}E{:+03}", value, exp)),
            (Ok(exp), false) => Value(format!("{}e{:+03}", value, exp)),
        },
    }
}

fn format_to_type_f(spec: FormatSpec, value: f64) -> TesslaString {
    if spec.uppercase {
        return Error("Invalid format type %F")
    }
    // alternate form (#) seems to be ignored by java for %f

    let precision = spec.precision.unwrap_or(6);

    // FIXME: support locale separators (,): https://crates.io/crates/num-format

    let formatted = match (spec.flag_plus_sign, spec.flag_pad_sign, spec.flag_enclose_negatives,
                           spec.width, spec.flag_left_justify, spec.flag_zero_pad) {
        (true, true, _, _, _, _) => return Error("Invalid format flag combination ' +' for %f"),

        (_, true, _, None, _, _) if value >= 0_f64 => format!(" {0:.1$}", value, precision),
        (_, _, true, None, _, _) if value < 0_f64 => format!("({0:.1$})", -value, precision),
        (true, _, _, None, _, _) => format!("{0:+.1$}", value, precision),
        (_, _, _, None, _, _) => format!("{0:.1$}", value, precision),

        (_, true, _, Some(width), _, true) if value >= 0_f64 => format!(" {0:01$.2$}", value, width, precision),
        (_, _, true, Some(width), _, true) if value < 0_f64 => format!("({0:01$.2$})", -value, width, precision),
        (true, _, _, Some(width), _, true) => format!("{0:+01$.2$}", value, width, precision),
        (_, _, _, Some(width), _, true) => format!("{0:01$.2$}", value, width, precision),

        (_, true, _, Some(width), true, false) if value >= 0_f64 => format!(" {0:<1$.2$}", value, width, precision),
        (_, _, true, Some(width), true, false) if value < 0_f64 => format!("({0:<1$.2$})", -value, width, precision),
        (true, _, _, Some(width), true, false) => format!("{0:+<1$.2$}", value, width, precision),
        (_, _, _, Some(width), true, false) => format!("{0:<1$.2$}", value, width, precision),

        (_, true, _, Some(width), false, false) if value >= 0_f64 => format!(" {0:>1$.2$}", value, width, precision),
        (_, _, true, Some(width), false, false) if value < 0_f64 => format!("({0:>1$.2$})", -value, width, precision),
        (true, _, _, Some(width), false, false) => format!("{0:+>1$.2$}", value, width, precision),
        (_, _, _, Some(width), false, false) => format!("{0:>1$.2$}", value, width, precision),
    };

    Value(formatted)
}

fn format_to_type_g(spec: FormatSpec, value: f64) -> TesslaString {
    if spec.flag_alternate_form {
        return Error("Invalid format flag '#' specified for %g")
    }

    let mut modified_spec = FormatSpec { ..spec };
    if spec.precision.is_none() {
        modified_spec.precision = Some(6);
    } else if spec.precision.unwrap() == 0 {
        modified_spec.precision = Some(1);
    }

    let precision = match i32::try_from(modified_spec.precision.unwrap()) {
        Err(_) => return Error("Failed to cast precision to i32 without overflow"),
        Ok(value) => value
    };
    if 10e-4_f64 <= value && value < 10_f64.powi(precision) {
        format_to_type_f(modified_spec, value)
    } else {
        format_to_type_e(modified_spec, value)
    }
}

fn format_to_type_a(spec: FormatSpec, value: f64) -> TesslaString {
    if spec.flag_locale_separators || spec.flag_enclose_negatives {
        return Error("Invalid format flags specified for %a");
    }

    let formatted = if value == f64::NAN {
        "NaN".to_string()
    } else if value == f64::INFINITY {
        "Infinity".to_string()
    } else {
        // Extract float features, dec2flt/float.rs
        let bits = value.to_bits();
        let sign: i8 = if bits >> 63 == 0 { 1 } else { -1 };
        let mut exponent: i16 = ((bits >> 52) & 0x7ff) as i16;
        let mantissa = if exponent == 0 {
            (bits & 0xfffffffffffff) << 1
        } else {
            (bits & 0xfffffffffffff) | 0x10000000000000
        };
        // Exponent bias + mantissa shift
        exponent -= 1023 + 52;

        if value.is_normal() {
            if sign > 0 {
                format!("0x1.{:x}p{:x}", mantissa, exponent)
            } else {
                format!("-0x1.{:x}p{:x}", mantissa, exponent)
            }
        } else {
            if sign > 0 {
                format!("0x0.{:x}p{:x}", mantissa, exponent)
            } else {
                format!("-0x0.{:x}p{:x}", mantissa, exponent)
            }
        }
    };

    if spec.uppercase {
        Value(formatted.to_uppercase())
    } else {
        Value(formatted)
    }
}

// 5.6 Option

pub type TesslaOption<T> = TesslaValue<Option<T>>;

impl<T: Display> TesslaDisplay for Option<T> {
    #[inline]
    fn tessla_fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Some(value) => write!(f, "Some({})", value),
            None => f.write_str("None"),
        }
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

impl<T> TesslaOption<T> {
    #[inline]
    pub fn is_none(&self) -> TesslaBool {
        match self {
            &Error(error) => Error(error),
            Value(value) => Value(value.is_none()),
        }
    }

    #[inline]
    pub fn is_some(&self) -> TesslaBool {
        match self {
            &Error(error) => Error(error),
            Value(value) => Value(value.is_some()),
        }
    }
}

impl<T: Clone> TesslaOption<TesslaValue<T>> {
    #[inline]
    pub fn get_some(&self) -> TesslaValue<T> {
        match self {
            &Error(error) => Error(error),
            Value(Some(value)) => value.clone(),
            Value(None) => Error("Tried to getSome(None)"),
        }
    }

    #[inline]
    pub fn get_some_or_else(&self, fallback: TesslaValue<T>) -> TesslaValue<T> {
        match self {
            &Error(error) => Error(error),
            Value(Some(value)) => value.clone(),
            Value(None) => fallback,
        }
    }
}

// Unit

pub type TesslaUnit = TesslaValue<()>;

impl TesslaDisplay for () {
    #[inline]
    fn tessla_fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("()")
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


pub type TesslaSet<T> = TesslaValue<HashSet<T>>;

impl<T: Display> TesslaDisplay for HashSet<T> {
    fn tessla_fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("TODO")
    }
}


impl<T> TesslaSet<T>{

    #[inline]
    pub fn Set_empty() -> TesslaSet<TesslaValue<T>> {
        Value(HashSet::<TesslaValue<T>>::new())
    }
}



impl<T: Clone + Eq + Hash> TesslaSet<TesslaValue<T>> {
    #[inline]
    pub fn Set_add (&mut self, x: TesslaValue<T>) -> TesslaSet<TesslaValue<T>> {
        match self {
            Error(error) => Error(error),
            Value(value) => {
                value.insert(x);
                return self.clone();
            },
        }
    }

    #[inline]
    pub fn Set_contains(&self, item: TesslaValue<T>) -> TesslaBool {
        match self {
            &Error(error) => Error(error),
            Value(value) => return Value(value.contains(&item)),
        }
    }



    /*
    #[inline]
    pub fn Set_intersection(set1: TesslaSet<T>, set2: TesslaSet<T>) -> TesslaSet<T>{
        match set1 {
            &Error(error) => Error(error),
            Some(value1) => {
                match set2 {
                    &Error(error) => Error(error),
                    Some(value2) => {
                        let out: TesslaValue<HashSet<T>> = Value(value1.intersection(value2));
                        return out;
                    },
                }
            },
        }
    }

    #[inline]
    pub fn Set_minus(set1: TesslaSet<T>, set2: TesslaSet<T>) -> TesslaSet<T> {
        let out: TesslaValue<HashSet<T>> = Value(HashSet::new());
        for item in set1 {
            if !set2.contains(item) {
                out.add(item);
            }
        }
        return out;
    }
*/
    #[inline]
    pub fn Set_remove(&mut self, item: TesslaValue<T>) -> TesslaSet<TesslaValue<T>> {
        match self {
            Error(error) => Error(error),
            Value(value) => {
                value.remove(&item);
                return self.clone();
            },
        }
    }

    #[inline]
    pub fn Set_size(&self) -> TesslaInt{
        match self {
            &Error(error) => Error(error),
            Value(value) => {
                return Value(value.len() as i64);
            },
        }
    }

    #[inline]
    pub fn Set_union(&self, set2 : TesslaSet<TesslaValue<T>>) -> TesslaSet<TesslaValue<T>>{
        match (self, set2) {
            (Error(error), _) => Error(error),
            (_, Error(error)) => Error(error),
            (Value(value1), Value(value2)) => {
                return Value(value1.clone().union(value2));
            }
        }
    }
}
/*
    #[inline]
    pub fn fold<U>(&self, start : U, function : fn(TesslaValue<U>, TesslaValue<T>) -> TesslaValue<U>) -> TesslaValue<U>{
        match self {
            &Error(error) => Error(error),
            Value(Some(value)) => {
                let result: U = start;
                for item in value {
                    result = function(result,item);
                }
                return result;
            },
        }
    }

}

pub type TesslaMap<T,U> = TesslaValue<HashMap<T,U>>;

impl<T,U> TesslaMap<TesslaValue<T>,TesslaValue<U>>{

    #[inline]
    pub fn add(&self , key: T, item: U) -> TesslaMap<T,U>{
        match self{
            &Error(error) => Error(error),
            Value(Some(value)) => {
                value.insert(key,item);
                return self;
            },
        }
    }

    #[inline]
    pub fn contains(&self , key: T) -> bool{
        match self{
            &Error(error) => Error(error),
            Value(Some(value)) => value.containsa_key(key),
        }
    }

    #[inline]
    pub fn empty() -> TesslaMap<T,U>{
        let x: TesslaValue<HashMap<T>> = Value(HashMap::new());
        return x;
    }

    #[inline]
    pub fn fold<R>(&self, start : R, function : fn(TesslaValue<R>, TesslaValue<T>, TesslaValue<U>) -> TesslaValue<R>) -> TesslaValue<R>{
        match self {
            &Error(error) => Error(error),
            Value(Some(value)) => {
                let result: R = start;
                for item in value {
                    result = function(result,item,value.get(item));
                }
                return result;
            },
        }
    }

    #[inline]
    pub fn get(&self, key: T) -> U{
        match self{
            &Error(error) => Error(error),
            Value(Some(value)) => value.get(key)
        }
    }

    #[inline]
    pub fn keys() -> TesslaList<T>{
        todo!()
    }

    #[inline]
    pub fn remove(&self, key: T) -> TesslaMap<T,U>{
        match self{
            &Error(error) => Error(error),
            Value(Some(value)) => {
                value.remove(key);
                return self;
            }
        }
    }

    #[inline]
    pub fn size(&self) -> TesslaInt{
        match self {
            &Error(error) => Error(error),
            Value(Some(value)) => {
                let size: i64 = 0;
                for x in value {
                    size+=1;
                }
                return size;
            },
        }
    }
}

pub type TesslaList<T> = TesslaValue<Vector<T>>;

impl<T> TesslaList<TesslaValue<T>>{

    #[inline]
    pub fn append(&self , elem: T) -> TesslaList<T>{
        match self{
            &Error(error) => Error(error),
            Value(Some(value)) => {
                value.insert(value.len()-1,elem);
                return self;
            }
        }
    }

    #[inline]
    pub fn empty() -> TesslaList<T>{
        let x: TesslaList<Vector<T>> = Value(Vector::new());
        return x;
    }

    #[inline]
    pub fn fold<U>(&self, start: U, function: fn(TesslaValue<U>, TesslaValue<T>) -> U) -> U{
        match self {
            &Error(error) => Error(error),
            Value(Some(value)) => {
                let result: U = start;
                for item in value {
                    result = function(result,item);
                }
                return result;
            },
        }
    }

    #[inline]
    pub fn get(&self, index: i64) -> T{
        match self {
            &Error(error) => Error(error),
            Value(Some(value)) => value.get(index),
        }
    }

    #[inline]
    pub fn init(&self) -> TesslaList<T>{
        match self {
            &Error(error) => Error(error),
            Value(Some(value)) => {
                let x = value.clone();
                return x.slice(x.len()-2);
            },
        }
    }

    #[inline]
    pub fn prepend(elem: T, list: TesslaList<T>) -> TesslaList<T>{
        match list {
            &Error(error) => Error(error),
            Value(Some(value)) => {
                value.insert(0,elem);
                return list;
            },
        }
    }

    #[inline]
    pub fn set(&self, index: i64, elem: T) -> TesslaList<T>{
        match self {
            &Error(error) => Error(error),
            Value(Some(value)) => {
                value.set(index,elem);
                return self;
            },
        }
    }

    #[inline]
    pub fn size(&self) -> i64{
        match self {
            &Error(error) => Error(error),
            Value(Some(value)) => value.len(),
        }
    }

    #[inline]
    pub fn tail(&self) -> TesslaList<T>{
        match self {
            &Error(error) => Error(error),
            Value(Some(value)) => {
                let x = value.clone();
                return x.split_off(1);
            },
        }
    }
}*/