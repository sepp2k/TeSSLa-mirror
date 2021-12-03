use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::ops::{Add, BitAnd, BitOr, BitXor, Deref, Div, Mul, Neg, Not, Rem, Shl, Shr, Sub};
use std::str::FromStr;

use TesslaValue::*;

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
    fn clone(&self) -> Self {
        match self {
            &Error(error) => Error(error),
            Value(value) => Value(value.clone()),
        }
    }
}

impl<T: FromStr> FromStr for TesslaValue<T> {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match T::from_str(s) {
            Err(_) => Ok(Error("Failed to parse value from String")),
            Ok(value) => Ok(Value(value)),
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
impl<T: Div<Output = T> + PartialEq<i64>> Div for TesslaValue<T> {
    type Output = Self;

    #[inline]
    fn div(self, rhs: Self) -> Self::Output {
        use TesslaValue::*;
        match (self, rhs) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (_, Value(value)) if value == 0 => Error("Division by zero"),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.div(rvalue)),
        }
    }
}

// lhs % rhs
impl<T: Rem<Output = T> + PartialEq<i64>> Rem for TesslaValue<T> {
    type Output = Self;

    #[inline]
    fn rem(self, rhs: Self) -> Self::Output {
        use TesslaValue::*;
        match (self, rhs) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (_, Value(value)) if value == 0 => Error("Division by zero"),
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

impl Deref for TesslaBool {
    type Target = bool;

    #[inline]
    fn deref(&self) -> &Self::Target {
        match &self {
            &Error(error) => panic!("Deref to bool failed, was error: {}", error),
            Value(value) => value,
        }
    }
}

impl TesslaBool {
    #[inline]
    pub fn then_else<T, F>(&self, if_true: /* lazy */ F, if_false: /* lazy */ F) -> TesslaValue<T>
    where F: FnOnce() -> TesslaValue<T> {
        match self {
            &Error(error) => Error(error),
            &Value(value) => if value { if_true() } else { if_false() },
        }
    }

    #[inline]
    pub fn and<F>(&self, rhs: /* lazy */ F) -> TesslaBool
    where F: FnOnce() -> TesslaBool {
        match self {
            &Error(error) => Error(error),
            &Value(value) => if value { rhs() } else { Value(false) },
        }
    }

    #[inline]
    pub fn or<F>(&self, rhs: /* lazy */ F) -> TesslaBool
    where F: FnOnce() -> TesslaBool {
        match self {
            &Error(error) => Error(error),
            &Value(value) => if value { Value(true) } else { rhs() },
        }
    }

}

// 5.2 Comparison

// lhs == rhs, lhs != rhs
impl<T: PartialEq> PartialEq for TesslaValue<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Error(_), _) | (_, Error(_)) => true, // TODO is this correct?
            (Value(lvalue), Value(rvalue)) => lvalue.eq(rvalue),
        }
    }
}

// lhs < rhs, lhs <= rhs, lhs >= rhs, lhs > rhs
impl<T: PartialOrd> PartialOrd for TesslaValue<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Error(_), _) | (_, Error(_)) => None, // TODO is this correct?
            (Value(lvalue), Value(rvalue)) => lvalue.partial_cmp(rvalue),
        }
    }
}

// 5.3 Integer

pub type TesslaInt = TesslaValue<i64>;

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

impl Deref for TesslaString {
    type Target = String;

    #[inline]
    fn deref(&self) -> &Self::Target {
        match &self {
            &Error(error) => panic!("Deref to String failed, was error: {}", error),
            Value(value) => &value,
        }
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

impl<T: Display> Display for TesslaValue<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            &Error(error) => write!(f, "Error: {}", error),
            Value(value) => value.fmt(f),
        }
    }
}

impl TesslaString {
    #[inline]
    pub fn format<T: Display>(&self, value: &TesslaValue<T>) -> TesslaString {
        match (self, value) {
            (&Error(error), _) | (_, &Error(error)) => Error(error),
            (Value(format_string), Value(value)) =>
                todo!("{}.format({}) /* See Section 4.13.3.1 */", format_string, value),
        }
    }
}

// 5.6 Option

pub type TesslaOption<T> = TesslaValue<Option<T>>;

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

impl<T: Clone> TesslaOption<T> {
    #[inline]
    pub fn get_some(&self) -> TesslaValue<T> {
        match self {
            &Error(error) => Error(error),
            Value(Some(value)) => Value(value.clone()),
            Value(None) => Error("Tried to getSome(None)"),
        }
    }

    #[inline]
    pub fn get_some_or_else(&self, fallback: T) -> TesslaValue<T> {
        match self {
            &Error(error) => Error(error),
            Value(Some(value)) => Value(value.clone()),
            Value(None) => Value(fallback),
        }
    }
}

pub type TesslaUnit = TesslaValue<()>;
