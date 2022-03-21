use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Not, Rem, Shl, Shr, Sub};

use im::HashMap;
use im::HashSet;
use im::Vector;

use TesslaValue::*;

#[derive(Hash)]
pub enum TesslaValue<T> {
    Error(&'static str),
    Value(T),
}

impl<T> TesslaValue<T> {
    #[inline]
    pub fn is_value(&self) -> bool {
        matches!(*self, Value(_))
    }

    #[inline]
    pub fn is_error(&self) -> bool {
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

impl<T: PartialEq> Eq for TesslaValue<T> {}
impl<T: PartialEq> PartialEq<Self> for TesslaValue<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Error(_), _) | (_, Error(_)) => false,
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

// --- Number operations ---

// lhs & rhs
impl<T: BitAnd<Output = T>> BitAnd for TesslaValue<T> {
    type Output = Self;

    #[inline]
    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.bitand(rvalue))
        }
    }
}

// lhs | rhs
impl<T: BitOr<Output = T>> BitOr for TesslaValue<T> {
    type Output = Self;

    #[inline]
    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.bitor(rvalue))
        }
    }
}

// lhs ^ rhs
impl<T: BitXor<Output=T>> BitXor for TesslaValue<T> {
    type Output = Self;

    #[inline]
    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.bitxor(rvalue)),
        }
    }
}

// lhs << rhs
impl<T: Shl<Output = T>> Shl for TesslaValue<T> {
    type Output = Self;

    #[inline]
    fn shl(self, rhs: Self) -> Self::Output {
        use TesslaValue::*;
        match (self, rhs) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.shl(rvalue)),
        }
    }
}

// lhs >> rhs
impl<T: Shr<Output = T>> Shr for TesslaValue<T> {
    type Output = Self;

    #[inline]
    fn shr(self, rhs: Self) -> Self::Output {
        use TesslaValue::*;
        match (self, rhs) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.shr(rvalue)),
        }
    }
}

// lhs + rhs
impl Add for TesslaInt {
    type Output = Self;

    #[inline]
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.wrapping_add(rvalue))
        }
    }
}

impl Add for TesslaFloat {
    type Output = Self;

    #[inline]
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.add(rvalue))
        }
    }
}

// lhs - rhs
impl Sub for TesslaInt {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.wrapping_sub(rvalue))
        }
    }
}

impl Sub for TesslaFloat {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.sub(rvalue))
        }
    }
}

// lhs * rhs
impl Mul for TesslaInt {
    type Output = Self;

    #[inline]
    fn mul(self, rhs: Self) -> Self::Output {
        use TesslaValue::*;
        match (self, rhs) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (_, Value(value)) if value == 0_i64 => Error("Division by zero"),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.wrapping_mul(rvalue)),
        }
    }
}

impl Mul for TesslaFloat {
    type Output = Self;

    #[inline]
    fn mul(self, rhs: Self) -> Self::Output {
        use TesslaValue::*;
        match (self, rhs) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.mul(rvalue)),
        }
    }
}

// lhs / rhs
impl Div for TesslaInt {
    type Output = Self;

    #[inline]
    fn div(self, rhs: Self) -> Self::Output {
        use TesslaValue::*;
        match (self, rhs) {
            (Error(error), _) | (_, Error(error)) => Error(error),
            (_, Value(value)) if value == 0_i64 => Error("Division by zero"),
            (Value(lvalue), Value(rvalue)) => Value(lvalue.wrapping_div(rvalue)),
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
            (Value(lvalue), Value(rvalue)) => Value(lvalue.wrapping_rem(rvalue)),
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

#[macro_export]
macro_rules! tessla_bool {
    (if ($condition:expr) {$then:expr} else {$else:expr}) => {
        match $condition {
            $crate::TesslaValue::Value(true) => { $then },
            $crate::TesslaValue::Value(false) => { $else },
            $crate::TesslaValue::Error(error) => Error(error)
        }
    };
    (($condition1:expr) && ($condition2:expr)) => {
        match $condition1 {
            $crate::TesslaValue::Value(true) => { $condition2 },
            false_or_error => false_or_error
        }
    };
    (($condition1:expr) || ($condition2:expr)) => {
        match $condition1 {
            Value(false) => { $condition2 },
            true_or_error => true_or_error
        }
    };
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

impl From<TesslaFloat> for TesslaInt {
    #[inline]
    fn from(value: TesslaFloat) -> Self {
        match value {
            Error(error) => Error(error),
            Value(value) => Value(value as i64),
        }
    }
}

impl TesslaInt {
    #[inline]
    pub fn abs(&self) -> Self {
        match self {
            Error(error) => Error(error),
            Value(value) => Value(value.abs())
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

    #[inline]
    pub fn abs(&self) -> Self {
        match self {
            Error(error) => Error(error),
            Value(value) => Value(value.abs())
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

impl<T: ToString> TesslaValue<T> {
    #[inline]
    pub fn to_string(&self) -> TesslaString {
        match self {
            &Error(error) => Error(error),
            Value(value) => Value(value.to_string()),
        }
    }
}

impl TesslaString {
    pub fn concat(&self, other: &TesslaString) -> TesslaString {
        match (self, other) {
            (&Error(error), _) | (_, &Error(error)) => Error(error),
            (Value(lhs), Value(rhs)) => Value(lhs.to_owned() + rhs),
        }
    }

    pub fn toUpper(&self) -> Self {
        match self {
            Error(error) => Error(error),
            Value(value) => Value(value.to_uppercase())
        }
    }

    pub fn toLower(&self) -> Self {
        match self {
            Error(error) => Error(error),
            Value(value) => Value(value.to_lowercase())
        }
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

// Set

pub type TesslaSet<T> = TesslaValue<HashSet<T>>;

impl<T: Display> TesslaDisplay for HashSet<T> {
    fn tessla_fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Set(")?;
        let mut entries = Vector::from_iter(
            self.iter().map(|value| format!("{}", value)));
        entries.sort();
        for (i, entry) in entries.iter().enumerate() {
            if i > 0 {
                f.write_str(", ")?;
            }
            f.write_str(entry)?;
        }
        f.write_str(")")
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
    pub fn Set_add(&self, x: TesslaValue<T>) -> TesslaSet<TesslaValue<T>> {
        match self {
            Error(error) => Error(error),
            Value(value) => Value(value.update(x))
        }
    }

    #[inline]
    pub fn Set_contains(&self, item: TesslaValue<T>) -> TesslaBool {
        match self {
            Error(error) => Error(error),
            Value(value) => Value(value.contains(&item))
        }
    }

    #[inline]
    pub fn Set_intersection(&self, set2: TesslaSet<TesslaValue<T>>) -> TesslaSet<TesslaValue<T>>{
        match (self, set2) {
            (Error(error), _) => Error(error),
            (_, Error(error)) => Error(error),
            (Value(set_a), Value(set_b)) =>
                Value(set_a.clone().intersection(set_b))
        }
    }

    #[inline]
    pub fn Set_minus(&self, set2: TesslaSet<TesslaValue<T>>) -> TesslaSet<TesslaValue<T>> {
        match (self, set2) {
            (Error(error), _) => Error(error),
            (_, Error(error)) => Error(error),
            (Value(set_a), Value(set_b)) =>
                Value(set_a.clone().difference(set_b))
        }
    }

    #[inline]
    pub fn Set_remove(&self, item: TesslaValue<T>) -> TesslaSet<TesslaValue<T>> {
        match self {
            Error(error) => Error(error),
            Value(value) =>
                Value(value.without(&item))
        }
    }

    #[inline]
    pub fn Set_size(&self) -> TesslaInt{
        match self {
            Error(error) => Error(error),
            Value(value) => match i64::try_from(value.len()) {
                Ok(size) => Value(size),
                Err(_) => Error("Failed to convert usize to i64")
            },
        }
    }

    #[inline]
    pub fn Set_union(&self, set2 : TesslaSet<TesslaValue<T>>) -> TesslaSet<TesslaValue<T>>{
        match (self, set2) {
            (Error(error), _) => Error(error),
            (_, Error(error)) => Error(error),
            (Value(set_a), Value(set_b)) =>
                Value(set_a.clone().union(set_b))
        }
    }

    #[inline]
    pub fn Set_fold<U>(&self, start: TesslaValue<U>, function: impl Fn(TesslaValue<U>, TesslaValue<T>) -> TesslaValue<U>) -> TesslaValue<U> {
        match self {
            Error(error) => Error(error),
            Value(value) => {
                let mut result: TesslaValue<U> = start;
                for item in value.iter() {
                    result = function(result, item.clone());
                }
                return result;
            },
        }
    }

    #[inline]
    pub fn Set_map<U: Clone + Eq + Hash>(&self, function: impl Fn(TesslaValue<T>) -> TesslaValue<U>) -> TesslaSet<TesslaValue<U>>{
        match self {
            Error(error) => Error(error),
            Value(set) => {
                let result = TesslaSet::Set_empty();
                for item in set {
                    result.Set_add(function(item.clone()));
                }
                return result;
            }
        }
    }
}

// Map

pub type TesslaMap<T,U> = TesslaValue<HashMap<T,U>>;

impl<T: Display + Hash + Eq, U: Display> TesslaDisplay for HashMap<T,U> {
    fn tessla_fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Map(")?;
        let mut entries = Vector::from_iter(
            self.iter().map(|(key, value)| format!("{} -> {}", key, value)));
        entries.sort();
        for (i, entry) in entries.iter().enumerate() {
            if i > 0 {
                f.write_str(", ")?;
            }
            f.write_str(entry)?;
        }
        f.write_str(")")
    }
}

impl<T, U> TesslaMap<TesslaValue<T>, TesslaValue<U>> {
    #[inline]
    pub fn Map_empty() -> TesslaMap<TesslaValue<T>, TesslaValue<U>>{
        Value(HashMap::<TesslaValue<T>, TesslaValue<U>>::new())
    }
}

impl<T: Clone + Eq + Hash,U: Clone + Eq + Hash> TesslaMap<TesslaValue<T>,TesslaValue<U>>{
    #[inline]
    pub fn Map_add(&self , key: TesslaValue<T>, item: TesslaValue<U>) -> TesslaMap<TesslaValue<T>,TesslaValue<U>>{
        match self{
            Error(error) => Error(error),
            Value(value) => {
                let mut result: HashMap<TesslaValue<T>, TesslaValue<U>> = HashMap::new();
                result.insert(key, item);
                return Value(result.union(value.clone()));
            },
        }
    }

    #[inline]
    pub fn Map_contains(&self , key: TesslaValue<T>) -> TesslaBool{
        match self{
            Error(error) => Error(error),
            Value(value) => Value(value.contains_key(&key)),
        }
    }

    #[inline]
    pub fn Map_fold<R>(&self, start : TesslaValue<R>, function : impl Fn(TesslaValue<R>, TesslaValue<T>, TesslaValue<U>) -> TesslaValue<R>) -> TesslaValue<R>{
        match self {
            Error(error) => Error(error),
            Value(map) => {
                let mut result: TesslaValue<R> = start;
                for (key, value) in map.iter() {
                    result = function(result, key.clone(), value.clone());
                }
                return result;
            }
        }
    }

    #[inline]
    pub fn Map_get(&self, key: TesslaValue<T>) -> TesslaValue<U>{
        match self{
            Error(error) => Error(error),
            Value(map) => match map.get(&key) {
                None => Error("Map does not contain the given key"),
                Some(value) => value.clone(),
            },
        }
    }

    #[inline]
    pub fn keys(&self) -> TesslaList<TesslaValue<T>> {
        match self {
            Error(error) => Error(error),
            Value(value) => {
                let mut out: Vector<TesslaValue<T>> = Vector::new();
                for item in value.keys(){
                    out.push_back(item.clone());
                }
                return Value(out);
            }
        }
    }

    #[inline]
    pub fn Map_remove(&self, key: TesslaValue<T>) -> TesslaMap<TesslaValue<T>,TesslaValue<U>>{
        match self{
            Error(error) => Error(error),
            Value(value) => {
                Value(value.without(&key))
            }
        }
    }

    #[inline]
    pub fn Map_size(&self) -> TesslaInt{
        match self {
            Error(error) => Error(error),
            Value(value) => match i64::try_from(value.len()) {
                Ok(size) => Value(size),
                Err(_) => Error("Failed to convert usize to i64")
            },
        }
    }

    #[inline]
    pub fn Map_map<A: Clone + Eq + Hash, B: Clone>(&self, function: impl Fn(TesslaValue<T>, TesslaValue<U>) -> (TesslaValue<A>, TesslaValue<B>)) -> TesslaMap<TesslaValue<A>, TesslaValue<B>> {
        match self {
            Error(error) => Error(error),
            Value(map) => {
                let mut result: HashMap<TesslaValue<A>,TesslaValue<B>> = HashMap::new();
                for (key, value) in map.iter() {
                    let (mapped_key, mapped_value) = function(key.clone(), value.clone());
                    result.insert(mapped_key, mapped_value);
                }
                return Value(result);
            }
        }
    }
}

// List

pub type TesslaList<T> = TesslaValue<Vector<T>>;

impl<T: Display + Clone> TesslaDisplay for Vector<T> {
    fn tessla_fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("List(")?;
        let mut start = true;
        for item in self.iter() {
            if start {
                start = false;
            } else {
                f.write_str(", ")?;
            }
            write!(f, "{}", item)?;
        }
        f.write_str(")")
    }
}

impl<T: Clone> TesslaList<TesslaValue<T>>{
    #[inline]
    pub fn List_append(&self , elem: TesslaValue<T>) -> TesslaList<TesslaValue<T>>{
        match self{
            Error(error) => Error(error),
            Value(list) => {
                let mut result: Vector<TesslaValue<T>> = list.clone();
                result.push_back(elem);
                return Value(result);
            }
        }
    }

    #[inline]
    pub fn List_empty() -> TesslaList<TesslaValue<T>>{
        let x: TesslaList<TesslaValue<T>> = Value(Vector::new());
        return x;
    }

    #[inline]
    pub fn List_fold<U>(&self, start: TesslaValue<U>, function: impl Fn(TesslaValue<U>, TesslaValue<T>) -> TesslaValue<U>) -> TesslaValue<U> {
        match self {
            Error(error) => Error(error),
            Value(value) => {
                let mut result: TesslaValue<U> = start;
                for item in value {
                    result = function(result, item.clone());
                }
                return result;
            }
        }
    }

    #[inline]
    pub fn List_get(&self, index: TesslaInt) -> TesslaValue<T>{
        match (self, index) {
            (Error(error), _) => Error(error),
            (_, Error(error)) => Error(error),
            (Value(list), Value(index)) => match list.get(index as usize) {
                Some(value) => value.clone(),
                None => Error("Index out of bounds")
            },
        }
    }

    #[inline]
    pub fn List_init(&self) -> TesslaList<TesslaValue<T>>{
        match self {
            Error(error) => Error(error),
            Value(value) => {
                if value.len() > 1 {
                    Value(value.clone().slice(..(value.len() - 1)))
                } else {
                    Self::List_empty()
                }
            },
        }
    }

    #[inline]
    pub fn List_prepend(&self, elem: TesslaValue<T>) -> TesslaList<TesslaValue<T>>{
        match self {
            Error(error) => Error(error),
            Value(list) => {
                let mut result = list.clone();
                result.push_front(elem);
                return Value(result);
            }
        }
    }

    #[inline]
    pub fn List_set(&self, index: i64, elem: TesslaValue<T>) -> TesslaList<TesslaValue<T>>{
        match self {
            Error(error) => Error(error),
            Value(list) => {
                let mut result = list.clone();
                result.set(index as usize, elem);
                return Value(result);
            }
        }
    }

    #[inline]
    pub fn List_size(&self) -> TesslaInt{
        match self {
            Error(error) => Error(error),
            Value(value) => match i64::try_from(value.len()) {
                Ok(size) => Value(size),
                Err(_) => Error("Failed to convert usize to i64")
            },
        }
    }

    #[inline]
    pub fn List_tail(&self) -> TesslaList<TesslaValue<T>> {
        match self {
            Error(error) => Error(error),
            Value(value) => {
                if value.len() > 1 {
                    Value((value.clone()).split_off(1))
                } else {
                    Self::List_empty()
                }
            },
        }
    }

    #[inline]
    pub fn List_map<U: Clone>(&self, function: impl Fn(TesslaValue<T>) -> TesslaValue<U>) -> TesslaList<TesslaValue<U>>{
        match self {
            Error(error) => Error(error),
            Value(list) => {
                let mut result: Vector<TesslaValue<U>> = Vector::new();
                for item in list {
                    result.push_back(function(item.clone()));
                }
                return Value(result);
            }
        }
    }
}