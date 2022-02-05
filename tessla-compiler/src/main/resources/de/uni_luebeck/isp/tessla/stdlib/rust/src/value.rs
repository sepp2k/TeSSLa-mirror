use std::fmt::{Display, Formatter};
use std::hash::{Hash};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Not, Rem, Shl, Shr, Sub};
use im::HashSet;
use im::HashMap;
use im::Vector;

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

impl<T> TesslaSet<T>{
    #[inline]
    pub fn Set_empty() -> TesslaSet<TesslaValue<T>> {
        Value(HashSet::<TesslaValue<T>>::new())
    }
}

impl<T: Clone + Eq + Hash> TesslaSet<TesslaValue<T>> {
    #[inline]
    pub fn Set_add (&self, x: TesslaValue<T>) -> TesslaSet<TesslaValue<T>> {
        match self {
            Error(error) => Error(error),
            Value(value) => {
                let mut x_set: HashSet<TesslaValue<T>> = HashSet::<TesslaValue<T>>::new();
                x_set.insert(x);
                let out: TesslaSet<TesslaValue<T>> = Value(x_set.union(value.clone()));
                return out;
            },
        }
    }

    #[inline]
    pub fn Set_contains(&self, item: TesslaValue<T>) -> TesslaBool {
        match self {
            Error(error) => Error(error),
            Value(value) => return Value(value.contains(&item)),
        }
    }

    #[inline]
    pub fn Set_intersection(&self, set2: TesslaSet<TesslaValue<T>>) -> TesslaSet<TesslaValue<T>>{
        match self {
            Error(error) => Error(error),
            Value(value1) => {
                match set2 {
                    Error(error) => Error(error),
                    Value(value2) => {
                        let out: TesslaValue<HashSet<TesslaValue<T>>> = Value(value1.clone().intersection(value2.clone()));
                        return out;
                    },
                }
            },
        }
    }

    #[inline]
    pub fn Set_minus(&self, set2: TesslaSet<TesslaValue<T>>) -> TesslaSet<TesslaValue<T>> {
        match self {
            Error(error) => Error(error),
            Value(value) => {
                let out: TesslaValue<HashSet<TesslaValue<T>>> = Value(HashSet::new());
                for item in value.iter() {
                    if !set2.Set_contains(item.clone()).get_value() {
                        out.Set_add(item.clone());
                    }
                }
                return out;
            }
        }
    }

    #[inline]
    pub fn Set_remove(&self, item: TesslaValue<T>) -> TesslaSet<TesslaValue<T>> {
        match self {
            Error(error) => Error(error),
            Value(value) => {
                let out: TesslaSet<TesslaValue<T>> = Value(value.clone().without(&item));
                return out;
            },
        }
    }

    #[inline]
    pub fn Set_size(&self) -> TesslaInt{
        match self {
            Error(error) => Error(error),
            Value(value) => {
                return Value(i64::try_from(value.len()).unwrap());
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

    #[inline]
    pub fn Set_fold<U>(&self, start : TesslaValue<U>, function : fn(TesslaValue<U>, TesslaValue<T>) -> TesslaValue<U>) -> TesslaValue<U>{
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
}

// Map

pub type TesslaMap<T,U> = TesslaValue<HashMap<T,U>>;

impl<T: Display + Hash + Eq, U: Display> TesslaDisplay for HashMap<T,U> {
    fn tessla_fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Map(")?;
        let mut start = true;
        for item in self.iter() {
            if start {
                start = false;
            } else {
                f.write_str(", ")?;
            }
            write!(f, "{} -> {}", item.0, item.1)?;
        }
        f.write_str(")")
    }
}

impl<T: Clone + Eq + Hash,U: Clone + Eq + Hash> TesslaMap<TesslaValue<T>,TesslaValue<U>>{
    #[inline]
    pub fn Map_add(&self , key: TesslaValue<T>, item: TesslaValue<U>) -> TesslaMap<TesslaValue<T>,TesslaValue<U>>{
        match self{
            Error(error) => Error(error),
            Value(value) => {
                let mut x_map: HashMap<TesslaValue<T>,TesslaValue<U>> = HashMap::<TesslaValue<T>,TesslaValue<U>>::new();
                x_map.insert(key,item);
                return Value(x_map.union(value.clone()));
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
    pub fn Map_empty() -> TesslaMap<TesslaValue<T>,TesslaValue<U>>{
        let x: TesslaValue<HashMap<TesslaValue<T>, TesslaValue<U>>> = Value(HashMap::new());
        return x;
    }

    #[inline]
    pub fn Map_fold<R>(&self, start : TesslaValue<R>, function : Box<dyn Fn(TesslaValue<R>, TesslaValue<T>, TesslaValue<U>) -> TesslaValue<R>>) -> TesslaValue<R>{
        match self {
            Error(error) => Error(error),
            Value(value) => {
                let mut result: TesslaValue<R> = start;
                for item in value.keys() {
                    result = function(result, item.clone(), value.get(item).unwrap().clone());
                }
                return result;
            },
        }
    }

    #[inline]
    pub fn Map_get(&self, key: TesslaValue<T>) -> TesslaValue<U>{
        match self{
            Error(error) => Error(error),
            Value(value) => value.get(&key).unwrap().clone()
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
            Value(value) => {
                Value(i64::try_from(value.len()).unwrap())
            },
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
            Value(value) => {
                let mut x_list: Vector<TesslaValue<T>> = Vector::<TesslaValue<T>>::new();
                x_list.append(value.clone());
                x_list.insert( x_list.len(),elem);
                return Value(x_list);
            }
        }
    }

    #[inline]
    pub fn List_empty() -> TesslaList<TesslaValue<T>>{
        let x: TesslaList<TesslaValue<T>> = Value(Vector::new());
        return x;
    }

    #[inline]
    pub fn List_fold<U>(&self, start: TesslaValue<U>, function: Box<dyn Fn(TesslaValue<U>, TesslaValue<T>) -> TesslaValue<U>>) -> TesslaValue<U>{
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
        match self {
            Error(error) => Error(error),
            Value(value) => value.get(index.get_value() as usize).unwrap().clone(),
        }
    }

    #[inline]
    pub fn List_init(&self) -> TesslaList<TesslaValue<T>>{
        match self {
            Error(error) => Error(error),
            Value(value) => {
                let mut x: Vector<TesslaValue<T>> = Vector::<TesslaValue<T>>::new();
                x.append(value.clone());
                return Value(x.clone().split_at(x.clone().len()-2).0);
            },
        }
    }

    #[inline]
    pub fn List_prepend(&self, elem: TesslaValue<T>) -> TesslaList<TesslaValue<T>>{
        match self {
            Error(error) => Error(error),
            Value(value) => {
                let mut x: Vector<TesslaValue<T>> = Vector::<TesslaValue<T>>::new();
                x.append(value.clone());
                x.insert(0,elem);
                return Value(x);
            },
        }
    }

    #[inline]
    pub fn List_set(&self, index: i64, elem: TesslaValue<T>) -> TesslaList<TesslaValue<T>>{
        match self {
            Error(error) => Error(error),
            Value(value) => {
                let mut x: Vector<TesslaValue<T>> = Vector::<TesslaValue<T>>::new();
                x.append(value.clone());
                x.set(index as usize, elem);
                return Value(x);
            },
        }
    }

    #[inline]
    pub fn List_size(&self) -> TesslaInt{
        match self {
            Error(error) => Error(error),
            Value(value) => Value(i64::try_from(value.len()).unwrap()),
        }
    }

    #[inline]
    pub fn List_tail(&self) -> TesslaList<TesslaValue<T>>{
        match self {
            Error(error) => Error(error),
            Value(value) => {
                let mut x: Vector<TesslaValue<T>> = Vector::<TesslaValue<T>>::new();
                x.append(value.clone());
                return Value(x.split_off(1));
            },
        }
    }

    #[inline]
    pub fn List_map(&self, function: Box<dyn Fn(TesslaValue<T>) -> TesslaValue<T>>) -> TesslaList<TesslaValue<T>>{
        match self {
            Error(error) => Error(error),
            Value(value) => {
                let x: TesslaValue<Vector<TesslaValue<T>>> = Value(Vector::<TesslaValue<T>>::new());
                for item in value{
                    x.List_append(function(item.clone()));
                }
                return x;
            }
        }
    }
}