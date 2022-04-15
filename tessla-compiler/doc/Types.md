# Types of the generated monitor 

## Scala

For the interaction with a generated monitor API or interaction with Scala externs it may be useful to know how TeSSLa types are translated in the monitor.
This document contains an overview of this type translation:


| TeSSLa Type   | Scala Type                | Notes                                                     |
|---            |---                        |---                                                        |
|  Unit         | Boolean                   | always true                                               |
|  Bool         | Boolean                   |                                                           |
|  Int          | Long                      |                                                           |
|  Float        | Double                    |                                                           |
|  String       | String                    |                                                           |
|  Set          | scala.immutable.Set       |                                                           |
|  List         | scala.immutable.List      |                                                           |
|  Map          | scala.immutable.Map       |                                                           |
|  Tuple        | Scala Tuple               |                                                           |
|  Record       | Scala Tuple               | in the alpahnumeric order of the field names              |

## Rust

In Rust all types are wrapped in a `TesslaValue`. This makes it possible to pass error values on the stream:

```
#[derive(Hash)]
pub enum TesslaValue<T> {
    Error(&'static str),
    Value(T),
}
```

TesslaValue is specified together with some specializations in `value.rs` in the standard library.

| TeSSLa Type   | Rust  Type                    | Notes                                     |
|---            |-------------------------------|-------------------------------------------|
|  Unit         | TesslaValue<()>               |                                           |
|  Bool         | TesslaValue&lt;bool>          |                                           |
|  Int          | TesslaValue&lt;i64>           |                                           |
|  Float        | TesslaValue&lt;f64>           |                                           |
|  String       | TesslaValue&lt;String>        |                                           |
|  Set          | TesslaValue<im::HashSet<T>>   |                                           |
|  List         | TesslaValue<im::Vector<T>>    |                                           |
|  Map          | TesslaValue<im::HashMap<T,U>> |                                           |
|  Tuple        | TesslaValue<(T1, T2, ...)>    | Rust tuples wrapped in a TesslaValue. |
|  Record       | Rust struct                   |                                           |

For the immutable data structures the [im](https://docs.rs/im/15.0.0/im/) crate is used.
