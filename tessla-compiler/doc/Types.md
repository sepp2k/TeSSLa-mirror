# Types of the generated monitor 

## Scala

For the interaction with a generated monitor API or interaction with Scala externs it may be useful to know how TeSSLa types are translated in the monitor.
This document contains an overview of this type translation:


| TeSSLa Type   | Scala Type                | Notes                                                     |
|---------------|---------------------------|-----------------------------------------------------------|
| Unit          | Boolean                   | always true                                               |
| Bool          | Boolean                   |                                                           |
| Int           | Long                      |                                                           |
| Float         | Double                    |                                                           |
| String        | String                    |                                                           |
| Set           | scala.immutable.Set       |                                                           |
| List          | scala.immutable.List      |                                                           |
| Map           | scala.immutable.Map       |                                                           |
| Tuple         | Scala Tuple               |                                                           |
| Record        | Scala Tuple               | in the alpahnumeric order of the field names              |

## Rust

In Rust all values used in TeSSLa streams are wrapped in the `TesslaValue` enum. This is necessary to account for
error values on streams. This enum is defined in the standard library in `value.rs` as:
```rust
pub enum TesslaValue<T> {
    Error(&'static str),
    Value(T),
}
```

### Types

| TeSSLa Type | Rust  Type                                              | Notes                                                                |
|-------------|---------------------------------------------------------|----------------------------------------------------------------------|
| Unit        | `type TesslaUnit = TesslaValue<()>`                     |                                                                      |
| Bool        | `type TesslaBool = TesslaValue<bool>`                   |                                                                      |
| Int         | `type TesslaInt = TesslaValue<i64>`                     |                                                                      |
| Float       | `type TesslaFloat = TesslaValue<f64>`                   |                                                                      |
| String      | `type TesslaString = TesslaValue<String>`               |                                                                      |
| Option      | `type TesslaOption<T> = TesslaValue<Option<T>>`         | Uses the Rust Option type.                                           |
| List        | `type TesslaList<T> = TesslaValue<im::Vector<T>>`       |                                                                      |
| Set         | `type TesslaSet<T> = TesslaValue<im::HashSet<T>>`       | The value type must implement `Hash + Eq`                            |
| Map         | `type TesslaMap<K, V> = TesslaValue<im::HashMap<K, V>>` | The key type must implement `Hash + Eq`                              |
| Tuple       | `TesslaValue<(T1, T2, ...)>`                            | Rust tuples wrapped in a TesslaValue. Supported for up to 12 values. |
| Record      | Rust struct                                             | Generated in `monitor.rs` as needed.                                 |

For the immutable data structures (List, Set, Map) the [im](https://docs.rs/im/15.0.0/im/) crate is used.

### Records

TeSSLa record types are named algorithmically by the name of their fields. A record type like
`{{a_test: Int, fieldB: Bool, _3: Option[Unit]}]` will generate as:
```rust
pub struct Struct___3_a__test_fieldB<ST___3, ST_a__test, ST_fieldB> {
    a__test: TesslaValue<ST_a__test>,
    fieldB: TesslaValue<ST_fieldB>,
    __3: TesslaValue<ST___3>,
}
```
You can see, that any underscore is escaped to two underscores, the fields (in the name) are ordered alphabetically, and
any member types are stripped and replaced with type parameters. This is necessary to allow for functions to return
records passing in something of a generic type, which can then be matched to the same struct with specific types applied.
Making each of those records a distinct type would necessitate complex conversion code.

If you know how the struct is called, and all fields are set to concrete values, initializing it does not require
explicit type arguments:
```rust
Value(Struct___3_a__test_fieldB {
    a__test: Value(5_i64),
    fieldB: Value(true),
    __3: Value(None),
})
```
It is strongly recommended using an IDE with Rust support when dealing with these structs directly. 