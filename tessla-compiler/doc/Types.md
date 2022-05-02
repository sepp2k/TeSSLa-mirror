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
To instantiate for example an integer value, simply "call":
```rust
let int = TesslaValue::Value(42_i64);
```
or shorter, if you import the enum variants into scope:
```rust
use TesslaValue::*;

let foo: TesslaBool = Value(true);
let bar: TesslaInt = Error("Something is wrong with this value");
```

#### Note:
- The `TesslaValue` variants are imported by default, if you include `monitor.rs` or the `tessla_stdlib` crate.
- Do not mix up `TesslaValue::Error` with Rust's `Result::Err` which can be shortened to `Err` by default.
- Instantiating a `TesslaValue` (especially an `Error`) may in some contexts require specifying the type `T`,
because the compiler is unable to infer it automatically. Do this either by assigning it to a variable with explicit type,
or by using `TesslaValue::<T>::Value(...)` or `Error::<T>("...")` (known as turbofish `::<>`).
- Instead of the turbofish annotation you can also use a [type alias](https://doc.rust-lang.org/reference/items/type-aliases.html)
to access the enum variants: `TesslaUnit::Error("...")`. There's a type alias for most TeSSLa types, as defined in the table below.

### Types

| TeSSLa Type | Rust Type and Alias                                     | Notes                                                                                     |
|-------------|---------------------------------------------------------|-------------------------------------------------------------------------------------------|
| Unit        | `type TesslaUnit = TesslaValue<()>`                     |                                                                                           |
| Bool        | `type TesslaBool = TesslaValue<bool>`                   |                                                                                           |
| Int         | `type TesslaInt = TesslaValue<i64>`                     |                                                                                           |
| Float       | `type TesslaFloat = TesslaValue<f64>`                   |                                                                                           |
| String      | `type TesslaString = TesslaValue<String>`               |                                                                                           |
| Option      | `type TesslaOption<T> = TesslaValue<Option<T>>`         | Uses the Rust Option type.                                                                |
| List        | `type TesslaList<T> = TesslaValue<im::Vector<T>>`       |                                                                                           |
| Set         | `type TesslaSet<T> = TesslaValue<im::HashSet<T>>`       | The value type must implement `Hash + Eq`                                                 |
| Map         | `type TesslaMap<K, V> = TesslaValue<im::HashMap<K, V>>` | The key type must implement `Hash + Eq`                                                   |
| Tuple       | `TesslaValue<(T1, T2, ...)>`                            | Rust tuples wrapped in a TesslaValue, these have no alias. Supported for up to 12 values. |
| Record      | Rust struct                                             | Generated in `monitor.rs` as needed, also have no alias.                                  |

For the immutable data structures (List, Set, Map) the [im crate](https://docs.rs/im/15.0.0/im/) is used.

Any custom types you want to use must implement the `Clone` Trait.

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