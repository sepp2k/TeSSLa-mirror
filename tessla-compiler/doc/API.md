Monitor API Documentation
==================================

## Scala

### Inputs

If a TeSSLa specification is compiled to an API monitor the interaction can be done in the following way:

For every input stream `i` the API (object `TesslaMonitor`) contains a Lambda-variable 

```
set_var_i(value, timestamp)
```

which can be called successively when a new data event arrives on the stream.
Note: The `timestamp` parameter may not be decreasing, not even between different input streams.

Further more for every output stream `o` a Lambda-variable

### Outputs

```
out_o(value, timestamp, name, error)
```

In the default setting this variable is set to `null` so it has to be overwritten before the first
output on this stream arrives by a custom function handling the output.

The `name` parameter contains the output name of the stream given by the `@name` annotation from
TeSSLa Core (which is automatically generated during compilation).

The `error` parameter is of type `Throwable`. If no error occurred the value is `null`.

Note: For outputs of expressions, e.g. `out time(a)` a new stream with auto-generated name, e.g. `$42`
is created and the callback is hence `out_$42` and can change between compiler versions and with
adjustments of the specification. It is therefore advisable to use the out statement only with
defined streams when using the API monitor. The upper example could e.g. be rewritten to:

```
def o = time(a)
out o
```

and the output then be accessed by `out_o`.

### Flushing

When an event with timestamp `t` arrives, all events up to this timestamp exclusively are calculated.
One can manually affect a calculation for the current timestamp by calling the function `flush()`
or to an arbitrary timestamp exclusively by calling `step(timestamp)`.

## Rust

The Rust interface is wholly contained in the `monitor.rs` file. Importing this file gives you all the tools necessary
to write a program that puts in events, and receives results according to the given TeSSLa specification.

The main loop of this program should do these three steps:
1. Set values for all input streams at the current timestamp `t`
2. Call `flush` or `step` to process the output for the current timestamp `t`.
This will call the appropriate output callbacks 
3. Advance to the next future timestamp, and return to 1.

For a good starting template have a look at the `main.rs` interface you can generate, and simply replace the input/output
bits with your own logic.

### Types

`TesslaValue<T>` is a Rust enum type that can either be an `Error(&str)` or a `Value(T)` depending on the state of the
stream. As such when writing an interface you need to wrap any values passed in with `Value(...)`, and make sure e.g. by
using a match expression that a result has been computed without error:
```rust
state.out_o = Some(|event: TesslaValue<T>, timestamp: i64| {
    match event {
        Error(error) => eprintln!("error: {}", error),
        Value(value) => println!("success: {}", value)
    }
});
```
You can find additional, detailed information about the types used by the Rust backend [here](Types.md).

### State

The state of all streams is stored in the `State` struct. You can create an initialized State using `get_initial_state()`:
```rust
let state = &mut get_initial_state();
```
The State struct is made up of functions to set the value of input streams, optional callback functions to receive and
handle events on output streams, and private values holding input, output and intermittent stream values.

### Inputs

For every input stream `i` the `State` struct contains a function
```rust
state.set_i: fn(TesslaValue<T>, i64, &mut State)
```
which can be used to set the value of an input stream at the current time.

Note: By default the `timestamp` parameter of this function is ignored. If you want these functions to validate the
received timestamp and possibly compute the outputs when receiving a future timestamp, you can add this check to each
`set_` function in `get_initial_state()` before the event is actually set:
```rust
if state.current_ts != ts {
    step(state, ts, false);
}
```
This way the passed timestamp will be checked, and may not be decreasing, not even between different input streams.

Note: Due to technical limitations of Rust this function is mutable, but setting it
from outside the `monitor.rs` is not supported.

### Outputs

Furthermore, for every output stream `o` the `State` struct contains an optional function
```rust
state.out_o: Option<fn(TesslaValue<T>, i64)>
```
which is called when the underlying output stream has an event at the current time.

When no I/O interface is generated this variable is initialized to `None`.

Note: For outputs of expressions, e.g. `out time(x)` the stream name is created by escaping invalid characters in the
expression, e.g. `timeu0028_xu0029_` and the callback is thus called `out_timeu0028_xu0029_`. This behaviour may change
between compiler versions or with adjustments of the specification.
It is therefore advisable to use the `out` statement only with defined or aliased streams when using the monitor API.

The example expression could for example be given an alias so that the output can then be accessed by `out_o`, as such:
```
out time(x) as o
```

### Flushing

When an event with timestamp `t` arrives, all events up to this timestamp exclusively are calculated.
One can manually affect a calculation for the current timestamp by calling the function `pub fn flush(state: &mut State)`
or to an arbitrary timestamp exclusively by calling `pub fn step(state: &mut State, new_input_ts: i64, flush: bool)`.
