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

You'll find detailed information about the types used by the Rust backend [here](Types.md).

### State

The state of all streams is stored a state struct. You can acquire an initial state using `get_initial_state()`:
```
let state = &mut get_initial_state();
```
The state struct contains functions to set the value of input streams and optional callback
functions to receive output values.

### Inputs

For every input stream `i` the `state` struct contains a function
```
state.set_i: fn(TesslaValue<T>, i64, &mut State)
```

which can be called successively when a new data event arrives on the stream.
Note: The `timestamp` parameter may not be decreasing, not even between different input streams.

### Outputs

Further more for every output stream `o` the `state` struct contains a variable
```
state.out_o: Option<fn(TesslaValue<T>, i64)>
```

When no I/O interface is generated this variable is set to `None`.

Note: For outputs of expressions, e.g. `out time(x)` a new stream with auto-generated name, e.g. `timeu0028_xu0029_`
is created and the callback is hence `out_timeu0028_xu0029_` and can change between compiler versions and with
adjustments of the specification. It is therefore advisable to use the out statement only with
defined streams or aliased streams when using the API monitor. The upper example could e.g. be rewritten to:

```
out time(x) as o
```

and the output then be accessed by `out_o` as seen above.

## Flushing

When an event with timestamp `t` arrives, all events up to this timestamp exclusively are calculated.
One can manually affect a calculation for the current timestamp by calling the function `pub fn flush(state: &mut State)`
or to an arbitrary timestamp exclusively by calling `pub fn step(state: &mut State, new_input_ts: i64, flush: bool)`.

## Types

A list how TeSSLa types correspond to Scala and Rust types can be found [here](Types.md).
