# Instrumenter

This module contains the instrumenter, which provides functionality to instrument C code to directly generate events within its code, which can then be used as inputs for a TeSSLa monitor.

## Annotations

To declare which parts of the code should be instrumented, TeSSLa annotations are used. For example the specification 

```ruby
@InstFunctionCalled("add")
in add: Events[Unit]

@InstFunctionCalled("sub")
in sub: Events[Unit]
```

results in every `add` and `sub` function call being instrumented and an event being produced. For a full list of available annotations refer to the standard library.

