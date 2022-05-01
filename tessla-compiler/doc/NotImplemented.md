# Not implemented

## Rust

### Format strings

`String.format` calls are translated by the TeSSLa compiler into calls to 
Rust's `format!` macro. This has the advantage that we do not have to
parse and otherwise process format strings at runtime, since the format
string must already be known in Rust at compile time. At the same time,
however, this has the disadvantage that we have to follow Rust's `format!`
macro in terms of features, which at the current state doesn't implement
all the features required by the TeSSLa specification.

The following format types are currently _not implemented_:
 - `a, A`: Hexadecimal floating-point number with significand and exponent.

The following format flags are currently _not implemented_:
 - `(`: Enclose negative numbers with parenthesis.
 - `,`: Use a locale-specific grouping separator.

If you try to use one of the unimplemented functions, you will get a
`CommandNotSupportedError` at compile time, as with invalid format strings.
