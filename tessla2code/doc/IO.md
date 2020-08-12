Input-/Output-Format Documentation
==================================

In this document the input and output syntax of the TeSSLa compiler is specified.
Details on the notation can be found at the end of this document.

Input Syntax
----------------------------------

```
    INPUT     ::= TIMESTAMP ':' STREAM_ID ('=' VALUE)?
    VALUE     ::= UNIT | BOOL | INT | FLOAT | STRING | SET | MAP | LIST | TUPLE | RECORD
    UNIT      ::= '()'
    BOOL      ::= 'true' | 'false'
    INT       ::= [1-9][0-9]* | '0'
    FLOAT     ::= INT ('.' [0-9]+)?
    STRING    ::= '"' .* '"'
    SET       ::= 'Set(' VALUE*, ')'
    LIST      ::= 'List(' VALUE*, ')'
    MAP       ::= 'Map(' (VALUE '->' VALUE)*, ')'
    TUPLE     ::= '(' VALUE*, ')'
    RECORD    ::= '{' (FIELD_ID '=' VALUE)+, '}'

    TIMESTAMP ::= INT
```

Note:

 - STREAM_ID, FIELD_ID are defined by the IDs used in the TeSSLa specification
 - Tuples and records with field names \_1, \_2... are exclusively handled as tuples (also in the output)
 - Tuples and records with no fields are handled as empty tuples (also in the output)
 - `\\`, `\n`, `\r`, `\t`, `\"` can be used for escaping special signs inside the string in the usual way
 - Strings may not contain the `"`-character unless it is used as escape sequence `\"`
 - Between two terminal symbols there may be arbitrarily many whitespaces, except inside `FLOAT` and `INT`
 - Timestamps may not decrease
 - An input with an unknown stream ID is ignored
 - A second input to the same stream at the same timestamp overrides the previous value
 - For inputs on unit streams the part after `STREAM_ID` is fully ignored

 Example Input:

 ```
    0: a = 33
    0: b = 4.99
    0: c = "Multiline\nText"
    0: u = ()
    2: d = true
    2: e = (false, 4)
    2: u
    4: a = 12
    4: f = Set(22, 33, 55)
    4: g = Map(1 -> 3, 5 -> 7)
    4: h = List(1, 3.0)
    4: i = {a = true, b = "a b c"}}
 ```

Output Syntax
----------------------------------

```
    OUTPUT    ::= TIMESTAMP ':' OUT_NAME ('=' VALUE)?
    VALUE     ::= UNIT | BOOL | INT | FLOAT | STRING | SET | MAP | LIST | TUPLE | RECORD
    UNIT      ::= '()'
    BOOL      ::= 'true' | 'false'
    INT       ::= [1-9][0-9]* | '0'
    FLOAT     ::= INT ('.' [0-9]+)?
    STRING    ::= .*
    SET       ::= 'Set(' VALUE*, ')'
    LIST      ::= 'List(' VALUE*, ')'
    MAP       ::= 'Map(' (VALUE '->' VALUE)*, ')'
    TUPLE     ::= '(' VALUE*, ')'
    RECORD    ::= '{' (FIELD_ID '=' VALUE)+, '}'

    TIMESTAMP ::= INT
```

Note:

- The syntax of the output is equal to the input syntax except

    * Strings are not printed in quotes, and no characters are escaped
    * The name of an output (`OUT_NAME`) may be any text given by the annotation in the TeSSLa Core
    * Unit streams are always printed with value (i.e. `... = ()`)

- The output contains spaces in front and after the following terminal symbols: `':', '=', '->'` and after
the `','` of enumerations (e.g. in `SET`).

- Tuples and records with field names \_1, \_2... are exclusively handled as tuples (also in the input)
- Tuples and records with no fields are handled as empty tuples (also in the input)

Notation
----------------------------------

In this document an EBNF-like notation is used.

 - Nonterminal symbols are written in `CAPITALS`
 - Terminal symbols are written inside `'Quotes'` and have to be used sign by sign (no whitespaces in between, case-sensitive)
 - `[1-9]` means any digit between 1 and 9 `[0-9]`, any digit between 0 and 9 as terminal symbol
 - `.` is any character
 - Brackets are used for grouping
 - `X | Y` for any expression `X` and `Y` means occurence of `X` or occurence of `Y`
 - `X*` for any expression `X` means any number of occurences of `X`
 - `X+` for any expression `X` means at least one ocurence of `X`
 - `X*,` for any expression `X` is equal to `(X (',' X)*)?` (comma separation)
