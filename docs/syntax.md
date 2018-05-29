# Tessla Syntax

## Grammar:

    spec          ::= statement*
    statement     ::= definition | out | in
    definition    ::= annotation* ("def" | "define") ID typeParamList? paramList? (":" type)? ":=" expression
    annotation    ::= "@" ID
    paramList     ::= "(" ID ":" type ("," ID (":" type)?)* ")"
    typeParamList ::= "[" ID ("," ID)* "]"
    out           ::= "out" expression ("as" ID)?
    in            ::= "in" ID ":" type
    expression    ::= "if" expression "then" expression ("else" expression)?
                    | expression infixOperator expression
                    | unaryOperator expression
                    | "(" expression ")"
                    | "{" statement* expression "}"
                    | ID typeArguments? arguments?
                    | STRING
                    | INT
                    | "()"
                    | "true"
                    | "false"
    arguments     ::= "(" arg ("," arg)? ")"
    typeArguments ::= "[" type ("," type)* "]"
    arg           ::= ID "=" expression
                    | expression
    type          ::= ID "[" type ("," type)* "]"
                    | ID
    infixOperator ::= "<<" | ">>" | ">=" | "<=" | "<" | ">" | "!=" | "==" | "&" | "|" | "^"
                    | "+" | "-" | "*" | "/" | "&&" | "||"
    unaryOperator ::= "~" | "-" | "!"
    
    ID            ::= [a-zA-Z_][a-zA-Z_0-9]*

Anything that appears within quotes in the grammar is a keyword and can not be used as an identifier.

The precedence of the infix operators is as follows (lowest-to-highest):

 * `||`
 * `&&`
 * `==`, `<`, `>`, `<=`, `>=`, `!=`
 * `|`, `^`
 * `&`
 * `<<`, `>>`
 * `+`, `-`
 * `*`, `/`

## Annotations

Currently the only annotation is `@liftable`, which can only be applied to macro definitions whose parameter and return types must all only consist of value types (see below). `@liftable` will create a second version of the macro that is defined for `Events` instead.

It is not possible to define your own annotations.

## Types

The following primitive types are pre-defined in Tessla:

 * `Int`
 * `Bool`
 * `String`
 * `Unit`

The IDs in a type parameter list of a definition, name types during the scope of that definition.

For any types T, T1 through Tn `(T1, ..., TN) => T` is also a type.

For any type T, `Events<T>` and `Set<T>` are also types.

For any types T1, T2, `Map<T1, T2>` is also a type.

The primitive types, maps, sets and type parameters are value types (later this will be changed, so type parameters will only be value types when declared with that constraint).

The type parameters to `Events`, `Set` and `Map` must be value types (later this might be changed so sets and maps can contain non-value types and are only value types themselves when they contain other value types).

User-defined types are currently not supported.

None of these type names are keywords and type names have their own namespace, so it is perfectly possible to also use them as the names of values or macros. For example `def Int: Int = 42` would be a valid definition.

## Builtins

The following builtins exist in Tessla:

 * `nil: Events<D>`
 * `default(stream: Events<D>, value: D): Events<D>`
 * `default(stream: Events<D>, value: Events<D>): Events<D>`
 * `last(values: Events<D>, clock: Events<E>): Events<D>`
 * `const(stream: Events<D>, value: D): Events<D>`
 * `time(stream: Events<D>): Events<Int>`
 * `delayedLast(values: Events<D>, delay: Events<Int>): Events<D>`
 * `first[T1, T2](x: T1, x: T2): T1`

Again, these are not keywords.

Once the standard library is implemented, further functions will be available.
