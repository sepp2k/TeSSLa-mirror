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
                    | "{" definition* "return"? expression "}"
                    | expression where "{" definition* "}"
                    | "${" (memberDef ("," memberDef)*)? "}"
                    | "(" (expression ("," expression)*)? ")"
                    | expression "." ID
                    | ID typeArguments? arguments?
                    | STRING
                    | INT
                    | "true"
                    | "false"
    memberDef     ::= annotation* ID typeParamList? paramList? (":" type)? "=" expression
                    | ID
    arguments     ::= "(" arg ("," arg)? ")"
    typeArguments ::= "[" type ("," type)* "]"
    arg           ::= ID "=" expression
                    | expression
    type          ::= ID "[" type ("," type)* "]"
                    | ID
                    | "(" (type ("," type)*)? ")" ("=>" type)?
                    | "${" (memberType ("," memberType)*)? "}"
    memberType    ::= ID ":" type
    infixOperator ::= "<<" | ">>" | ">=" | "<=" | "<" | ">" | "!=" | "==" | "&" | "|" | "^"
                    | "+" | "-" | "*" | "/" | "&&" | "||"
    unaryOperator ::= "~" | "-" | "!"
    
    ID            ::= [a-zA-Z_][a-zA-Z_0-9]*

Use of the `define` keyword over `def` is deprecated. So is using blocks without using `return` or `where` (as this can lead to ambiguities).

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

For any type T, `Events[T]` and `Set[T]` are also types.

For any types T1, T2, `Map[T1, T2]` is also a type.

For any types T1 through Tn and distinct identifiers id1 through idn `${id1: T1, ..., idn: Tn}` is also a type.

The primitive types, maps, sets and type parameters are value types (later this will be changed, so type parameters will only be value types when declared with that constraint).

The type parameters to `Events`, `Set` and `Map` must be value types (later this might be changed so sets and maps can contain non-value types and are only value types themselves when they contain other value types).

User-defined types are currently not supported.

None of these type names are keywords and type names have their own namespace, so it is perfectly possible to also use them as the names of values or macros. For example `def Int: Int = 42` would be a valid definition.

## Builtins

The following builtins exist in Tessla:

 * `nil[T]: Events[T]`
 * `default[T](stream: Events[T], value: T): Events[T]`
 * `defaultFrom[T](stream: Events[T], value: Events[T]): Events[T]`
 * `last[T1, T2](values: Events[T1], clock: Events[T2]): Events[T1]`
 * `const[T](stream: Events[T], value: T): Events[T]`
 * `time[T](stream: Events[T]): Events[Int]`
 * `delayedLast[T](values: Events[T], delays: Events[Int]): Events[T]`
 * `delay[T](delays: Events[Int], resets: Events[T]): Events[Unit]`
 * `first[T1, T2](x: T1, x: T2): T1`
 * `merge[T](stream1: Events[T], stream2: Events[T]): Events[T]`

Again, these are not keywords.

Once the standard library is implemented, further functions will be available.
