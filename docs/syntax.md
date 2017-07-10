# Tessla Syntax

## Grammar:

    spec ::= statement*
    statement     ::= definition | out | in
    definition    ::= ("def" | "define") ID paramList? (":" type)? ":=" expression
    paramList     ::= "(" ID (":" type)? ("," ID (":" type)?)* ")"
                    | "(" ")"
    out           ::= "out" expression ("as" ID)?
    in            ::= "in" ID ":" type
    expression    ::= "if" expression "then" expression ("else" expression)?
                    | expression ":" type
                    | expression infixOperator expression
                    | unaryOperator expression
                    | "(" expression ")"
                    | "{" statement* expression "}"
                    | ID "(" (arg ("," arg)*)? ")"
                    | ID
                    | STRING
                    | INT
                    | "()"
                    | "true"
                    | "false"
    arg           ::= ID "=" expression
                    | expression
    type          ::= ID "<" type ("," type)* ">"
                    | ID
    infixOperator ::= "%" | "<<" | ">>" | ">=" | "<=" | "<" | ">" | "!=" | "==" | "&" | "|" | "^"
                    | "+" | "-" | "*" | "/"
    unaryOperator ::= "~" | "-" | "!"

Anything that appears within quotes in the grammar is a keyword and can not be used as an identifier.

## Types

The following primitive types are pre-defined in Tessla:

 * `Int`
 * `Bool`
 * `String`
 * `Unit`

Additionally the type `Events<T>` exists for any primitive type `T` (and only for primitive types, `Events<Events<Int>>` would not be a valid type for example).

User-defined types are currently not supported, so these are all the types there are at the moment.

None of these type names are keywords and type names have their own namespace, so it is perfectly possible to also use them as the names of values or macros. For example `def Int: Int = 42` would be a valid definition.

## Builtins

The following builtins exist in Tessla:

 * `nil`
 * `default`
 * `last`
 * `const`
 * `time`
 * `delayedLast`

Again, these are not keywords.

Once the standard library is implemented, further functions will be available.
