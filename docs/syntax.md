# Tessla Syntax

## Grammar:

    spec          ::= statement*
    statement     ::= definition | typeDef | out | outAll | print | in
    definition    ::= TESSLADOC? annotation* "def" ID typeParamList? paramList? (":" type)? "=" expression EOS
    typeDef       ::= TESSLADOC? "type" ID typeParamList? '=' type EOS
    annotation    ::= "@" ID
    paramList     ::= "(" ID ":" type ("," ID (":" type)?)* ")"
    typeParamList ::= "[" ID ("," ID)* "]"
    out           ::= "out" expression ("as" ID)? EOS
    outAll        ::= "out" "*" EOS
    print         ::= "print" expression EOS
    in            ::= "in" ID ":" type EOS
    expression    ::= "static"? "if" expression "then" expression "else" expression
                    | expression infixOperator expression
                    | unaryOperator expression
                    | "(" expression ")"
                    | "{" definition* "return"? expression "}"
                    | expression where "{" definition* "}"
                    | "{" (memberDef ("," memberDef)*)? "}"
                    | "(" (expression ("," expression)*)? ")"
                    | expression "." ID
                    | ID typeArguments? arguments?
                    | STRING
                    | INT timeUnit?
                    | "true"
                    | "false"
    timeUnit      ::= "fs" | "ps" | "ns" | "µs" | "us" | "ms" | "s" | "min" | "h" | "d"
    memberDef     ::= ID ("=" | ":") expression
                    | ID
    arguments     ::= "(" arg ("," arg)? ")"
    typeArguments ::= "[" type ("," type)* "]"
    arg           ::= ID "=" expression
                    | expression
    type          ::= ID "[" type ("," type)* "]"
                    | ID
                    | "(" (type ("," type)*)? ")" ("=>" type)?
                    | "{" (memberType ("," memberType)*)? "}"
    memberType    ::= ID ":" type
    infixOperator ::= "<<" | ">>" | ">=" | "<=" | "<" | ">" | ">=." | "<=." | ">." | "<." |"!=" | "=="
                    | "&" | "|" | "^" | "+" | "-" | "*" | "/" | "%" | "+." | "-." | "*." | "/." | "&&" | "||"
    unaryOperator ::= "~" | "-" | "-." | "!"
    
    ID            ::= [a-zA-Z_][a-zA-Z_0-9]*
    FLOAT         ::= DECIMAL_DIGIT+ "." DECIMAL_DIGIT+ | DECIMAL_DIGIT+ ("." DECIMAL_DIGIT+) "e" ('+'|'-') DECIMAL_DIGIT+
    INT           ::= DECIMAL_DIGIT+ | "0x" HEX_DIGIT+
    EOS           ::= ";" | "\n"

* A `DECIMAL_DIGIT` is any character in the Unicode category Nd
* A `HEX_DIGIT` is a `DECIMAL_DIGIT`, an ASCII letter from the range 'a' through 'f' or 'A' through 'F', or a fixed-width version of these letters (Unicode code points FF21 through FF26 or FF41 through FF46)
* `DECIMAL_DIGIT` and `HEX_DIGIT` aren't tokens, just helper definitions used in the definitions of `FLOAT` and `INT`
* String literals start and end with ASCII double quotes. Inside a string literal any character that's not a double quote, a backslash or a dollar sign can be used to refer to themselves. A backslash can be used to espace these special characters, so `\"`, `\$` and `\\` refer to a double quote, dollar and backslash respectively. Further the escape sequences `\r`, `\n`, `\t` and `\a` can be used to refer to carriage return, newline, tabulator and the system bell (Unicode code point 0007) respectively. An unescaped dollar can either be followed by an identifier or by `"{" expression "}"`. This will cause the value of the given identifier or expression to be converted to string using the `toString` function and then inserted into the string (string interpolation).
* Newlines that are directly preceded by a backslash are ignored and do not constitute an EOS token.
* Additionally newlines can be used in the following positions without creating an EOS token:
  * Directly after an infix operator
  * Directly after an opening brace, bracket or parenthesis
  * Directly before an opening brace, bracket or parenthesis
  * Directly after a comma
  * Directly before and/or after the `then` and/or `else` keywords
  * Directly after the `=>` of a lambda expression
  * Directly after the `=` (or `:=`) of a definition
  * As part of an empty line that only consists of whitespace
* Comments start with `--` or `#` and extend to the end of the line. They are ignored by the parser.
* TesslaDoc comments are comments that start with `---` or `##`. They can only appear in front of definitions.
* Use of the keyword `return` is deprecated
* Anything that appears within quotes in the grammar, except for time units, is a keyword and can not be used as an identifier.
* The precedence of the infix operators is as follows (lowest-to-highest):
  * `||`
  * `&&`
  * `==`, `<`, `>`, `<=`, `>=`, `!=`, `>.`, `<.`, `<=.`, `>=.`
  * `|`, `^`
  * `&`
  * `<<`, `>>`
  * `+`, `-`, `+.`, `-.`
  * `*`, `/`, `%`, `*.`, `/.`
* For backwards compatibility, `${` can be used instead of `{` to start object literals or types and `:=` can be used instead of `=` in definitions


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

For any type T, `Events[T]`, `List[T]`, `Set[T]` and `Option[T]` are also types.

For any types T1, T2, `Map[T1, T2]` is also a type.

For any types T1 through Tn and distinct identifiers id1 through idn `{id1: T1, ..., idn: Tn}` is also a type.

For any types T1 through Tn `(T1, ..., Tn)` is also a type and is equivalent to `{_1: T1, ..., _n: Tn}`.

The types `()`, `{}` and `Unit` are all equivalent and represent the empty object/tuple

The primitive types, list, maps, sets and type parameters are value types (later this will be changed, so type parameters will only be value types when declared with that constraint). Object/tuple types are value types if and only if all of their member types are value types.

The type parameters to `Events`, `Option`, `List`, `Set` and `Map` must be value types (later this might be changed so data structures can contain non-value types and are only value types themselves when they contain other value types).

None of these type names are keywords and type names have their own namespace, so it is perfectly possible to also use them as the names of values or macros. For example `def Int: Int = 42` would be a valid (albeit stupid) definition.
