# Syntax of Input Traces

## Grammar

    trace      ::= event*
    event      ::= timeRange ":" IDENTIFIER ("=" expression)?
    timeRange  ::= DECINT
                 | DECINT lessOp ID (lessOp DECINT)?
                 | DECINT ("," DECINT)? ".." DECINT?
    lessOp     ::= "<" | "<="
    expression ::= INT | FLOAT | STRING | "true" | "false" |
                 | ID
                 | "(" expression*, ")"
                 | ("{" | "${") memberDef*, "}"
                 | "Some" "(" expression ")"
                 | "None"
                 | "List" "(" (expression*, ")"
                 | "Set" "(" (expression*, ")"
                 | "Map" "(" (keyVal*, ")"
                 | prefixOp expression
                 | expression infixOp expression
                 | "if" expression "then" expression "else" expression
    keyVal     ::= expression "->" expression
    prefixOp   ::= "!" | "~" | "-"
    infixOp    ::= "*" | "/" | "%" | "+" | "-" | "<<" | ">>" | "&" | "|" | "^" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||"
    memberDef  ::= ID (":" | "=") expression

* Here the notation `*,` is used to refer to 0 or more elements separated by "," tokens.
* Tuples, lists, sets, maps and objects can have a trailing comma at the end
* The precedence of operators is the same as in the TeSSLa grammar.
* The rules for literals and identifiers are also the same as in the TeSSLa grammar.
* `DECINT` refers to an integer literal that's specifically written in decimal notation

## Example

    1: x
    2: x = ()
    3: y = 5
    3: z = -5
    4: b = true
    6: b = false
    6: s = "Hallo Welt"
