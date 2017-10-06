# Syntax of Input Traces

## Grammar

    trace    ::= timeunit? event*
    timeunit ::= "$timeunit" "=" ("ns" | "us" | "ms" | "s" | "min" | "h" | "d")
    event    ::= INT ":" IDENTIFIER ("=" value)?
    value    ::= INT | STRING | "true" | "false" | "(" ")"
