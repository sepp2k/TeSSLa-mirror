# Syntax of Input Traces

## Grammar

    trace    ::= timeunit? event*
    timeunit ::= "$timeunit" "=" ("ns" | "us" | "ms" | "s" | "min" | "h" | "d")
    event    ::= INT ":" IDENTIFIER ("=" value)?
    value    ::= INT | STRING | "true" | "false" | "(" ")"

## Example

    0: x
    1: x = ()
    2: y = 5
    2: z = -5
    3: b = true
    5: b = false
    5: s = "Hallo Welt"
