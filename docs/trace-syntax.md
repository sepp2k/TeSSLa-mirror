# Syntax of Input Traces

## Grammar

    trace    ::= timeunit? event*
    timeunit ::= "$timeunit" "=" ("fs" | "ps" | "ns" | "us" | "ms" | "s" | "min" | "h" | "d")
    event    ::= INT ":" IDENTIFIER ("=" value)?
    value    ::= INT | STRING | "true" | "false" | "(" ")"

## Example

    $timeunit = "s"
    1: x
    2: x = ()
    3: y = 5
    3: z = -5
    4: b = true
    6: b = false
    6: s = "Hallo Welt"
