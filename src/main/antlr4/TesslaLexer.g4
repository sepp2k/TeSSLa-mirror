lexer grammar TesslaLexer;

@members {
    int nesting = 0;
}

DEF: 'def';
WHERE: 'where';
INCLUDE: 'include';
TYPE: 'type';
IN: 'in';
OUT: 'out';
PRINT: 'print';
AS: 'as';
STATIC: 'static';
IF: 'if';
THEN: 'then';
ELSE: 'else';
FUN: 'fun';
RETURN: 'return';
TRUE: 'true';
FALSE: 'false';
LBRACKET: '[';
RBRACKET: ']';
LPAR: '(';
RPAR: ')';
AT: '@';
COMMA: ',';
SEMI: ';';
DOT: '.';
DOTDOT: '..';
COLONEQ: ':=';
ROCKET: '=>';
DOLLAR_BRACE: '${';
EQ: '=';
COLON: ':';
PLUS: '+';
MINUS: '-';
TIMES: '*';
DIV: '/';
MOD: '%';
FPLUS: '+.';
FMINUS: '-.';
FTIMES: '*.';
FDIV: '/.';
AND: '&';
OR: '|';
XOR: '^';
LSHIFT: '<<';
RSHIFT: '>>';
TILDE: '~';
EQEQ: '==';
NEQ: '!=';
GEQ: '>=';
GT: '>';
LT: '<';
LEQ: '<=';
FGEQ: '>=.';
FGT: '>.';
FLT: '<.';
FLEQ: '<=.';
ANDAND: '&&';
OROR: '||';
NOT: '!';

DQUOTE: '"' -> pushMode(IN_STRING);

LBRACE: '{' {
    nesting++;
    pushMode(DEFAULT_MODE);
};

RBRACE: '}' {
    if (nesting > 0) {
        nesting--;
        popMode();
    }
};

fragment NameChar
    : NameStartChar
    | [\p{Number}]
    ;

fragment NameStartChar
    : [_\p{Alpha}\p{General_Category=Other_Letter}\p{Emoji_Presentation}]
    ;

ID: NameStartChar NameChar*;
DECINT: [\p{Nd}]+;
HEXINT: '0x'[\p{Nd}a-fA-F\uFF21-\uFF26\uFF41-\uFF46]+;
FLOAT: [\p{Nd}]+ ('.' [\p{Nd}]+ ('e' [\p{Nd}]+)? | 'e' [\p{Nd}]+);

DOCLINE: ('---' | '##') [^\n]* ('\r'? '\n')+;
// Skip the comment, but not the linebreak at the end of the comment. This way the parser will still see a
// linebreak that terminates a statement when a comment is written on the same line as a statement.
LINE_COMMENT: ('--' | '#') ~'\n'* -> skip;
ESCAPED_NEWLINE: '\\\n' -> skip;
NL: '\r'? '\n';
WS: [ \t]+ -> skip;

INVALID: .;

mode IN_STRING;

TEXT: ~[\\$"]+ ;
DOLLAR_BRACE_IN_STRING: '${' {
    nesting++;
} -> pushMode(DEFAULT_MODE), type(DOLLAR_BRACE);
DOLLAR: '$' -> pushMode(SINGLE_ID);
ESCAPE_SEQUENCE: '\\' . ;
DQUOTE_IN_STRING: '"' -> type(DQUOTE), popMode;

mode SINGLE_ID;

EMBEDDED_ID: ID -> type(ID), popMode;
