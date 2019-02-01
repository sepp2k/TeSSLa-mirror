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
COLONEQ: ':=';
ROCKET: '=>';
DOLLAR_BRACE: '${';
EQ: '=';
COLON: ':';
PLUS: '+';
MINUS: '-';
TIMES: '*';
DIV: '/';
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
    : [_\p{Alpha}\p{General_Category=Other_Letter}\p{Emoji}]
    ;

DECINT: [\p{Nd}]+;
HEXINT: '0x'[\p{Nd}a-fA-F\uFF21-\uFF26\uFF41-\uFF46]+;
FLOAT: [\p{Nd}]+.[\p{Nd}]+;
ID: NameStartChar NameChar*;

DOCLINE: ('---' | '##') .*? ('\r'? '\n')+;
LINE_COMMENT: ('--' | '#') .*? ('\r'? '\n')+;
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
