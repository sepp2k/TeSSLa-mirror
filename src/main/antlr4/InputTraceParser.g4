parser grammar InputTraceParser;

options {
    tokenVocab = 'InputTraceLexer';
}

line: eventRange? NL? EOF;

eventRange: timeRange ':' streamName=ID ('=' expression)?;

timeRange
    : DECINT # SingleTime
    | lowerBound=DECINT lowerOp=('<'|'<=') ID (upperOp=('<'|'<=') upperBound=DECINT)? #CompRange
    | first=DECINT (',' second=DECINT)? '..' last=DECINT? #Range
    ;

expression
    : ID #Variable
    | DQUOTE stringContents* DQUOTE #StringLiteral
    | (DECINT | HEXINT) #IntLiteral
    | FLOAT #FloatLiteral
    | 'true' #True
    | 'false' #False
    | '(' (elems+=expression (',' elems+=expression)*)? lastComma=','? ')' #TupleExpression
    | ('${' | '{') (members+=memberDefinition (',' members+=memberDefinition)* ','?)? '}' #ObjectLiteral
    | 'Some' '(' expression ')' #Some
    | 'None' #None
    | 'List' '(' (elems+=expression (',' elems+=expression)*)? ','? ')' #ListExpression
    | 'Set' '(' (elems+=expression (',' elems+=expression)*)? ','? ')' #SetExpression
    | 'Map' '(' (elems+=keyVal (',' elems+=keyVal)*)? ','? ')' #MapExpression
    | op=('!' | '~' | '-') expression #UnaryExpression
    | lhs=expression op=('*' | '/' | '%') rhs=expression #InfixExpression
    | lhs=expression op=('+' | '-') rhs=expression #InfixExpression
    | lhs=expression op=('<<' | '>>') rhs=expression #InfixExpression
    | lhs=expression op='&' rhs=expression #InfixExpression
    | lhs=expression op=('|' | '^') rhs=expression #InfixExpression
    | lhs=expression op=('==' | '!=' | '<' | '>' | '<=' | '>=') rhs=expression #InfixExpression
    | lhs=expression op='&&' rhs=expression #InfixExpression
    | lhs=expression op='||' rhs=expression #InfixExpression
    | 'if' condition=expression 'then' thenCase=expression 'else' elseCase=expression #ITE
    ;

stringContents
    : TEXT
    | ESCAPE_SEQUENCE
    | '${' expression '}'
    | '$' ID
    ;

memberDefinition: ID (':'|'=') expression;

keyVal: key=expression '->' value=expression;