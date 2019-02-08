parser grammar InputTraceParser;

options {
    tokenVocab = 'InputTraceLexer';
}

// This rule should be called to skip empty lines at the beginning of the file and so
// the EOF flag is set if the file is empty (so it won't try to call the line rule on
// an empty file)
skipEmptyLines: NL* EOF?;


// Allow either a line break or the end of file to
// terminate the line. Consume multiple consecutive line breaks and possibly EOF at the end,
// so we'll always have matched the end of file after parsing the last line.
line: eventRange (NL+ EOF? | EOF);

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
    | lhs=expression op=('*' | '/' | '%' | '*.' | '/.') rhs=expression #InfixExpression
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