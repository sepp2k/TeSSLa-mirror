parser grammar TesslaSyntax;

options {
    tokenVocab = 'TesslaLexer';
}

spec: NL* includes+=include* statements+=statement* EOF;

eos: NL+ | ';' | EOF;

include: 'include' file=stringLit eos;

statement
    : def #Definition
    | tessladoc+=DOCLINE* NL* 'type' name=ID ('[' typeParameters+=ID (',' typeParameters+=ID)* ']')? (':='|'=') NL* type eos #TypeDefinition
    | 'in' ID (':' type)? eos # In
    | 'out' expression ('as' ID)? eos # Out
    | 'print' expression eos # Print
    | 'out' '*' eos # OutAll
    ;

def: header=definitionHeader (':='|'=') NL* body eos;

body: expression ('where' '{' NL* defs+=def+ '}')?;

definitionHeader:
    tessladoc+=DOCLINE* NL*
    annotations+=annotation*
    DEF name=ID
    ('[' NL* typeParameters+=ID (',' NL* typeParameters+=ID)* NL* ']')?
    ('(' NL* parameters+=param (',' NL* parameters+=param)* NL* ')')?
    (':' resultType=type)?;

annotation: '@' ID NL*;

param: ID (':' parameterType=type)?;

type
    : name=ID #SimpleType
    | name=ID '[' typeArguments+=type (',' typeArguments+=type)* ']' #TypeApplication
    | '(' parameterTypes+=type (',' parameterTypes+=type)* ')' '=>' resultType=type #FunctionType
    | ('${' | '{') NL* (memberSigs+=memberSig (',' NL* memberSigs+=memberSig)* (',' '..'?)?)? NL* '}' #ObjectType
    | '(' (elementTypes+=type (',' elementTypes+=type)*)? ')' #TupleType
    ;

memberSig: name=ID ':' type;

expression
    : ID #Variable
    | stringLit #StringLiteral
    | (DECINT | HEXINT) timeUnit=ID? #IntLiteral
    | FLOAT #FloatLiteral
    | 'true' #True
    | 'false' #False
    | '(' NL* (elems+=expression (',' NL* elems+=expression)*)? lastComma=','? NL* ')' #TupleExpression
    | '{' NL* definitions+=def+ RETURN? expression NL* '}' #Block
    | ('${' | '{') NL* (members+=memberDefinition (',' NL* members+=memberDefinition)* ','?)? NL* '}' #ObjectLiteral
    | function=expression (
        ('[' NL* typeArguments+=type (',' NL* typeArguments+=type)* NL* ']')? '(' NL* arguments+=arg (',' NL* arguments+=arg)* NL* ')'
      | ('[' NL* typeArguments+=type (',' NL* typeArguments+=type)* NL* ']')
      )  #FunctionCall
    | obj=expression '.' NL* fieldName=ID #MemberAccess
    | op=('!' | '~' | '-' | '-.') expression #UnaryExpression
    | lhs=expression op=('*' | '/' | '%' | '*.' | '/.') NL* rhs=expression #InfixExpression
    | lhs=expression op=('+' | '-' | '+.' | '-.') NL* rhs=expression #InfixExpression
    | lhs=expression op=('<<' | '>>') NL* rhs=expression #InfixExpression
    | lhs=expression op='&' NL* rhs=expression #InfixExpression
    | lhs=expression op=('|' | '^') NL* rhs=expression #InfixExpression
    | lhs=expression op=('==' | '!=' | '<' | '>' | '<=' | '>=' | '<.' | '>.' | '<=.' | '>=.') NL* rhs=expression #InfixExpression
    | lhs=expression op='&&' NL* rhs=expression #InfixExpression
    | lhs=expression op='||' NL* rhs=expression #InfixExpression
    | ifToken='if' condition=expression NL* 'then' NL* thenCase=expression NL* ('else' NL* elseCase=expression)? #ITE
    | 'static' 'if' condition=expression NL* 'then' NL* thenCase=expression NL* 'else' NL* elseCase=expression #ITE
    | funKW='fun'? openingParen='(' NL* params+=param (',' NL* params+=param)* NL* closingParen=')' '=>' NL* expression #Lambda
    ;

memberDefinition: name=ID ((':'|'=') value=expression)?;

arg: (name=ID '=')? expression;

stringLit: openingQuote=DQUOTE stringContents* closingQuote=DQUOTE;

stringContents
    : TEXT
    | ESCAPE_SEQUENCE
    | '${' expression '}'
    | '$' ID
    ;