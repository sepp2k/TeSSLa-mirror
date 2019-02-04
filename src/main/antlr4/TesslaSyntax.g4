parser grammar TesslaSyntax;

options {
    tokenVocab = 'TesslaLexer';
}

spec: nl* includes+=include* statements+=statement* EOF;

nl: NL | LINE_COMMENT;

eos: nl+ | ';' | EOF;

include: 'include' file=stringLit eos;

statement
    : def #Definition
    | 'type' name=ID ('[' typeParameters+=ID (',' typeParameters+=ID)* ']')? (':='|'=') nl* type eos #TypeDefinition
    | 'in' ID (':' type)? eos # In
    | 'out' expression ('as' ID)? eos # Out
    | 'out' '*' eos # OutAll
    ;

def: header=definitionHeader (':='|'=') nl* body eos;

body: expression ('where' '{' nl* defs+=def+ '}')?;

definitionHeader:
    tessladoc+=DOCLINE* nl*
    annotations+=annotation*
    DEF name=ID
    ('[' nl* typeParameters+=ID (',' nl* typeParameters+=ID)* nl* ']')?
    ('(' nl* parameters+=param (',' nl* parameters+=param)* nl* ')')?
    (':' resultType=type)?;

annotation: '@' ID nl*;

param: ID (':' parameterType=type)?;

type
    : name=ID #SimpleType
    | name=ID '[' typeArguments+=type (',' typeArguments+=type)* ']' #TypeApplication
    | '(' parameterTypes+=type (',' parameterTypes+=type)* ')' '=>' resultType=type #FunctionType
    | (DOLLAR_BRACE | '{') nl* (memberSigs+=memberSig (',' nl* memberSigs+=memberSig)*)? ','? nl* '}' #ObjectType
    | '(' (elementTypes+=type (',' elementTypes+=type)*)? ')' #TupleType
    ;

memberSig: name=ID ':' type;

expression
    : ID #Variable
    | stringLit #StringLiteral
    | (DECINT | HEXINT) timeUnit=ID? #IntLiteral
    | 'true' #True
    | 'false' #False
    | '(' (elems+=expression (',' elems+=expression)*)? lastComma=','? ')' #TupleExpression
    | '{' nl* definitions+=def+ RETURN? expression nl* '}' #Block
    | (DOLLAR_BRACE | '{') nl* members+=memberDefinition (',' nl* members+=memberDefinition)* ','? nl* '}' #ObjectLiteral
    | function=expression (
        ('[' nl* typeArguments+=type (',' nl* typeArguments+=type)* nl* ']')? '(' nl* arguments+=arg (',' nl* arguments+=arg)* nl* ')'
      | ('[' nl* typeArguments+=type (',' nl* typeArguments+=type)* nl* ']')
      )  #FunctionCall
    | obj=expression '.' nl* fieldName=ID #MemberAccess
    | op=('!' | '~' | '-') expression #UnaryExpression
    | lhs=expression op=('*' | '/') nl* rhs=expression #InfixExpression
    | lhs=expression op=('+' | '-') nl* rhs=expression #InfixExpression
    | lhs=expression op=('<<' | '>>') nl* rhs=expression #InfixExpression
    | lhs=expression op='&' nl* rhs=expression #InfixExpression
    | lhs=expression op=('|' | '^') nl* rhs=expression #InfixExpression
    | lhs=expression op=('==' | '!=' | '<' | '>' | '<=' | '>=') nl* rhs=expression #InfixExpression
    | lhs=expression op='&&' nl* rhs=expression #InfixExpression
    | lhs=expression op='||' nl* rhs=expression #InfixExpression
    | ifToken='if' condition=expression nl* 'then' nl* thenCase=expression nl* ('else' nl* elseCase=expression)? #ITE
    | 'static' 'if' condition=expression nl* 'then' nl* thenCase=expression nl* 'else' nl* elseCase=expression #ITE
    | funKW='fun'? openingParen='(' nl* params+=param (',' nl* params+=param)* nl* closingParen=')' '=>' nl* expression #Lambda
    ;

memberDefinition: name=ID ((':'|'=') value=expression)?;

arg: (name=ID '=')? expression;

stringLit: openingQuote=DQUOTE stringContents* closingQuote=DQUOTE;

stringContents
    : TEXT
    | ESCAPE_SEQUENCE
    | DOLLAR_BRACE expression '}'
    | '$' ID
;