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
    | 'type' header=typeHeader ':=' nl* type eos #TypeDefinition
    | 'in' ID (':' type)? eos # In
    | 'out' expression ('as' ID)? eos # Out
    | 'out' '*' eos # OutAll
    ;

def: header=definitionHeader ':=' nl* body=expression eos;

definitionHeader:
    tessladoc+=DOCLINE*
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
    | DOLLAR_BRACE (memberSigs+=memberSig (',' memberSigs+=memberSig)*)? '}' #ObjectType
    ;

memberSig: name=ID ':' type;

typeHeader: ID ('[' typeParameters+=ID (',' typeParameters+=ID)* ']')?;

expression
    : ID #Variable
    | stringLit #StringLiteral
    | (DECINT | HEXINT) #IntLiteral
    | '(' inner=expression ')' #ParenthesizedExpression
    | '{' nl* definitions+=def* RETURN? expression nl* '}' #Block
    | function=expression (
        ('[' nl* typeArguments+=type (',' nl* typeArguments+=type)* nl* ']')? '(' nl* arguments+=arg (',' nl* arguments+=arg)* nl* ')'
      | ('[' nl* typeArguments+=type (',' nl* typeArguments+=type)* nl* ']')
      )  #FunctionCall
    | obj=expression '.' fieldName=ID #MemberAccess
    | op=('!' | '~' | '-') expression #UnaryExpression
    | lhs=expression op=('*' | '/') rhs=expression #InfixExpression
    | lhs=expression op=('+' | '-') rhs=expression #InfixExpression
    | lhs=expression op=('<<' | '>>') rhs=expression #InfixExpression
    | lhs=expression op='&' rhs=expression #InfixExpression
    | lhs=expression op=('|' | '^') rhs=expression #InfixExpression
    | lhs=expression op=('==' | '!=' | '<' | '>' | '<=' | '>=') rhs=expression #InfixExpression
    | lhs=expression op='&&' rhs=expression #InfixExpression
    | lhs=expression op='||' rhs=expression #InfixExpression
    | staticModifier='static'? ifToken='if' condition=expression nl*
        'then' nl* thenCase=expression nl* 'else' nl* elseCase=expression # ITE
    ;

arg: (name=ID '=')? expression;


stringLit: openingQuote=DQUOTE stringContents* closingQuote=DQUOTE;

stringContents
    : TEXT
    | ESCAPE_SEQUENCE
    | DOLLAR_BRACE expression '}'
    | '$' ID
;