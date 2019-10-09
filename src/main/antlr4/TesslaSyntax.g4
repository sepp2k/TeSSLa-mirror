parser grammar TesslaSyntax;

options {
    tokenVocab = 'TesslaLexer';
}

spec: NL* includes+=include* entries+=entry* EOF;

eos: NL+ | ';' | EOF;

include: 'include' file=stringLit eos;

entry: statement eos;

statement
    : def #Definition
    | tessladoc+=DOCLINE* NL* 'def' '@' ID ( '(' parameters+=param (',' parameters+=param)* ')' )? #AnnotationDefinition
    | tessladoc+=DOCLINE* NL* 'type' NL* name=ID ('[' typeParameters+=ID (',' typeParameters+=ID)* ']')? (':='|'=') NL* typeBody #TypeDefinition
    | tessladoc+=DOCLINE* NL* 'module' NL* name=ID NL* '{' NL* contents+=entry* '}' #ModuleDefinition
    | keyword=('import'|'imexport') path+=ID ('.' path+=ID)* ('.' wildcard='*')? #ImportStatement
    | annotations+=annotation* 'in' NL* ID NL* ':' NL* type #In
    | 'out' NL* expression ('as' NL* ID)? #Out
    | 'print' NL* expression #Print
    | 'out' NL* '*' #OutAll
    ;

def: header=definitionHeader (':='|'=') NL* body;

body
    : expression ('where' '{' NL* (defs+=def eos)* '}')? #ExpressionBody
    | '__builtin__' '(' name=ID (',' expression)? ')' #BuiltInBody
    ;

definitionHeader:
    tessladoc+=DOCLINE* NL*
    annotations+=annotation*
    'def' NL* name=ID
    ('[' NL* typeParameters+=ID (',' NL* typeParameters+=ID)* NL* ']')?
    ('(' NL* parameters+=param (',' NL* parameters+=param)* NL* ')')?
    (':' NL* resultType=type)?;

annotation: '@' ID ( '(' arguments+=annotationArg (',' arguments+=annotationArg)* ')' )? NL*;

annotationArg: (name=ID '=' NL*)? constantExpression;

constantExpression
    : (stringLit | DECINT | HEXINT | FLOAT) #ConstantLiteral
    | '(' NL* (elems+=constantExpression (',' NL* elems+=constantExpression)*)?  NL* ')' #ConstantTuple
    | '{' NL* (members+=constantMemberDefinition (',' NL* members+=constantMemberDefinition)*)?  NL* '}' #ConstantObject
    ;

constantMemberDefinition: name=ID ((':'|'=') NL* value=constantExpression)?;


param: ID (':' NL* parameterType=type)?;

typeBody
    : type #TypeAliasBody
    | '__builtin__' '(' name=ID ')' #BuiltInTypeBody
    ;

type
    : name=ID #SimpleType
    | name=ID '[' NL* typeArguments+=type (',' NL* typeArguments+=type)* NL* ']' #TypeApplication
    | '(' NL* parameterTypes+=type (',' NL* parameterTypes+=type)* NL* ')' NL* '=>' NL* resultType=type #FunctionType
    | ('${' | '{') NL* (memberSigs+=memberSig (',' NL* memberSigs+=memberSig)* (',' '..'?)?)? NL* '}' #ObjectType
    | '(' NL* (elementTypes+=type (',' NL* elementTypes+=type)* NL*)? ')' #TupleType
    ;

memberSig: name=ID ':' type;

expression
    : ID #Variable
    | stringLit #StringLiteral
    | (DECINT | HEXINT) timeUnit=ID? #IntLiteral
    | FLOAT #FloatLiteral
    | '(' NL* (elems+=expression (',' NL* elems+=expression)*)? lastComma=','? NL* ')' #TupleExpression
    | '{' NL* (definitions+=def eos)+ RETURN? expression NL* '}' #Block
    | ('${' | '{') NL* (members+=memberDefinition (',' NL* members+=memberDefinition)* ','? NL*)? '}' #ObjectLiteral
    | function=expression (
        ('[' NL* typeArguments+=type (',' NL* typeArguments+=type)* NL* ']')? '(' NL* arguments+=arg (',' NL* arguments+=arg)* NL* ')'
      | ('[' NL* typeArguments+=type (',' NL* typeArguments+=type)* NL* ']')
      )  #FunctionCall
    | '__root__' '.' NL* fieldName=ID #RootMemberAccess
    | obj=expression '.' NL* fieldName=ID #MemberAccess
    | op=('!' | '~' | '-' | '-.') NL* expression #UnaryExpression
    | lhs=expression op=('*' | '/' | '%' | '*.' | '/.') NL* rhs=expression #InfixExpression
    | lhs=expression op=('+' | '-' | '+.' | '-.') NL* rhs=expression #InfixExpression
    | lhs=expression op=('<<' | '>>') NL* rhs=expression #InfixExpression
    | lhs=expression op='&' NL* rhs=expression #InfixExpression
    | lhs=expression op=('|' | '^') NL* rhs=expression #InfixExpression
    | lhs=expression op=('==' | '!=' | '<' | '>' | '<=' | '>=' | '<.' | '>.' | '<=.' | '>=.') NL* rhs=expression #InfixExpression
    | lhs=expression op='&&' NL* rhs=expression #InfixExpression
    | lhs=expression op='||' NL* rhs=expression #InfixExpression
    | ifToken='if' condition=expression NL* 'then' NL* thenCase=expression NL* 'else' NL* elseCase=expression #ITE
    | 'static' 'if' condition=expression NL* 'then' NL* thenCase=expression NL* 'else' NL* elseCase=expression #ITE
    | openingParen='(' NL* params+=param (',' NL* params+=param)* NL* closingParen=')' '=>' NL* expression #Lambda
    ;

memberDefinition: name=ID ((':'|'=') NL* value=expression)?;

arg: (name=ID '=' NL*)? expression;

stringLit: openingQuote=(DQUOTE | 'f"') stringContents* closingQuote=DQUOTE;

stringContents
    : TEXT #Text
    | FORMAT #Format
    | ESCAPE_SEQUENCE #EscapeSequence
    | '${' expression '}' FORMAT? #StringInterpolation
    | '$' ID FORMAT? #StringInterpolation
    ;