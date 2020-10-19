parser grammar TesslaSyntax;

options {
    tokenVocab = 'TesslaLexer';
}

spec:
    NL*
    globalAnnotations+=globalAnnotation*
    includes+=include*
    entries+=entry*
    EOF;

eos: NL+ | ';' | EOF;

include: 'include' file=stringLit eos;

entry: statement eos;

statement
    : def #Definition
    | tessladoc+=DOCLINE* NL* 'def' NL* ('@' | '@@') ID ( '(' NL* (parameters+=param (',' NL* parameters+=param)* )? NL* ')' )? #AnnotationDefinition
    | tessladoc+=DOCLINE* NL* 'type' NL* name=ID ('[' NL* typeParameters+=ID (',' NL* typeParameters+=ID)* NL* ']')? (':='|'=') NL* typeBody #TypeDefinition
    | tessladoc+=DOCLINE* NL* 'module' NL* name=ID NL* '{' NL* contents+=entry* NL* '}' #ModuleDefinition
    | 'import' path+=ID ('.' path+=ID)* #ImportStatement
    | annotations+=annotation* 'in' NL* ID ':' NL* type #In
    | annotations+=annotation* 'out' NL* (expression ('as' NL* ID)? | star='*') #Out
    ;

def: header=definitionHeader (':='|'=') NL* body;

body
    : expression ('where' '{' NL* (defs+=def eos)* '}')? #ExpressionBody
    | 'extern' '(' NL* name=externID (',' NL* expression)? NL* ')' #BuiltInBody
    ;

definitionHeader:
    tessladoc+=DOCLINE* NL*
    liftable='liftable'? NL*
    'def' NL* name=ID
    ('[' NL* typeParameters+=ID (',' NL* typeParameters+=ID)* NL* ']')?
    (paren='(' NL* (parameters+=param (',' NL* parameters+=param)* )? NL* ')')?
    (':' NL* resultType=type)?;

annotation: '@' annotationInner;
globalAnnotation: '@@' annotationInner;

annotationInner: ID ( '(' NL* (arguments+=annotationArg (',' NL* arguments+=annotationArg)* )? NL* ')' )? NL*;

annotationArg: (name=ID '=' NL*)? expression;

param: ID (':' NL* parameterType=evalType)?;

evalType: evaluation=(STRICT | LAZY)? typ=type;

typeBody
    : type #TypeAliasBody
    | 'extern' '(' NL* name=externID NL* ')' #BuiltInTypeBody
    ;

type
    : name=ID #SimpleType
    | name=ID '[' NL* typeArguments+=type (',' NL* typeArguments+=type)* NL* ']' #TypeApplication
    | '(' NL* parameterTypes+=evalType (',' NL* parameterTypes+=evalType)* NL* ')' '=>' NL* resultType=type #FunctionType
    | ('${' | '{') NL* (memberSigs+=memberSig (',' NL* memberSigs+=memberSig)* (',')?)? NL* '}' #ObjectType
    | '(' NL* (elementTypes+=type (',' NL* elementTypes+=type)* )? NL* ')' #TupleType
    ;

memberSig: name=ID ':' NL* type;

expression
    : ID #Variable
    | stringLit #StringLiteral
    | (DECINT | HEXINT) timeUnit=ID? #IntLiteral
    | FLOAT #FloatLiteral
    | '(' NL* (elems+=expression (',' NL* elems+=expression)*)? lastComma=','? NL* ')' #TupleExpression
    | '{' NL* (definitions+=def eos)* RETURN? expression NL* '}' #Block
    | ('${' | '{') NL* (members+=memberDefinition (',' NL* members+=memberDefinition)* ','? NL*)? '}' #ObjectLiteral
    | function=expression (
        ('[' NL* typeArguments+=type (',' NL* typeArguments+=type)* NL* ']')? '(' (NL* arguments+=arg (',' NL* arguments+=arg)* )? NL* ')'
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
    | openingParen='(' NL* (params+=param (',' NL* params+=param)* )? NL* closingParen=')' '=>' NL* expression #Lambda
    ;

memberDefinition: name=ID ((':'|'=') NL* value=expression)?;

arg: (name=ID '=' NL*)? expression;

stringLit: openingQuote=(DQUOTE | 'f"') stringContents* closingQuote=DQUOTE;

externID: DQUOTE content=TEXT DQUOTE;

stringContents
    : TEXT #Text
    | FORMAT #Format
    | ESCAPE_SEQUENCE #EscapeSequence
    | '${' expression '}' FORMAT? #StringInterpolation
    | '$' ID FORMAT? #StringInterpolation
    ;