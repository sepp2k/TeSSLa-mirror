grammar CPattern;

start: pattern EOF;

pattern
  : (functionName=ID '::')? name=ID #Variable
  | pattern '[' ']' #ArrayAccess
  | pattern '.' ID #StructUnionAccess
  | pattern '->' ID #StructUnionAccessArrow
  | '*' pattern #Dereference
  | '&' pattern #Reference
  | '(' pattern ')' #Parentheses
  ;

ID: [a-zA-Z_][a-zA-Z0-9_]*;

INVALID:.;