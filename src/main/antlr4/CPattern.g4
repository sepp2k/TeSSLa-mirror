grammar CPattern;

start: pattern EOF;

pattern
  : ID #Variable
  | pattern '[' ']' #Array
  | pattern '.' ID #Member
  | '*' pattern #Deref
  | '&' pattern #Ref
  ;

ID: [a-zA-Z_][a-zA-Z0-9_]*;

INVALID:.;