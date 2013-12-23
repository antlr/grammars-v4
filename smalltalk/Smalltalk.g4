/*
  Converted to ANTLR 4 by James Ladd (Redline Smalltalk Project http://redline.st).
  Adapted from the Amber Smalltalk grammar perser.pegjs 
  
  2013/11/27 James Ladd (object@redline.st)
*/

grammar Smalltalk;

script : sequence EOF;
sequence : ws temps? ws statements? ws;
ws : (SEPARATOR | COMMENT)*;
temps : PIPE (ws IDENTIFIER)* ws PIPE;
statements : answer
           | expressions ws PERIOD ws answer
           | expressions PERIOD?;
answer : CARROT ws expression ws PERIOD?;
expression : assignment | cascade | keywordSend | binarySend | primitive;
expressions : expression expressionList*;
expressionList : ws PERIOD ws expression;
cascade : ws (keywordSend | binarySend) (ws SEMI_COLON ws message ws)+;
message : binaryMessage | unaryMessage | keywordMessage;
assignment : variable ws ASSIGNMENT ws expression;
variable : IDENTIFIER;
binarySend : unarySend binaryTail?;
unarySend : operand ws unaryTail?;
keywordSend : binarySend keywordMessage;
keywordMessage : ws (keywordPair ws)+;
keywordPair : KEYWORD ws binarySend ws;
operand : literal | reference | subexpression;
subexpression : OPEN_PAREN ws expression ws CLOSE_PAREN;
literal : runtimeLiteral | parsetimeLiteral;
runtimeLiteral : dynamicDictionary | dynamicArray | block;
block : BLOCK_START blockParamList? sequence? ws BLOCK_END;
blockParamList : (ws BLOCK_PARAM)+ ws PIPE?;
dynamicDictionary : DYNDICT_START ws expressions? ws DYNARR_END;
dynamicArray : DYNARR_START ws expressions? ws DYNARR_END;
parsetimeLiteral : pseudoVariable | number | charConstant | literalArray | string | symbol;
number : numberExp | hex | stFloat | stInteger;
numberExp : (stFloat | stInteger) EXP stInteger;
charConstant : CHARACTER_CONSTANT;
hex : MINUS? HEX HEXDIGIT+;
stInteger : MINUS? DIGITS;
stFloat : MINUS? DIGITS PERIOD DIGITS;
pseudoVariable : RESERVED_WORD;
string : STRING;
symbol : HASH bareSymbol;
primitive : LT ws KEYWORD ws DIGITS ws GT;
bareSymbol : (IDENTIFIER | BINARY_SELECTOR) | KEYWORD+ | string;
literalArray : LITARR_START literalArrayRest;
literalArrayRest : ws ((parsetimeLiteral | bareLiteralArray | bareSymbol) ws)* CLOSE_PAREN;
bareLiteralArray : OPEN_PAREN literalArrayRest;
parseTimeLiteral : pseudoVariable | number | literalArray | string | symbol;
unaryTail : unaryMessage ws unaryTail?;
unaryMessage : ws unarySelector;
unarySelector : IDENTIFIER;
keywords : KEYWORD+;
reference : variable;
binaryTail : binaryMessage binaryTail?;
binaryMessage : ws BINARY_SELECTOR ws (unarySend | operand);

PIPE : '|';
PERIOD : '.';
CARROT : '^';
COLON : ':';
SEMI_COLON : ';';
ASSIGNMENT : ':=';
MINUS : '-';
HASH : '#';
DOLLAR : '$';
EXP : 'e';
HEX : '16r';
LITARR_START : '#(';
CLOSE_PAREN : ')';
OPEN_PAREN : '(';
DYNDICT_START : '#{';
DYNARR_END : '}';
DYNARR_START : '{';
BLOCK_START : '[';
BLOCK_END : ']';
LT : '<';
GT : '>';
RESERVED_WORD : 'self' | 'nil' | 'true' | 'false' | 'super';
DIGIT : '0'..'9';
DIGITS : ('0'..'9')+;
HEXDIGIT : 'a'..'z' | 'A'..'Z' | DIGIT;
BINARY_SELECTOR : ('-' | '\\' | '+' | '*' | '/' | '=' | '>' | '<' | ',' | '@' | '%' | '~' | '|' | '&' | '?')+;
IDENTIFIER : ('a'..'z' | 'A'..'Z') ('a'..'z' | 'A'..'Z' | '0'..'9' | '_')*;
KEYWORD : IDENTIFIER COLON;
BLOCK_PARAM : COLON IDENTIFIER;
CHARACTER_CONSTANT : DOLLAR (HEXDIGIT | DOLLAR);
SEPARATOR : (' ' | '\t' | '\r' | '\n' | '\u00A0' | '\uFEFF' | '\u2028' | '\u2029')+;
STRING : '\'' (.)*? '\'';
COMMENT : '"' (.)*? '"';

