
/*
  Converted to ANTLR 4 by James Ladd (Redline Smalltalk Project http://redline.st).
  Adapted from the Amber Smalltalk grammar parser.pegjs
  
  2015/01/18 James Ladd (object@redline.st)
*/
grammar Smalltalk;

script
   : sequence EOF
   ;

sequence
   : temps? ws statements?
   ;

ws
   : ( SEPARATOR | COMMENT )*
   ;

temps
   : PIPE ( ws IDENTIFIER )+ ws PIPE
   ;

statements
   : answer ws # StatementAnswer | expressions ws PERIOD ws answer # StatementExpressionsAnswer | expressions PERIOD? ws # StatementExpressions
   ;

answer
   : CARROT ws expression ws PERIOD?
   ;

expression
   : assignment | cascade | keywordSend | binarySend | primitive
   ;

expressions
   : expression expressionList*
   ;

expressionList
   : PERIOD ws expression
   ;

cascade
   : ( keywordSend | binarySend ) ( ws SEMI_COLON ws message )+
   ;

message
   : binaryMessage | unaryMessage | keywordMessage
   ;

assignment
   : variable ws ASSIGNMENT ws expression
   ;

variable
   : IDENTIFIER
   ;

binarySend
   : unarySend binaryTail?
   ;

unarySend
   : operand ws unaryTail?
   ;

keywordSend
   : binarySend keywordMessage
   ;

keywordMessage
   : ws ( keywordPair ws )+
   ;

keywordPair
   : KEYWORD ws binarySend ws
   ;

operand
   : literal | reference | subexpression
   ;

subexpression
   : OPEN_PAREN ws expression ws CLOSE_PAREN
   ;

literal
   : runtimeLiteral | parsetimeLiteral
   ;

runtimeLiteral
   : dynamicDictionary | dynamicArray | block
   ;

block
   : BLOCK_START blockParamList? ws sequence? BLOCK_END
   ;

blockParamList
   : ( ws BLOCK_PARAM )+
   ;

dynamicDictionary
   : DYNDICT_START ws expressions? ws DYNARR_END
   ;

dynamicArray
   : DYNARR_START ws expressions? ws DYNARR_END
   ;

parsetimeLiteral
   : pseudoVariable | number | charConstant | literalArray | string | symbol
   ;

number
   : numberExp | hex | stFloat | stInteger
   ;

numberExp
   : ( stFloat | stInteger ) EXP stInteger
   ;

charConstant
   : CHARACTER_CONSTANT
   ;

hex
   : MINUS? HEX HEXDIGIT+
   ;

stInteger
   : MINUS? DIGIT+
   ;

stFloat
   : MINUS? DIGIT+ PERIOD DIGIT+
   ;

pseudoVariable
   : RESERVED_WORD
   ;

string
   : STRING
   ;

symbol
   : HASH bareSymbol
   ;

primitive
   : LT ws KEYWORD ws DIGIT+ ws GT
   ;

bareSymbol
   : ( IDENTIFIER | BINARY_SELECTOR ) | KEYWORD+ | string
   ;

literalArray
   : LITARR_START literalArrayRest
   ;

literalArrayRest
   : ws ( ( parsetimeLiteral | bareLiteralArray | bareSymbol ) ws )* CLOSE_PAREN
   ;

bareLiteralArray
   : OPEN_PAREN literalArrayRest
   ;

unaryTail
   : unaryMessage ws unaryTail? ws
   ;

unaryMessage
   : ws unarySelector
   ;

unarySelector
   : IDENTIFIER
   ;

keywords
   : KEYWORD+
   ;

reference
   : variable
   ;

binaryTail
   : binaryMessage binaryTail?
   ;

binaryMessage
   : ws BINARY_SELECTOR ws ( unarySend | operand )
   ;


SEPARATOR
   : [ \t\r\n]
   ;


STRING
   : '\'' ( . )*? '\''
   ;


COMMENT
   : '"' ( . )*? '"'
   ;


BLOCK_START
   : '['
   ;


BLOCK_END
   : ']'
   ;


CLOSE_PAREN
   : ')'
   ;


OPEN_PAREN
   : '('
   ;


PIPE
   : '|'
   ;


PERIOD
   : '.'
   ;


SEMI_COLON
   : ';'
   ;


BINARY_SELECTOR
   : ( '\\' | '+' | '*' | '/' | '=' | '>' | '<' | ',' | '@' | '%' | '~' | PIPE | '&' | '-' | '?' )+
   ;


LT
   : '<'
   ;


GT
   : '>'
   ;


MINUS
   : '-'
   ;


RESERVED_WORD
   : 'nil' | 'true' | 'false' | 'self' | 'super'
   ;


IDENTIFIER
   : [a-zA-Z]+ [a-zA-Z0-9_]*
   ;


CARROT
   : '^'
   ;


COLON
   : ':'
   ;


ASSIGNMENT
   : ':='
   ;


HASH
   : '#'
   ;


DOLLAR
   : '$'
   ;


EXP
   : 'e'
   ;


HEX
   : '16r'
   ;


LITARR_START
   : '#('
   ;


DYNDICT_START
   : '#{'
   ;


DYNARR_END
   : '}'
   ;


DYNARR_START
   : '{'
   ;


DIGIT
   : [0-9]
   ;


HEXDIGIT
   : [0-9a-fA-F]
   ;


KEYWORD
   : IDENTIFIER COLON
   ;


BLOCK_PARAM
   : COLON IDENTIFIER
   ;


CHARACTER_CONSTANT
   : DOLLAR ( HEXDIGIT | DOLLAR )
   ;
