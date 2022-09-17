/**
License Dual Licensed under BSD 3-Clause and MIT.  See included `LICENSE` file
for full text.
Copyright Arroyo Networks 2019
Authors: Josh Marshall
*/


lexer grammar RegoLexer;

channels { COMMENTS_AND_FORMATTING }


/*<1>*/
Comment
  : [^\\] '#' [^\r\n]* -> Channel(COMMENTS_AND_FORMATTING)
  ;

fragment
DoubleQuote
  : '"'
  ;

fragment
BackTick
  : '`'
  ;

fragment
QuotedString 
  : DoubleQuote Char* DoubleQuote
  ;

fragment
RawString 
  : BackTick ~[`]* BackTick 
  ;

/*<2>*/
String 
  : QuotedString 
  | RawString
  ;

/*Antlr doesn't support the '{4}' syntax.*/
fragment
UnicodeEscape 
  : 'u' [0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]
  ;

/*<3>*/
Bool 
  : 'true' | 'false'
  ;

/*<4>*/
Null 
  : 'null'
  ;

/*<5>*/
As 
  : 'as' 
  ;

/*<6>*/
Default 
  : 'default' 
  ;

/*<7>*/
Else 
  : 'else' 
  ;

/*<8>*/
Import 
  : 'import' 
  ;

/*<9>*/
Package 
  : 'package' 
  ;

/*<10>*/
Not 
  : 'not' 
  ;

/*<11>*/
With 
  : 'with' 
  ;

/*<12>*/
Set
  : 'set('
  ;

/*<13>*/
LSBrace
  : '['
  ;

/*<14>*/
LCBrace
  : '{'
  ;

/*<15>*/
LParan
  : '('
  ;

/*<16>*/
RSBrace
  : ']'
  ;

/*<17>*/
RCBrace
  : '}'
  ;

/*<18>*/
RParan
  : ')'
  ;

/*<19>*/
Mid
  : '|'
  ;

/*<20>*/
FactorOperator
  : '*'
  | '/' 
  | '%'
  ;

fragment
Plus
  : '+'
  ;

fragment
Minus
  : '-'
  ;

/*<21>*/
ArithOperator
  : Plus 
  | Minus
  ;

/*<22>*/
RelationOperator
  : '==' 
  | '!=' 
  | '<=' 
  | '>=' 
  | '>' 
  | '<'
  ;

/*<23>*/
EqOper 
  : ':='
  | '='
  ;

/*<24>*/
Comma
  : ','
  ;

/*<25>*/
Semicolon
  : ';'
  ;

/*<26>*/
Colon
  : ':'
  ;

/*<27>*/
Ampersand
  : '&'
  ;

/*<28>*/
Dot
  : '.'
  ;

/*<29>*/
WhiteSpace
  : [ \t] -> Channel(COMMENTS_AND_FORMATTING)
  ;

/*<30>*/
LineEnd
  : [\n] -> Channel(COMMENTS_AND_FORMATTING)
  ;

/*<31>*/
WindowsLineEnd
  : [\r] -> skip
  ;

/*****************************************************************************/

fragment
E
  : 'e' | 'E'
  ;

fragment
Real
  : (DecimalDigit* '.' DecimalDigit+ | DecimalDigit+ '.'?)
  ;

/*<32>*/
UnsignedNumber
  : Real (E (Plus | Minus)? Real)?
  ;

fragment
AsciiLetter 
  : [A-Za-z_]
  ;

fragment 
DecimalDigit 
  : [0-9]
  ;

fragment
Char 
  : ~[\u0000-\u001f"\\]
  | '\\' EscapeSequence 
  ;

/*
fragment
MyEscapedChar 
  : [\u0000-\u001f'"''\\']
  ;
*/

fragment
EscapeSequence 
  : SingleCharEscape 
  | UnicodeEscape
  ;

fragment
SingleCharEscape 
  : [\\/bfnrt]
  | DoubleQuote
  | BackTick
  ;

/*****************************************************************************/


/*<33>*/
Name
  : AsciiLetter (AsciiLetter | DecimalDigit)*
  ;