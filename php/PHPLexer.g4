/*
 [The "BSD licence"]
 Copyright (c) 2013 Tom Everett
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

//
//
// HTML Grammar based on the ANTLR4 XML Grammar by Terence Parr
//
//
lexer grammar PHPLexer;

HTML_COMMENT    
    : '<!--' .*? '-->'
    ;

HTML_DTD 
    : '<!' .*? '>'
    ;

HTML_EntityRef  
    : '&' HTML_Name ';' 
    ;

HTML_CharRef    
    : '&#' DIGIT+ ';'
    | '&#x' HEXDIGIT+ ';'
    ;

SEA_WS
    :  (' '|'\t'|'\r'? '\n')+ 
    ;

OPEN
    : '<' -> pushMode(INSIDE)
    ;

HTML_TEXT
    : ~[<&]+ 
    ;        // match any 16 bit char other than < and &

PHP_Start
    : ('<?' | '<?php' | '<?PHP') -> pushMode(PHP)
    ;

mode INSIDE;

HTML_CLOSE           
    : '>' -> popMode
    ;

HTML_SLASH_CLOSE     
    : '/>' -> popMode
    ;

HTML_SLASH      
    : '/' 
    ;

HTML_EQUALS     
    : '=' 
    ;

HTML_QUOTED_STRING     
    : '"' ~[<"]* '"'
    | '\'' ~[<']* '\''
    ;

//HTML_UNQUOTED_STRING
//    : [a..zA..Z] 
//    ;

HTML_HEX
    : '#' HEXDIGIT+
    ;
    
HTML_Name       
    :
    NameStartChar NameChar* 
    ;

S
    : [ \t\r\n] -> skip 
    ;

fragment
HEXDIGIT        
    :   
    [a-fA-F0-9]
    ;

fragment
DIGIT           
    :   
    [0-9]
    ;

fragment
NameChar        
    : NameStartChar
    | '-' 
    | '_' 
    | '.' 
    | DIGIT 
    | '\u00B7'
    | '\u0300'..'\u036F'
    | '\u203F'..'\u2040'
    ;

fragment
NameStartChar
    :   [:a-zA-Z]
    |   '\u2070'..'\u218F' 
    |   '\u2C00'..'\u2FEF' 
    |   '\u3001'..'\uD7FF' 
    |   '\uF900'..'\uFDCF' 
    |   '\uFDF0'..'\uFFFD'
    ;

mode PHP;

PHP_END         
    :   '?>' -> popMode ;

MultiLineComment
    : '/*' .*? '*/' -> channel(HIDDEN) 
    ;

SinglelineComment
    : '//' ~[\r\n]* -> channel(HIDDEN)
    ;

UnixComment
    : '#' ~[\r\n]* -> channel(HIDDEN)
    ;

RealE
    : 'e' | 'E';
As
    : 'as';
SemiColon
    : ';';
Instanceof
    : 'instanceof';
And
    : 'and';
List
    : 'list';
Const
    : 'const';
Or
    : 'or';
Xor
    : 'xor';
Global
    : 'global';
Continue
    : 'continue';
Return
    : 'return';
Implements
    : 'implements';
Var
    : 'var';
Class
    : 'class';
Extends
    : 'extends';
Do
    : 'do';
Switch
    : 'switch';
Case
    : 'case';
Default
    : 'default';
Function
    : 'function';
Break
    : 'break';
If
    : 'if';
Else
    : 'else';
ElseIf
    : 'elseif';
For
    : 'for';
Foreach
    : 'foreach';
While
    : 'while';
Equals
    : '=';
New
    : 'new';
Clone
    : 'clone';
Ampersand
    : '&';
Pipe
    : '|';
Bang
    : '!';
Plus
    : '+';
Minus
    : '-';
Asterisk
    : '*';
Percent
    : '%';
Forwardslash
    : '/';
Tilde
    : '~';
InstanceMember
    : '->';
SuppressWarnings
    : '@';
Dollar
    : '$';
Dot
    : '.';
ArrayAssign
    : '=>';
LogicalOr
    : '||';
LogicalAnd
    : '&&';
ClassMember
    : '::';
QuestionMark
    : '?';    
OpenRoundBracket
    : '(';
CloseRoundBracket
    : ')';
OpenSquareBracket
    : '[';
CloseSquareBracket
    : ']';
OpenCurlyBracket
    : '{';
CloseCurlyBracket
    : '}';
Interface
    : 'Interface';
Comma
    : ',';
Colon
    : ':';
Abstract
    : 'abstract';
Static
    : 'static';
    
Array
    : ('a'|'A')('r'|'R')('r'|'R')('a'|'A')('y'|'Y')
    ;

RequireOperator
    : 'require' | 'require_once' | 'include' | 'include_once'
    ;

PrimitiveType
    : 'int'|'float'|'string'|'array'|'object'|'bool'
    ;

AccessModifier
    : 'public' | 'private' | 'protected' 
    ;

DecimalNumber 
    : ('0'..'9')+ 
    ;

HexNumber
    : '0'('x'|'X')('0'..'9'|'a'..'f'|'A'..'F')+
    ;

OctalNumber
    : '0'('0'..'7')+
    ;

Float
    : ('0'..'9')* '.' ('0'..'9')+
    ;
	
Digits
    : '0'..'9'+
    ;

Boolean
    : 'true' | 'false'
    ;

SingleQuotedString
     : '\'' ('\\' '\''
     | '\\' '\\' 
     | '\\' | ~ ('\'' | '\\'))* 
       '\''
     ;

DoubleQuotedString_Start
    : '"'  -> pushMode(PHPString);

Identifier
    : ('a'..'z' | 'A'..'Z' | '_')  ('a'..'z' | 'A'..'Z' | '0'..'9' | '_')*
    ;
   
AssignmentOperator
    : '+='|'-='|'*='|'/='|'.='|'%='|'&='|'|='|'^='|'<<='|'>>='
    ;
    
EqualityOperator
    : '==' | '!=' | '===' | '!=='
    ;

ComparisionOperator
    : '<' | '<=' | '>' | '>=' | '<>'
    ;
    
ShiftOperator
    : '<<' | '>>'
    ;

IncrementOperator
    : '--'|'++'
    ;
    
WS : [ \t\r\n]+ -> skip;

mode PHPString;

fragment EscapeCharacter
    : 'n' | 'r' | 't' | '\\' | '$' | '"' | Digits
    ;

DoubleQuotedStringBody
    : ( '\\' EscapeCharacter 
    | '\\' 
    | ~('\\'| '"' | '$') )* 
    ;

VarName
    : '$' Identifier
    ;

DoubleQuotedString_End
    :  '"' ->popMode;


