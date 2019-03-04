
/** A Java stacktrace text grammar for ANTLR v3 
 *  see http://java.sun.com/javase/6/docs/api/java/lang/Throwable.html
 *
 *  Version 1.0 -- initial release March 9, 2008 (requires 3.0 or higher)
 *
 *  Primary authors: Luca Dall'Olio, Christian Cassano, Gabriele Contini
 */

/*
    Ported to Antlr4 by Tom Everett <tom@khubla.com>
*/
grammar StackTrace;

startRule
   : stackTrace EOF
   ;

stackTrace
   : messageLine + stackTraceLine* causedByLine?
   ;

stackTraceLine
   : (atLine | ellipsisLine)
   ;

atLine
   : AT qualifiedMethod '(' classFile (COLON Number)? ')'
   ;

causedByLine
   : CAUSED_BY stackTrace
   ;

ellipsisLine
   : ELLIPSIS Number MORE_
   ;

messageLine
   : (qualifiedClass message?)
   ;

qualifiedClass
   : packagePath? className innerClassName*
   ;

innerClassName
   : ('$' className)
   ;

classFile
   : (identifier '.java' | NATIVE_METHOD | UNKNOWN_SOURCE)
   ;

qualifiedMethod
   : qualifiedClass DOT (methodName | constructor)?
   ;

constructor
   : INIT
   ;

methodName
   : identifier
   ;

packagePath
   : (identifier DOT) +
   ;

className
   : JavaWord
   ;

identifier
   : JavaWord
   ;

message
   : COLON (: .)*?
   ;


Number
   : Digit +
   ;


JavaWord
   : (JavaCharacter) +
   ;


fragment JavaCharacter
   : (CapitalLetter | NonCapitalLetter | Symbol | Digit)
   ;


DOT
   : '.'
   ;


AT
   : 'at'
   ;


CAUSED_BY
   : 'Caused by:'
   ;


MORE_
   : 'more'
   ;


ELLIPSIS
   : '...'
   ;


COLON
   : ':'
   ;


NATIVE_METHOD
   : 'Native Method'
   ;


UNKNOWN_SOURCE
   : 'Unknown Source'
   ;


INIT
   : '<init>'
   ;


NonCapitalLetter
   : 'a' .. 'z'
   ;


CapitalLetter
   : 'A' .. 'Z'
   ;


Symbol
   : '_'
   ;


Digit
   : '0' .. '9'
   ;


WS
   : (' ' | '\r' | '\t' | '\u000C' | '\n') -> skip
   ;
