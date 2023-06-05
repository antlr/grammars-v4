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
/*
* examples here: http://svn.ez.no/svn/ezcomponents/trunk/Document/tests/files/wiki/creole/
*/

grammar creole;

document
   : (line? CR)* EOF
   ;

line
   : markup +
   ;

markup
   : bold
   | italics
   | href
   | title
   | hline
   | text_
   | listitem
   | image
   | tablerow
   | tableheader
   | nowiki
   ;

text_
   : (TEXT | RSLASH) + ('\\\\' text_)*
   ;

bold
   : '**' markup + '**'?
   ;

italics
   : RSLASH RSLASH markup + RSLASH RSLASH
   ;

href
   : LBRACKET text_ ('|' markup +)? RBRACKET
   | LBRACE text_ '|' markup + RBRACE
   ;

image
   : LBRACE text_ RBRACE
   ;

hline
   : '----'
   ;

listitem
   : ('*' + markup)
   | ('#' + markup)
   ;

tableheader
   : ('|=' markup +) + '|' WS*
   ;

tablerow
   : ('|' markup +) + '|' WS*
   ;

title
   : '=' + markup '='*
   ;

nowiki
   : NOWIKI
   ;


HASH
   : '#'
   ;


LBRACKET
   : '[['
   ;


RBRACKET
   : ']]'
   ;


LBRACE
   : '{{'
   ;


RBRACE
   : '}}'
   ;


TEXT
   : (LETTERS | DIGITS | SYMBOL | WS) +
   ;


WS
   : [ \t]
   ;


CR
   : '\r'? '\n' | EOF
   ;


NOWIKI
   : '{{{' .*? '}}}'
   ;


RSLASH
   : '/'
   ;


fragment LETTERS
   : [a-zA-Z]
   ;


fragment DIGITS
   : [0-9]
   ;


fragment SYMBOL
   : '.' | ';' | ':' | ',' | '(' | ')' | '-' | '\\' | '\'' | '~' | '"' | '+'
   ;
