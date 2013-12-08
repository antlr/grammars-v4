/*
 [The "BSD licence"]
 Copyright (c) 2013 Terence Parr
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

/** Must process R input with this before passing to R.g4; see TestR.java
    We strip NL inside expressions.
 */

parser grammar RFilter;

options { tokenVocab=R; }

@members {
protected int curlies = 0;
}

// TODO: MAKE THIS GET ONE COMMAND ONLY
stream : (elem|NL|';')* EOF ;

eat :   (NL {((WritableToken)$NL).setChannel(Token.HIDDEN_CHANNEL);})+ ;

elem:   op eat?
    |   atom
    |   '{' eat? {curlies++;} (elem|NL|';')* {curlies--;} '}'
    |   '(' (elem|eat)* ')'
    |   '[' (elem|eat)* ']'
    |   '[[' (elem|eat)* ']' ']'
    |   'function' eat? '(' (elem|eat)* ')' eat?
    |   'for' eat? '(' (elem|eat)* ')' eat?
    |   'while' eat? '(' (elem|eat)* ')' eat?
    |   'if' eat? '(' (elem|eat)* ')' eat?
    |   'else'
        {
        // ``inside a compound expression, a newline before else is discarded,
        // whereas at the outermost level, the newline terminates the if
        // construction and a subsequent else causes a syntax error.''
        /*
        Works here
            if (1==0) { print(1) } else { print(2) }

        and correctly gets error here:

            if (1==0) { print(1) }
            else { print(2) }

        this works too:

            if (1==0) {
              if (2==0) print(1)
              else print(2)
            }
        */
        WritableToken tok = (WritableToken)_input.LT(-2);
        if (curlies>0&&tok.getType()==NL) tok.setChannel(Token.HIDDEN_CHANNEL);
        }
    ;

atom:   'next' | 'break' | ID | STRING | HEX | INT | FLOAT | COMPLEX | 'NULL'
    |   'NA' | 'Inf' | 'NaN' | 'TRUE' | 'FALSE'
    ;

op  :   '+'|'-'|'*'|'/'|'^'|'<'|'<='|'>='|'>'|'=='|'!='|'&'|'&&'|USER_OP|
        'repeat'|'in'|'?'|'!'|'='|':'|'~'|'$'|'@'|'<-'|'->'|'='|'::'|':::'|
        ','|'...'|'||'| '|'
    ;
