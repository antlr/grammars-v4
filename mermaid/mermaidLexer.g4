/*
 BSD License

 Copyright (c) 2026, Tom Everett All rights reserved.

 Redistribution and use in source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this list of conditions
 and the following disclaimer. 2. Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the documentation and/or other
 materials provided with the distribution. 3. Neither the name of Tom Everett nor the names of its
 contributors may be used to endorse or promote products derived from this software without specific
 prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
 CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

lexer grammar mermaidLexer;

ER_DIAGRAM: 'erDiagram' -> mode (ER);
CLASS_DIAGRAM: 'classDiagram' -> mode (CLASS_MODE);
COMMENT: '%%' ~[\r\n]* -> skip;
WS: [ \r\n\t]+ -> skip;

mode ER;
ER_NEXT_DIAGRAM: 'erDiagram' -> type(ER_DIAGRAM);
ER_NEXT_CLASS: 'classDiagram' -> type(CLASS_DIAGRAM), mode(CLASS_MODE);
ER_LINETYPE: '..' | '--';
ER_CARDINALITY: '|o' | 'o|' | '||' | '}o' | 'o{' | '}|' | '|{';
ER_COLON: ':';
ER_LBRACE: '{';
ER_RBRACE: '}';
ER_STRING: '"' ('\\' . | ~["\\\r\n])* '"';
ER_INTTYPE: 'int';
ER_FLOATTYPE:'float';
ER_STRINGTYPE:'string';
ER_KEY: 'PK' | 'FK' | 'UK';
ER_COMMA: ',';
ER_NAME: [a-zA-Z_] [a-zA-Z0-9_\u005B\u005D()-]*;
ER_COMMENT: '%%' ~[\r\n]* -> skip;
ER_NL: '\r'? '\n';
ER_WS: [ \t]+ -> skip;

mode CLASS_MODE;
CLASS_NEXT_ER: 'erDiagram' -> type(ER_DIAGRAM), mode(ER);
CLASS_NEXT_DIAGRAM: 'classDiagram' -> type(CLASS_DIAGRAM);
CLASS: 'class';
CLASS_RELATION: '<|--' | '--|>' | '*--' | '--*' | 'o--' | '--o'
    | '-->' | '<--' | '--' | '..>' | '<..' | '..|>' | '<|..' | '..';
CLASS_MEMBER: ':' ~[\r\n]+;
CLASS_STRING: '"' ('\\' . | ~["\\\r\n])* '"';
CLASS_NAME: [a-zA-Z_] [a-zA-Z0-9_-]*;
CLASS_OPEN: '{' -> pushMode(CLASS_BODY);
CLASS_COMMENT: '%%' ~[\r\n]* -> skip;
CLASS_NL: '\r'? '\n';
CLASS_WS: [ \t]+ -> skip;

mode CLASS_BODY;
CLASS_CLOSE: '}' -> popMode;
CLASS_BODY_COMMENT: '%%' ~[\r\n]* -> skip;
CLASS_BODY_LINE: ~[}\r\n]+;
CLASS_BODY_NL: '\r'? '\n';

