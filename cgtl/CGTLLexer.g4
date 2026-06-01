/*
* Copyright (c) 2023 Kevin Lano
* This program and the accompanying materials are made available under the
* terms of the Eclipse Public License 2.0 which is available at
* http://www.eclipse.org/legal/epl-2.0
*
* SPDX-License-Identifier: EPL-2.0
*/

lexer grammar CGTLLexer;

options { }

// Keywords and CGTL symbols

NOT                : 'not';
ALL                : 'all';
ANY                : 'any';
MATCHES            : 'matches';

WHEN               : '«when»';
ACTION             : '«action»';
ENDRULE            : '«end»';
MAPSTO             : '«mapsto»';

COMMA              : ',';
COLONCOLON         : '::';
BQ                 : '`'; 
REPLACE            : '/';


STRING_LITERAL:     '"' (~["\\\r\n])* '"';

MULTILINE_COMMENT: '/*' .*? '*/' -> channel(HIDDEN);

SYMBOL : [!#-/:-@[-^{-~]+;

fragment Digits
    : [0-9]+
    ;

NEWLINE : [\r\n]+ -> skip ;
INT     : [0-9]+ ;
FLOAT_LITERAL:  Digits '.' Digits ;

ID  :   [a-zA-Z][a-zA-Z0-9_$]* ;      // match identifiers
VAR :   '_' [0-9]+
    | '_*'
    | '_+'
    ; 

WS  :   [ \t\n\r]+ -> skip ;

