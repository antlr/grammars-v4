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

/**
derived from http://svn.r-project.org/R/trunk/src/main/gram.y
http://cran.r-project.org/doc/manuals/R-lang.html#Parser

I'm no R genius but this seems to work.

Requires RFilter.g4 to strip away NL that are really whitespace,
not end-of-command. See TestR.java

Usage:

$ antlr4 R.g4 RFilter.g4
$ javac *.java
$ java TestR sample.R
... prints parse tree ...
*/

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar R;

prog
    : ((SEMICOLON | NL)+ | expr )* EOF
    ;

/*
expr_or_assign
    :   expr ('<-'|'='|'<<-') expr_or_assign
    |   expr
    ;
*/

expr
    : expr LIST_ACCESS_START sublist LIST_ACCESS_END #ListAccess // '[[' follows R's yacc grammar
    | expr ARRAY_ACCESS_START sublist ARRAY_ACCESS_END #ArrayAccess
    | expr NAMESPACE_ACCESS expr #NamespaceAccess
    | expr COMPONENT_ACCESS expr #ComponentAccess
    | <assoc = right> expr '^' expr #Exponent
    | ADD_SUB expr #Sign
    | expr RANGE_OPERATOR expr #Range
    | expr USER_OP expr #UserDefinedOperation // anything wrappedin %: '%' .* '%'
    | expr MULT_DIV expr #MultOrDiv
    | expr ADD_SUB expr #AddOrSub
    | expr COMPARATOR expr #Comparison
    | NOT expr #Not
    | expr AND expr #And
    | expr OR expr #Or
    | '~' expr #ModelFormulaePrefix
    | expr '~' expr #ModelFormulaeInfix
    | expr (ASSIGN | EQUALS) expr #Assignment
    | FUNCTION PAREN_L formlist? PAREN_R expr #FunctionDefinition // define function
    | expr PAREN_L sublist PAREN_R #FunctionCall              // call function
    | CURLY_L exprlist CURLY_R #CompoundStatement                  // compound statement
    | IF PAREN_L expr PAREN_R expr #If
    | IF PAREN_L expr PAREN_R expr NL* ELSE expr #IfElse
    | FOR PAREN_L ID IN expr PAREN_R expr #For
    | WHILE PAREN_L expr PAREN_R expr #While
    | REPEAT expr #Repeat
    | HELP expr #Help // get help on expr, usually string or ID
    | NEXT #Next
    | BREAK #Break
    | PAREN_L expr PAREN_R #BracketTerm
    | ID #Id
    | STRING #String
    | HEX #Hex
    | INT #Int
    | FLOAT #Float
    | COMPLEX #Complex
    | NULL #Null
    | NA #Na
    | INF #Inf
    | NAN #Nan
    | TRUE #True
    | FALSE #False
    | NL+ expr #Newline
    ;

exprlist
    : expr ((SEMICOLON | NL) expr?)*
    ;

formlist
    : form (',' form)*
    ;

form
    : ID
    | ID EQUALS expr
    | '...'
    | '.'
    ;

sublist
    : sub (',' sub)*
    ;

sub
    : expr
    | ID EQUALS
    | ID EQUALS expr
    | STRING EQUALS
    | STRING EQUALS expr
    | NULL EQUALS
    | NULL EQUALS expr
    | '...'
    | '.'
    |
    ;

IF: 'if';
FOR: 'for';
WHILE: 'while';
REPEAT: 'repeat';
FUNCTION: 'function';
ELSE: 'else';
IN: 'in';

LIST_ACCESS_START: '[[';
LIST_ACCESS_END: ']]';
ARRAY_ACCESS_START: '[';
ARRAY_ACCESS_END: ']';
NAMESPACE_ACCESS: ':::' | '::';
COMPONENT_ACCESS: '$' | '@';

HELP: '?';
NEXT: 'next';
BREAK: 'break';

NULL: 'NULL';
NA: 'NA';
INF: 'inf';
NAN: 'NaN';
TRUE: 'TRUE';
FALSE: 'FALSE';

NOT: '!';
RANGE_OPERATOR: ':';

MULT_DIV: '*' | '/';
ADD_SUB: '+' | '-';
COMPARATOR: '>' | '>=' | '<' | '<=' | '==' | '!=';
ASSIGN: '<-' | '<<-' | '->' | '->>' | ':=';
EQUALS: '=';
AND: '&&' | '&';
OR: '||' | '|';

PAREN_L: '(';
PAREN_R: ')';
CURLY_L: '{';
CURLY_R: '}';

HEX
    : '0' ('x' | 'X') HEXDIGIT+ [Ll]?
    ;

INT
    : DIGIT+ [Ll]?
    ;

fragment HEXDIGIT
    : ('0' ..'9' | 'a' ..'f' | 'A' ..'F')
    ;

FLOAT
    : DIGIT+ '.' DIGIT* EXP? [Ll]?
    | DIGIT+ EXP? [Ll]?
    | '.' DIGIT+ EXP? [Ll]?
    ;

fragment DIGIT
    : '0' ..'9'
    ;

fragment EXP
    : ('E' | 'e') ('+' | '-')? INT
    ;

COMPLEX
    : INT 'i'
    | FLOAT 'i'
    ;

STRING
    : '"' (ESC | ~[\\"])*? '"'
    | '\'' ( ESC | ~[\\'])*? '\''
    | '`' ( ESC | ~[\\'])*? '`'
    ;

fragment ESC
    : '\\' [abtnfrv"'\\]
    | UNICODE_ESCAPE
    | HEX_ESCAPE
    | OCTAL_ESCAPE
    ;

fragment UNICODE_ESCAPE
    : '\\' 'u' HEXDIGIT HEXDIGIT HEXDIGIT HEXDIGIT
    | '\\' 'u' '{' HEXDIGIT HEXDIGIT HEXDIGIT HEXDIGIT '}'
    ;

fragment OCTAL_ESCAPE
    : '\\' [0-3] [0-7] [0-7]
    | '\\' [0-7] [0-7]
    | '\\' [0-7]
    ;

fragment HEX_ESCAPE
    : '\\' HEXDIGIT HEXDIGIT?
    ;

ID
    : '.' (LETTER | '_' | '.') (LETTER | DIGIT | '_' | '.')*
    | LETTER (LETTER | DIGIT | '_' | '.')*
    ;

fragment LETTER
    : [a-zA-Z]
    ;

USER_OP
    : '%' .*? '%'
    ;

COMMENT
    : '#' .*? '\r'? '\n' -> type(NL)
    ;

// Match both UNIX and Windows newlines
NL
    : '\r'? '\n'
    ;

SEMICOLON: ';';

WS
    : [ \t\u000C]+ -> skip
    ;