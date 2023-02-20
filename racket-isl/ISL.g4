/* Based on documentation provided at https://docs.racket-lang.org/htdp-langs/intermediate-lam.html */
grammar ISL;

program
    : defOrExpr+ EOF
    ;

defOrExpr
    : definition
    | expr
    | testCase
    | libraryRequire
    ;

definition
    : '(' 'define' '(' name NAME+ ')' expr ')'
    | '(' 'define' name expr ')'
    | '(' 'define-struct' name '(' name* ')' ')'
    ;

expr: '(' 'begin' expr+ ')'
    | '(' 'begin0' expr+ ')'
    | '(' 'set!' NAME expr ')'
    | '(' 'delay' expr ')'
    | '(' 'lambda' '(' NAME* ')' expr ')'
    | '(' 'λ' '(' NAME* ')' expr ')'
    | '(' 'local' '[' definition* ']' expr ')'
    | '(' 'letrec' '(' ('[' name expr ']')* ')' expr ')'
    | '(' 'shared' '(' ('[' name expr ']')* ')' expr ')'
    | '(' 'let' '(' ('[' name expr ']')* ')' expr ')'
    | '(' 'let*' '(' ('[' name expr ']')* ')' expr ')'
    | '(' 'recur' name '(' ('[' name expr']')* ')' expr ')'
    | '(' expr expr* ')'
    | '(' 'cond' ('[' expr expr ']')+ ')'
    | '(' 'cond' ('[' expr expr ']')*  '[' 'else ' expr ']' ')'
    | '(' 'if'  expr expr expr ')'
    | '(' 'and' expr expr expr+ ')'
    | '(' 'or'  expr expr expr+ ')'
    | '(' 'time' expr ')'
    | name
    | '’' quoted
    | '‘' quasiQuoted
    | '\u0027' '()'
    | NUMBER
    | BOOLEAN
    | STRING
    | CHARACTER
    ;

quoted
    : NAME
    | STRING
    | CHARACTER
    | '(' quoted* ')'
    | '’' quoted
    | '‘' quoted
    | ',' quoted
    | ',@' quoted
    ;

quasiQuoted
    : NAME
    | NUMBER
    | STRING
    | CHARACTER
    | '(' quasiQuoted* ')'
    | '’' quasiQuoted
    | '‘' quasiQuoted
    | ',' expr
    | ',@' expr
    ;

testCase
    : '(' 'check-expect' expr expr ')'
    | '(' 'check-random' expr expr ')'
    | '(' 'check-within' expr expr expr ')'
    | '(' 'check-member-of' expr expr+ ')'
    | '(' 'check-satisfied' expr name ')'
    | '(' 'check-error' expr expr? ')'
    ;

libraryRequire
    : '(' 'require' STRING ')'
    | '(' 'require' name ')'
    | '(' 'require' '(' name STRING ('(' STRING+ ')')? ')' ')'
    | '(' 'require' '(' name STRING pkg ')' ')'
    ;

pkg: '(' STRING STRING NUMBER NUMBER ')'
   ;

name: SYMBOL
    | NAME
    ;

// A symbol is a quote character followed by a name. A symbol is a value, just like 42, '(), or #false.
SYMBOL
    : '’' NAME
    ;

// A name or a variable is a sequence of characters not including a space or one of the following:
//   " , ' ` ( ) [ ] { } | ; #
NAME: ([$%&!*+\\^_~] | [--:<-Za-z])+
    ;

// A number is a number such as 123, 3/2, or 5.5.
NUMBER
    : INT
    | INT '.' [0-9]* [1-9]
    | INT '/' INT
    ;

INT: [1-9] [0-9]*
   | '0'
   ;

BOOLEAN
    : '#true'
    | '#T'
    | '#t'
    | '#false'
    | '#F'
    | '#f'
    ;

// A string is a sequence of characters enclosed by a pair of ".
// Unlike symbols, strings may be split into characters and manipulated by a variety of functions.
// For example, "abcdef", "This is a string", and "This is a string with \" inside" are all strings.
STRING
    : '"' ([ -~])* '"'
    ;

// A character begins with #\ and has the name of the character.
// For example, #\a, #\b, and #\space are characters.
CHARACTER
    : '#' '\u005C' [A-Za-z0-9]
    | '#' '\u005C' 'space'
    ;

LANG: '#lang' ~ ('\n' | '\r')* '\r'? '\n' -> channel (HIDDEN)
    ;

COMMENT
    : ';' ~ ('\n' | '\r')* '\r'? '\n' -> channel (HIDDEN)
    ;

WS: (' ' | '\r' | '\t' | '\u000C' | '\n') -> channel (HIDDEN)
  ;
