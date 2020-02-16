/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2014 Bart Kiers
 * Copyright (c) 2019 Robert Einhorn
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * Project      : Python3-parser; an ANTLR4 grammar for Python 3
 *                https://github.com/bkiers/Python3-parser
 * Developed by : Bart Kiers, bart@big-o.nl
 *
 * Project      : an ANTLR4 grammar for Python 3 without actions
 *                https://github.com/antlr/grammars-v4/tree/master/python/python3-without-actions
 * Developed by : Robert Einhorn, robert.einhorn.hu@gmail.com
 */

// Based on the Bart Kiers ANTLR4 Python grammar: https://github.com/bkiers/Python3-parser
// All comments that start with "///" are copy-pasted from
// the Python 3.3 Language Reference: https://docs.python.org/3.3/reference/grammar.html

grammar Python3;

tokens { INDENT, DEDENT }


/*
 * parser rules
 */

/// single_input: NEWLINE | simple_stmt | compound_stmt NEWLINE
single_input
 : NEWLINE
 | simple_stmt
 | compound_stmt NEWLINE
 ;

/// file_input: (NEWLINE | stmt)* ENDMARKER
file_input
 : ( NEWLINE | stmt )* EOF
 ;

/// eval_input: testlist NEWLINE* ENDMARKER
eval_input
 : testlist NEWLINE* EOF
 ;

/// decorator: '@' dotted_name [ '(' [arglist] ')' ] NEWLINE
decorator
 : '@' dotted_name ( '(' arglist? ')' )? NEWLINE
 ;

/// decorators: decorator+
decorators
 : decorator+
 ;

/// decorated: decorators (classdef | funcdef)
decorated
 : decorators ( classdef | funcdef )
 ;

/// funcdef: 'def' NAME parameters ['->' test] ':' suite
funcdef
 : DEF NAME parameters ( '->' test )? ':' suite
 ;

/// parameters: '(' [typedargslist] ')'
parameters
 : '(' typedargslist? ')'
 ;

/// typedargslist: (tfpdef ['=' test] (',' tfpdef ['=' test])* [','
///                ['*' [tfpdef] (',' tfpdef ['=' test])* [',' '**' tfpdef] | '**' tfpdef]]
///              |  '*' [tfpdef] (',' tfpdef ['=' test])* [',' '**' tfpdef] | '**' tfpdef)
typedargslist
 : tfpdef ( '=' test )? ( ',' tfpdef ( '=' test )? )* ( ',' ( '*' tfpdef? ( ',' tfpdef ( '=' test )? )* ( ',' '**' tfpdef )? 
                                                            | '**' tfpdef 
                                                            )? 
                                                      )?
 | '*' tfpdef? ( ',' tfpdef ( '=' test )? )* ( ',' '**' tfpdef )? 
 | '**' tfpdef
 ;

/// tfpdef: NAME [':' test]
tfpdef
 : NAME ( ':' test )?
 ;

/// varargslist: (vfpdef ['=' test] (',' vfpdef ['=' test])* [','
///       ['*' [vfpdef] (',' vfpdef ['=' test])* [',' '**' vfpdef] | '**' vfpdef]]
///     |  '*' [vfpdef] (',' vfpdef ['=' test])* [',' '**' vfpdef] | '**' vfpdef)
varargslist
 : vfpdef ( '=' test )? ( ',' vfpdef ( '=' test )? )* ( ',' ( '*' vfpdef? ( ',' vfpdef ( '=' test )? )* ( ',' '**' vfpdef )? 
                                                            | '**' vfpdef 
                                                            )? 
                                                      )?
 | '*' vfpdef? ( ',' vfpdef ( '=' test )? )* ( ',' '**' vfpdef )?
 | '**' vfpdef
 ;

/// vfpdef: NAME
vfpdef
 : NAME
 ;

/// stmt: simple_stmt | compound_stmt
stmt
 : simple_stmt 
 | compound_stmt
 ;

/// simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE
simple_stmt
 : small_stmt ( ';' small_stmt )* ';'? NEWLINE
 ;

/// small_stmt: (expr_stmt | del_stmt | pass_stmt | flow_stmt |
///              import_stmt | global_stmt | nonlocal_stmt | assert_stmt)
small_stmt
 : expr_stmt 
 | del_stmt 
 | pass_stmt 
 | flow_stmt 
 | import_stmt 
 | global_stmt 
 | nonlocal_stmt 
 | assert_stmt
 ;

/// expr_stmt: testlist_star_expr (augassign (yield_expr|testlist) |
///                      ('=' (yield_expr|testlist_star_expr))*)
expr_stmt
 : testlist_star_expr ( augassign ( yield_expr | testlist) 
                      | ( '=' ( yield_expr| testlist_star_expr ) )*
                      )
 ;           

/// testlist_star_expr: (test|star_expr) (',' (test|star_expr))* [',']
testlist_star_expr
 : ( test | star_expr ) ( ',' ( test |  star_expr ) )* ','?
 ;

/// augassign: ('+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' |
///             '<<=' | '>>=' | '**=' | '//=')
augassign
 : '+=' 
 | '-=' 
 | '*=' 
 | '@=' // PEP 465
 | '/=' 
 | '%=' 
 | '&=' 
 | '|=' 
 | '^=' 
 | '<<=' 
 | '>>=' 
 | '**=' 
 | '//='
 ;

/// del_stmt: 'del' exprlist
del_stmt
 : DEL exprlist
 ;

/// pass_stmt: 'pass'
pass_stmt
 : PASS
 ;

/// flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt
flow_stmt
 : break_stmt 
 | continue_stmt 
 | return_stmt 
 | raise_stmt 
 | yield_stmt
 ;

/// break_stmt: 'break'
break_stmt
 : BREAK
 ;

/// continue_stmt: 'continue'
continue_stmt
 : CONTINUE
 ;

/// return_stmt: 'return' [testlist]
return_stmt
 : RETURN testlist?
 ;

/// yield_stmt: yield_expr
yield_stmt
 : yield_expr
 ;

/// raise_stmt: 'raise' [test ['from' test]]
raise_stmt
 : RAISE ( test ( FROM test )? )?
 ;

/// import_stmt: import_name | import_from
import_stmt
 : import_name 
 | import_from
 ;

/// import_name: 'import' dotted_as_names
import_name
 : IMPORT dotted_as_names
 ;

/// # note below: the ('.' | '...') is necessary because '...' is tokenized as ELLIPSIS
/// import_from: ('from' (('.' | '...')* dotted_name | ('.' | '...')+)
///               'import' ('*' | '(' import_as_names ')' | import_as_names))
import_from
 : FROM ( ( '.' | '...' )* dotted_name 
        | ('.' | '...')+ 
        )
   IMPORT ( '*' 
          | '(' import_as_names ')' 
          | import_as_names
          )         
 ;

/// import_as_name: NAME ['as' NAME]
import_as_name
 : NAME ( AS NAME )?
 ;

/// dotted_as_name: dotted_name ['as' NAME]
dotted_as_name
 : dotted_name ( AS NAME )?
 ;

/// import_as_names: import_as_name (',' import_as_name)* [',']
import_as_names
 : import_as_name ( ',' import_as_name )* ','?
 ;

/// dotted_as_names: dotted_as_name (',' dotted_as_name)*
dotted_as_names
 : dotted_as_name ( ',' dotted_as_name )*
 ;

/// dotted_name: NAME ('.' NAME)*
dotted_name
 : NAME ( '.' NAME )*
 ;

/// global_stmt: 'global' NAME (',' NAME)*
global_stmt
 : GLOBAL NAME ( ',' NAME )*
 ;

/// nonlocal_stmt: 'nonlocal' NAME (',' NAME)*
nonlocal_stmt
 : NONLOCAL NAME ( ',' NAME )*
 ;

/// assert_stmt: 'assert' test [',' test]
assert_stmt
 : ASSERT test ( ',' test )?
 ;

/// compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef | decorated
compound_stmt
 : if_stmt 
 | while_stmt 
 | for_stmt 
 | try_stmt 
 | with_stmt 
 | funcdef 
 | classdef 
 | decorated
 ;

/// if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
if_stmt
 : IF test ':' suite ( ELIF test ':' suite )* ( ELSE ':' suite )?
 ;

/// while_stmt: 'while' test ':' suite ['else' ':' suite]
while_stmt
 : WHILE test ':' suite ( ELSE ':' suite )?
 ;

/// for_stmt: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]
for_stmt
 : FOR exprlist IN testlist ':' suite ( ELSE ':' suite )?
 ;

/// try_stmt: ('try' ':' suite
///            ((except_clause ':' suite)+
///       ['else' ':' suite]
///       ['finally' ':' suite] |
///      'finally' ':' suite))
try_stmt
 : TRY ':' suite ( ( except_clause ':' suite )+ 
                   ( ELSE ':' suite )? 
                   ( FINALLY ':' suite )?
                 | FINALLY ':' suite
                 )
 ;

/// with_stmt: 'with' with_item (',' with_item)*  ':' suite
with_stmt
 : WITH with_item ( ',' with_item )* ':' suite
 ;

/// with_item: test ['as' expr]
with_item
 : test ( AS expr )?
 ;

/// # NB compile.c makes sure that the default except clause is last
/// except_clause: 'except' [test ['as' NAME]]
except_clause
 : EXCEPT ( test ( AS NAME )? )?
 ;

/// suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT
suite
 : simple_stmt 
 | NEWLINE INDENT stmt+ DEDENT
 ;

/// test: or_test ['if' or_test 'else' test] | lambdef
test
 : or_test ( IF or_test ELSE test )?
 | lambdef
 ;

/// test_nocond: or_test | lambdef_nocond
test_nocond
 : or_test 
 | lambdef_nocond
 ;

/// lambdef: 'lambda' [varargslist] ':' test
lambdef
 : LAMBDA varargslist? ':' test
 ;

/// lambdef_nocond: 'lambda' [varargslist] ':' test_nocond
lambdef_nocond
 : LAMBDA varargslist? ':' test_nocond
 ;

/// or_test: and_test ('or' and_test)*
or_test
 : and_test ( OR and_test )*
 ;

/// and_test: not_test ('and' not_test)*
and_test
 : not_test ( AND not_test )*
 ;

/// not_test: 'not' not_test | comparison
not_test
 : NOT not_test 
 | comparison
 ;

/// comparison: star_expr (comp_op star_expr)*
comparison
 : star_expr ( comp_op star_expr )*
 ;

/// # <> isn't actually a valid comparison operator in Python. It's here for the
/// # sake of a __future__ import described in PEP 401
/// comp_op: '<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not'
comp_op
 : '<'
 | '>'
 | '=='
 | '>='
 | '<='
 | '<>'
 | '!='
 | IN
 | NOT IN
 | IS
 | IS NOT
 ;

/// star_expr: ['*'] expr
star_expr
 : '*'? expr
 ;

/// expr: xor_expr ('|' xor_expr)*
expr
 : xor_expr ( '|' xor_expr )*
 ;

/// xor_expr: and_expr ('^' and_expr)*
xor_expr
 : and_expr ( '^' and_expr )*
 ;

/// and_expr: shift_expr ('&' shift_expr)*
and_expr
 : shift_expr ( '&' shift_expr )*
 ;

/// shift_expr: arith_expr (('<<'|'>>') arith_expr)*
shift_expr
 : arith_expr ( '<<' arith_expr 
              | '>>' arith_expr 
              )*
 ;

/// arith_expr: term (('+'|'-') term)*
arith_expr
 : term ( '+' term
        | '-' term 
        )*
 ;

/// term: factor (('*'|'/'|'%'|'//') factor)*
term
 : factor ( '*' factor
          | '/' factor
          | '%' factor 
          | '//' factor 
          | '@' factor // PEP 465
          )*
 ;

/// factor: ('+'|'-'|'~') factor | power
factor
 : '+' factor 
 | '-' factor 
 | '~' factor 
 | power
 ;

/// power: atom trailer* ['**' factor]
power
 : atom trailer* ( '**' factor )?
 ;

/// atom: ('(' [yield_expr|testlist_comp] ')' |
///        '[' [testlist_comp] ']' |
///        '{' [dictorsetmaker] '}' |
///        NAME | NUMBER | STRING+ | '...' | 'None' | 'True' | 'False')
atom
 : '(' ( yield_expr | testlist_comp )? ')' 
 | '[' testlist_comp? ']'  
 | '{' dictorsetmaker? '}' 
 | NAME 
 | number 
 | str+ 
 | '...' 
 | NONE
 | TRUE
 | FALSE
 ;

/// testlist_comp: test ( comp_for | (',' test)* [','] )
testlist_comp
 : test ( comp_for 
        | ( ',' test )* ','? 
        )
 ;

/// trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
trailer
 : '(' arglist? ')' 
 | '[' subscriptlist ']' 
 | '.' NAME
 ;

/// subscriptlist: subscript (',' subscript)* [',']
subscriptlist
 : subscript ( ',' subscript )* ','?
 ;

/// subscript: test | [test] ':' [test] [sliceop]
subscript
 : test 
 | test? ':' test? sliceop?
 ;

/// sliceop: ':' [test]
sliceop
 : ':' test?
 ;

/// exprlist: star_expr (',' star_expr)* [',']
exprlist
 : star_expr ( ',' star_expr )* ','?
 ;

/// testlist: test (',' test)* [',']
testlist
 : test ( ',' test )* ','?
 ;

/// dictorsetmaker: ( (test ':' test (comp_for | (',' test ':' test)* [','])) |
///                   (test (comp_for | (',' test)* [','])) )
dictorsetmaker
 : test ':' test ( comp_for 
                 | ( ',' test ':' test )* ','? 
                 ) 
 | test ( comp_for 
        | ( ',' test )* ','? 
        )
 ;

/// classdef: 'class' NAME ['(' [arglist] ')'] ':' suite
classdef
 : CLASS NAME ( '(' arglist? ')' )? ':' suite
 ;

/// arglist: (argument ',')* (argument [',']
///                          |'*' test (',' argument)* [',' '**' test]
///                          |'**' test)
arglist
 : ( argument ',' )* ( argument ','?
                     | '*' test ( ',' argument )* ( ',' '**' test )?
                     | '**' test
                     )
 ;

/// # The reason that keywords are test nodes instead of NAME is that using NAME
/// # results in an ambiguity. ast.c makes sure it's a NAME.
/// argument: test [comp_for] | test '=' test  # Really [keyword '='] test
argument
 : test comp_for? 
 | test '=' test
 ;

/// comp_iter: comp_for | comp_if
comp_iter
 : comp_for 
 | comp_if
 ;

/// comp_for: 'for' exprlist 'in' or_test [comp_iter]
comp_for
 : FOR exprlist IN or_test comp_iter?
 ;

/// comp_if: 'if' test_nocond [comp_iter]
comp_if
 : IF test_nocond comp_iter?
 ;

/// yield_expr: 'yield' [testlist]
yield_expr
 : YIELD yield_arg?
 ;

/// yield_arg: 'from' test | testlist
yield_arg
 : FROM test 
 | testlist
 ;

str
 : STRING_LITERAL
 | BYTES_LITERAL
 ;

number
 : integer
 | FLOAT_NUMBER
 | IMAG_NUMBER
 ;

/// integer        ::=  decimalinteger | octinteger | hexinteger | bininteger
integer
 : DECIMAL_INTEGER
 | OCT_INTEGER
 | HEX_INTEGER
 | BIN_INTEGER
 ;

/*
 * lexer rules
 */

DEF : 'def';
RETURN : 'return';
RAISE : 'raise';
FROM : 'from';
IMPORT : 'import';
AS : 'as';
GLOBAL : 'global';
NONLOCAL : 'nonlocal';
ASSERT : 'assert';
IF : 'if';
ELIF : 'elif';
ELSE : 'else';
WHILE : 'while';
FOR : 'for';
IN : 'in';
TRY : 'try';
FINALLY : 'finally';
WITH : 'with';
EXCEPT : 'except';
LAMBDA : 'lambda';
OR : 'or';
AND : 'and';
NOT : 'not';
IS : 'is';
NONE : 'None';
TRUE : 'True';
FALSE : 'False';
CLASS : 'class';
YIELD : 'yield';
DEL : 'del';
PASS : 'pass';
CONTINUE : 'continue';
BREAK : 'break';

NEWLINE : ( '\r'? '\n' | '\r' | '\f' ) SPACES?;

/// identifier   ::=  id_start id_continue*
NAME
 : ID_START ID_CONTINUE*
 ;

/// stringliteral   ::=  [stringprefix](shortstring | longstring)
/// stringprefix    ::=  "r" | "R"
STRING_LITERAL
 : [uU]? [rR]? ( SHORT_STRING | LONG_STRING )
 ;

/// bytesliteral   ::=  bytesprefix(shortbytes | longbytes)
/// bytesprefix    ::=  "b" | "B" | "br" | "Br" | "bR" | "BR"
BYTES_LITERAL
 : [bB] [rR]? ( SHORT_BYTES | LONG_BYTES )
 ;

/// decimalinteger ::=  nonzerodigit digit* | "0"+
DECIMAL_INTEGER
 : NON_ZERO_DIGIT DIGIT*
 | '0'+
 ;

/// octinteger     ::=  "0" ("o" | "O") octdigit+
OCT_INTEGER
 : '0' [oO] OCT_DIGIT+
 ;

/// hexinteger     ::=  "0" ("x" | "X") hexdigit+
HEX_INTEGER
 : '0' [xX] HEX_DIGIT+
 ;

/// bininteger     ::=  "0" ("b" | "B") bindigit+
BIN_INTEGER
 : '0' [bB] BIN_DIGIT+
 ;

/// floatnumber   ::=  pointfloat | exponentfloat
FLOAT_NUMBER
 : POINT_FLOAT
 | EXPONENT_FLOAT
 ;

/// imagnumber ::=  (floatnumber | intpart) ("j" | "J")
IMAG_NUMBER
 : ( FLOAT_NUMBER | INT_PART ) [jJ]
 ;

DOT : '.';
ELLIPSIS : '...';
STAR : '*';
OPEN_PAREN : '(';
CLOSE_PAREN : ')';
COMMA : ',';
COLON : ':';
SEMI_COLON : ';';
POWER : '**';
ASSIGN : '=';
OPEN_BRACK : '[';
CLOSE_BRACK : ']';
OR_OP : '|';
XOR : '^';
AND_OP : '&';
LEFT_SHIFT : '<<';
RIGHT_SHIFT : '>>';
ADD : '+';
MINUS : '-';
DIV : '/';
MOD : '%';
IDIV : '//';
NOT_OP : '~';
OPEN_BRACE : '{';
CLOSE_BRACE : '}';
LESS_THAN : '<';
GREATER_THAN : '>';
EQUALS : '==';
GT_EQ : '>=';
LT_EQ : '<=';
NOT_EQ_1 : '<>';
NOT_EQ_2 : '!=';
AT : '@';
ARROW : '->';
ADD_ASSIGN : '+=';
SUB_ASSIGN : '-=';
MULT_ASSIGN : '*=';
AT_ASSIGN : '@=';
DIV_ASSIGN : '/=';
MOD_ASSIGN : '%=';
AND_ASSIGN : '&=';
OR_ASSIGN : '|=';
XOR_ASSIGN : '^=';
LEFT_SHIFT_ASSIGN : '<<=';
RIGHT_SHIFT_ASSIGN : '>>=';
POWER_ASSIGN : '**=';
IDIV_ASSIGN : '//=';

SKIP_
 : ( SPACES | COMMENT | LINE_JOINING ) -> skip
 ;

UNKNOWN_CHAR
 : .
 ;

/* 
 * fragments 
 */

/// shortstring     ::=  "'" shortstringitem* "'" | '"' shortstringitem* '"'
/// shortstringitem ::=  shortstringchar | stringescapeseq
/// shortstringchar ::=  <any source character except "\" or newline or the quote>
fragment SHORT_STRING
 : '\'' ( STRING_ESCAPE_SEQ | ~[\\\r\n\f'] )* '\''
 | '"' ( STRING_ESCAPE_SEQ | ~[\\\r\n\f"] )* '"'
 ;

/// longstring      ::=  "'''" longstringitem* "'''" | '"""' longstringitem* '"""'
fragment LONG_STRING
 : '\'\'\'' LONG_STRING_ITEM*? '\'\'\''
 | '"""' LONG_STRING_ITEM*? '"""'
 ;

/// longstringitem  ::=  longstringchar | stringescapeseq
fragment LONG_STRING_ITEM
 : LONG_STRING_CHAR
 | STRING_ESCAPE_SEQ
 ;

/// longstringchar  ::=  <any source character except "\">
fragment LONG_STRING_CHAR
 : ~'\\'
 ;

/// stringescapeseq ::=  "\" <any source character>
fragment STRING_ESCAPE_SEQ
 : '\\' .
 ;

/// nonzerodigit   ::=  "1"..."9"
fragment NON_ZERO_DIGIT
 : [1-9]
 ;

/// digit          ::=  "0"..."9"
fragment DIGIT
 : [0-9]
 ;

/// octdigit       ::=  "0"..."7"
fragment OCT_DIGIT
 : [0-7]
 ;

/// hexdigit       ::=  digit | "a"..."f" | "A"..."F"
fragment HEX_DIGIT
 : [0-9a-fA-F]
 ;

/// bindigit       ::=  "0" | "1"
fragment BIN_DIGIT
 : [01]
 ;

/// pointfloat    ::=  [intpart] fraction | intpart "."
fragment POINT_FLOAT
 : INT_PART? FRACTION
 | INT_PART '.'
 ;

/// exponentfloat ::=  (intpart | pointfloat) exponent
fragment EXPONENT_FLOAT
 : ( INT_PART | POINT_FLOAT ) EXPONENT
 ;

/// intpart       ::=  digit+
fragment INT_PART
 : DIGIT+
 ;

/// fraction      ::=  "." digit+
fragment FRACTION
 : '.' DIGIT+
 ;

/// exponent      ::=  ("e" | "E") ["+" | "-"] digit+
fragment EXPONENT
 : [eE] [+-]? DIGIT+
 ;

/// shortbytes     ::=  "'" shortbytesitem* "'" | '"' shortbytesitem* '"'
/// shortbytesitem ::=  shortbyteschar | bytesescapeseq
fragment SHORT_BYTES
 : '\'' ( SHORT_BYTES_CHAR_NO_SINGLE_QUOTE | BYTES_ESCAPE_SEQ )* '\''
 | '"' ( SHORT_BYTES_CHAR_NO_DOUBLE_QUOTE | BYTES_ESCAPE_SEQ )* '"'
 ;
    
/// longbytes      ::=  "'''" longbytesitem* "'''" | '"""' longbytesitem* '"""'
fragment LONG_BYTES
 : '\'\'\'' LONG_BYTES_ITEM*? '\'\'\''
 | '"""' LONG_BYTES_ITEM*? '"""'
 ;

/// longbytesitem  ::=  longbyteschar | bytesescapeseq
fragment LONG_BYTES_ITEM
 : LONG_BYTES_CHAR
 | BYTES_ESCAPE_SEQ
 ;

/// shortbyteschar ::=  <any ASCII character except "\" or newline or the quote>
fragment SHORT_BYTES_CHAR_NO_SINGLE_QUOTE
 : [\u0000-\u0009]
 | [\u000B-\u000C]
 | [\u000E-\u0026]
 | [\u0028-\u005B]
 | [\u005D-\u007F]
 ; 

fragment SHORT_BYTES_CHAR_NO_DOUBLE_QUOTE
 : [\u0000-\u0009]
 | [\u000B-\u000C]
 | [\u000E-\u0021]
 | [\u0023-\u005B]
 | [\u005D-\u007F]
 ; 

/// longbyteschar  ::=  <any ASCII character except "\">
fragment LONG_BYTES_CHAR
 : [\u0000-\u005B]
 | [\u005D-\u007F]
 ;

/// bytesescapeseq ::=  "\" <any ASCII character>
fragment BYTES_ESCAPE_SEQ
 : '\\' [\u0000-\u007F]
 ;

fragment SPACES
 : [ \t]+
 ;

fragment COMMENT
 : '#' ~[\r\n\f]*
 ;

fragment LINE_JOINING
 : '\\' SPACES? ( '\r'? '\n' | '\r' | '\f' )
 ;

fragment OTHER_ID_START
 : [\u2118\u212E\u309B\u309C]
 ;

/// id_start     ::=  <all characters in general categories Lu, Ll, Lt, Lm, Lo, Nl, the underscore, and characters with the Other_ID_Start property>
fragment ID_START
 : '_'
 | [\p{Letter}\p{Letter_Number}]
 | OTHER_ID_START
 ;

/// id_continue  ::=  <all characters in id_start, plus characters in the categories Mn, Mc, Nd, Pc and others with the Other_ID_Continue property>
fragment ID_CONTINUE
 : ID_START
 | [\p{Nonspacing_Mark}\p{Spacing_Mark}\p{Decimal_Number}\p{Connector_Punctuation}\p{Format}]
 ;
