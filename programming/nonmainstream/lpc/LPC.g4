grammar LPC;

/* Lex */

TypeModifier
    :   'nomask'
    |   'private'
    |   'otected'
    |   'public'
    |   'static'
    |   'varargs'
    ;

Assign
    :   '='
    |   '+='
    |   '-='
    |   '&='
    |   '|='
    |   '^='
    |   '<<='
    |   '>>='
    |   '*='
    |   '%='
    |   '/='
    ;

PlusPlus    :   '++' ;

MinusMinus  :   '--' ;

And         :   '&' ;

AndAnd
    :   '&&'
    ;

Caret
    :   '^'
    ;

Or
    :   '|'
    ;

OrOr
    :   '||'
    ;

Equal
    :   '=='
    ;

LeftShift
    :   '<<'
    ;

RightShift
    :   '>>'
    ;

Not
    :   '!'
    ;

NotEqual
    :   '!='
    ;

Compare
    //:   '<'
    :   LessEqual
    |   Great
    |   GreatEqual
    ;

/*
    [1..<1] did not allow Less
*/
//Less
//    :   '<'
//    ;

LessEqual
    :   '<='
    ;

Great
    :   '>'
    ;

GreatEqual
    :   '>='
    ;

/* Reserved */
Arrow
    :   '->'
    ;

BasicType
    :   'buffer'
    |   'float'
    |   'function'
    |   'int'
    |   'mapping'
    |   'mixed'
    |   'object'
    // |   'status'
    |   'string'
    |   'object_tx'
    |   'void'
    ;

Break
    :   'break'
    ;

Catch
    :   'catch'
    ;

Class
    :   'class'
    ;

Colon
    :   ':'
    ;

ColonColon
    :   '::'
    ;

Continue
    :   'continue'
    ;

DefinedName
    :   'foo'
    ;

Efun
    :   'efun'
    ;

Ellipsis
    :   '...'
    ;

Else
    :   'else'
    ;

If
    :   'if'
    ;

Inherit
    :   'inherit'
    ;

Return
    :   'return'
    ;

For
    :   'for'
    ;

Foreach
    :   'foreach'
    ;

In
    :   'in'
    ;

Switch
    :   'switch'
    ;

Case
    :   'case'
    ;

While
    :   'while'
    ;

Do
    :   'do'
    ;

Default
    :   'default'
    ;

New
    :   'new'
    ;

ParseCommand
    :   'parse_command'
    ;

Question
    :   '?'
    ;

Range
    :   '..'
    ;

SScanf
    :   'sscanf'
    ;

MappingOpen
    :   '(' (Whitespace|Newline)* '['
    ;

ArrayOpen
    :   '(' (Whitespace|Newline)* '{'
    ;

FunctionOpen
    :   '(' Whitespace* ':' {_input.LA(1) != ':'}?    // java
    ;

Number
    :   IntegerConstant
    ;

Parameter
    :   '$' DigitSequence
    ;

/* Pre processing */
ComplexDefine
    :   '#' Whitespace* 'define' (~[\\\r\n] | '\\\\' '\r'? '\n' | '\\'. )*
        -> skip
    ;

ComplexInclude
    :   '#' Whitespace* 'include'  ~[\r\n]*
        -> skip
    ;

ComplexPreprocessor
    :   '#' ~[\r\n]*
        -> skip
    ;

Real
    :   FractionalConstant
    ;

fragment
FractionalConstant
    :   DigitSequence? '.' DigitSequence
    // |   DigitSequence '.'   {self._input.LA(1) != ord('.')}? // python
    |   DigitSequence '.'   {_input.LA(1) != '.'}?    // java
    // |   DigitSequence '.'   {_input->LA(1) != '.'}?    // c++
    ;

DigitSequence
    :   Digit+
    ;


Identifier
    :   IdentifierNondigit
        (   IdentifierNondigit
        |   Digit
        )*
    ;

fragment
IdentifierNondigit
    :   Nondigit
    //|   UniversalCharacterName
    //|   // other implementation-defined characters...
    ;

fragment
Nondigit
    :   [a-zA-Z_]
    ;

fragment
Digit
    :   [0-9]
    ;

fragment
IntegerConstant
    :   DecimalConstant IntegerSuffix?
    |   OctalConstant IntegerSuffix?
    |   HexadecimalConstant IntegerSuffix?
    |   BinaryConstant
    ;

fragment
BinaryConstant
    :   '0' [bB] [0-1]+
    ;

fragment
DecimalConstant
    :   NonzeroDigit Digit*
    ;

fragment
OctalConstant
    :   '0' OctalDigit*
    ;

fragment
HexadecimalConstant
    :   HexadecimalPrefix HexadecimalDigit+
    ;

fragment
HexadecimalPrefix
    :   '0' [xX]
    ;

fragment
NonzeroDigit
    :   [1-9]
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
HexadecimalDigit
    :   [0-9a-fA-F]
    ;

fragment
IntegerSuffix
    :   UnsignedSuffix LongSuffix?
    |   UnsignedSuffix LongLongSuffix
    |   LongSuffix UnsignedSuffix?
    |   LongLongSuffix UnsignedSuffix?
    ;

fragment
UnsignedSuffix
    :   [uU]
    ;

fragment
LongSuffix
    :   [lL]
    ;

fragment
LongLongSuffix
    :   'll' | 'LL'
    ;

String
    :   StringPrefix? '"' SCharSequence? '"'
    ;

StringPrefix
    :   '@'
    ;

CharacterConstant
    :   '\'' SingleChar? '\''
    ;

fragment
SCharSequence
    :   SChar+
    ;

fragment
SChar
    :   ~["\\\r\n]                       // BUG, removed \" from here
    |   EscapeSequence
    |   '\\\n'   // Added line
    |   '\\\r\n' // Added line
    |   '\n'     // lpc want this
    |   '\r\n'   // lpc want this, too
    ;

fragment
SingleChar
    :   '"'
    |   SChar
    ;

fragment
EscapeSequence
    :   SimpleEscapeSequence
    |   OctalEscapeSequence
    |   HexadecimalEscapeSequence
    |   UniversalCharacterName
    ;

fragment
UniversalCharacterName
    :   '\\u' HexQuad
    |   '\\U' HexQuad HexQuad
    ;

fragment
HexQuad
    :   HexadecimalDigit HexadecimalDigit HexadecimalDigit HexadecimalDigit
    ;

fragment
HexadecimalEscapeSequence
    :   '\\x' HexadecimalDigit+
    ;

fragment
OctalEscapeSequence
    :   '\\' OctalDigit
    |   '\\' OctalDigit OctalDigit
    |   '\\' OctalDigit OctalDigit OctalDigit
    ;

fragment
SimpleEscapeSequence
    :   '\\' ['"?abfnrtv\\]
    |   '\\' [^+.[{}\]!@#$%&*()_=\-|/<>]    // WTF: LPC escapes these characters (inface, only warn in lpc)
    ;

TimeExpression
    :   'time_expression'
    ;


BlockComment
    :   '/*' .*? '*/'
        -> skip
    ;

LineComment
    :   '//' ~[\r\n]*
        -> skip
    ;

Whitespace
    :   [ \t]+
        -> skip
    ;

Newline
    :   (   '\r' '\n'?
        |   '\n'
        )
        -> skip
    ;


lpc_program
    :   program EOF
    ;

program
    :   program defination possible_semi_colon
    |   /* empty */
    ;

possible_semi_colon
    :   /* empty */
    |   ';'
    ;

defination
    :   function_defination
    |   data_type name_list ';'
    |   inheritance
    |   type_decl
    |   modifier_change
    ;

function_defination
    :   data_type optional_star identifier '(' argument ')' block_or_semi
    ;

modifier_change
    :   type_modifier_list ':'
    ;

type_modifier_list
    :   /* empty */
    |   TypeModifier type_modifier_list
    ;

type_decl
    :   type_modifier_list Class identifier '{' member_list '}'
    ;

member_list
    :   /* empty */
    |   member_list data_type member_name_list ';'
    ;

member_name_list
    :   member_name
    |   member_name ',' member_name_list
    ;

member_name
    :   optional_star identifier
    ;

name_list
    :   new_name
    |   new_name ',' name_list
    ;

new_name
    :   optional_star identifier
    |   optional_star identifier Assign expr0
    ;

expr0
    :   expr4 Assign expr0
    |   expr0 Question expr0 Colon expr0
    |   expr0 OrOr expr0
    |   expr0 AndAnd expr0
    |   expr0 Or expr0
    |   expr0 Caret expr0
    |   expr0 And expr0
    |   expr0 Equal expr0
    |   expr0 NotEqual expr0
    |   expr0 Compare expr0
    |   expr0 '<' expr0
    |   expr0 LeftShift expr0
    |   expr0 RightShift expr0

    |   expr0 ('*'|'%'|'/') expr0
    |   expr0 ('+'|'-') expr0

    |   cast expr0
    |   PlusPlus expr4
    |   MinusMinus expr4
    |   Not expr0
    |   '~' expr0
    |   '-' expr0
    |   expr4 PlusPlus   /* normal precedence here */
    |   expr4 MinusMinus
    |   expr4
    |   sscanf
    |   parse_command
    |   time_expression
    |   Number
    |   Real
    ;

time_expression
    :   TimeExpression expr_or_block
    ;

expr_or_block
    :   block
    |   '(' comma_expr ')'
    ;

comma_expr
    :   expr0
    |   comma_expr ',' expr0
    ;

parse_command
    :   ParseCommand '(' expr0 ',' expr0 ',' expr0 lvalue_list ')'
    ;

sscanf
    :   SScanf '(' expr0 ',' expr0 lvalue_list ')'
    ;

lvalue_list
    :   /* empty */
    |   ',' expr4 lvalue_list
    ;

cast
    :   '(' basic_type optional_star ')'
    ;

basic_type
    :   atomic_type
    ;

atomic_type
    :   BasicType
    |   Class DefinedName
    ;

expr4
    :   function_call
    |   expr4 function_arrow_call
    |   DefinedName
    |   Identifier
    |   Parameter
    |   '$' '(' comma_expr ')'
    |   expr4 Arrow identifier
    |   expr4 '[' comma_expr Range '<' comma_expr ']'
    |   expr4 '[' comma_expr Range comma_expr ']'
    |   expr4 '[' '<' comma_expr Range comma_expr ']'
    |   expr4 '[' '<' comma_expr Range '<' comma_expr ']'
    |   expr4 '[' comma_expr Range ']'
    |   expr4 '[' '<' comma_expr Range ']'
    |   expr4 '[' '<' comma_expr ']'
    |   expr4 '[' comma_expr ']'
    |   string
    |   CharacterConstant
    |   '(' comma_expr ')'
    |   catch_statement
    |   BasicType '(' argument ')' block
//    |   L_NEW_FUNCTION_OPEN ':' ')'
//    |   L_NEW_FUNCTION_OPEN ',' expr_list2 ':' ')'
    |   FunctionOpen comma_expr ':' ')'
    |   MappingOpen expr_list3 ']' ')'
    |   ArrayOpen expr_list '}' ')'
    ;

catch_statement
    :   Catch expr_or_block
    ;

expr_list
    :   /* empty */
    |   expr_list2
    |   expr_list2 ','
    ;

expr_list3
    :   /* empty */
    |   expr_list4
    |   expr_list4 ','
    ;

expr_list4
    :   assoc_pair
    |   expr_list4 ',' assoc_pair
    ;

assoc_pair
    :   expr0 ':' expr0
    ;

expr_list2
    :   expr_list_node
    |   expr_list2 ',' expr_list_node
    ;

expr_list_node
    :   expr0
    |   expr0 Ellipsis
    ;

string
    :   string_con2
    ;

string_con2
    :   String
    |   string_con2 String
    ;

string_con1
    :   string_con2
    |   '(' string_con1 ')'
    |   string_con1 '+' string_con1
    ;

// combine into expr4
function_call
    :   efun_override '(' expr_list ')'
    |   New '(' expr_list ')'
    |   New '(' Class DefinedName opt_class_init ')'
    |   DefinedName '(' expr_list ')'
    |   function_name_call  //function_name '(' expr_list ')'
    |   function_arrow_call //expr4 Arrow identifier '(' expr_list ')'
    |   '(' '*' comma_expr ')' '(' expr_list ')'
    ;

function_name_call
    :   function_name '(' expr_list ')'
    ;

function_arrow_call
    :   Arrow identifier '(' expr_list ')'
    ;

function_name
    :   Identifier
    |   ColonColon identifier
    |   BasicType ColonColon identifier
    |   identifier ColonColon identifier
    ;

opt_class_init
    :   /* empty */
    |   opt_class_init ',' class_init
    ;

class_init
    :   identifier ':' expr0
    ;

efun_override
    :   Efun ColonColon identifier
    |   Efun ColonColon New
    ;

block_or_semi
    :   block
    |   ';'
    ;

block
    :   '{' statements '}'
    ;

statements
    :   /* empty */
    |   statement statements
    |   local_declare_statement statements
    ;

local_declare_statement
    :   basic_type local_name_list ';'
    ;

local_name_list
    :   new_local_def
    |   new_local_def ',' local_name_list
    ;

new_local_def
    :   optional_star new_local_name
    |   optional_star new_local_name Assign expr0
    ;

new_local_name
    :   Identifier
    |   DefinedName
    ;

statement
    :   comma_expr ';'
    |   cond
    |   while_statement
    |   do_statement
    |   switch_statement
    |   returnStatement

    // decl_block
    |   block
    |   for_loop
    |   foreach_loop

    |   /* empty */ ';'
    |   Break ';'
    |   Continue ';'
    ;

while_statement
    :   While '(' comma_expr ')' statement
    ;

do_statement
    :   Do statement While '(' comma_expr ')' ';'
    ;

switch_statement
    :   Switch '(' comma_expr ')' '{' local_declarations case_statement switch_block '}'
    ;

local_declarations
    :   /* empty */
    |   local_declarations basic_type local_name_list ';'
    ;

case_statement
    :   Case case_label ':'
    |   Case case_label Range case_label ':'
    |   Default ':'
    ;

switch_block
    :   case_statement switch_block
    |   statement switch_block
    |   /* empty */
    ;

case_label
    :   constant
    |   CharacterConstant
    |   string_con1
    ;

constant
    :   constant '|' constant
    |   constant '^' constant
    |   constant '&' constant
    |   constant Equal constant
    |   constant NotEqual constant
    |   constant Compare constant
    |   constant '<' constant
    |   constant LeftShift constant
    |   constant RightShift constant
    |   '(' constant ')'

    |   constant '*' constant
    |   constant '%' constant
    |   constant '/' constant

    |   constant '-' constant
    |   constant '+' constant
    |   Number
    |   '-' Number
    |   Not Number
    |   '~' Number
    ;

//decl_block
//    :
//    |   block
//    |   for_loop
//    |   foreach_loop
//    ;

foreach_loop
    :   Foreach '(' foreach_vars In expr0 ')' statement
    ;

foreach_vars
    :   foreach_var
    |   foreach_var ',' foreach_var
    ;

for_loop
    :   For '(' first_for_expr ';' for_expr ';' for_expr ')' statement
    ;

foreach_var
    :   DefinedName
    |   single_new_local_def
    |   Identifier
    ;

first_for_expr
    :   for_expr
    |   single_new_local_def_with_init
    ;

single_new_local_def_with_init
    :   single_new_local_def Assign expr0
    ;

single_new_local_def
    :   basic_type optional_star new_local_name
    ;

for_expr
    :   /* EMPTY */
    |   comma_expr
    ;

returnStatement
    :   Return ';'
    |   Return comma_expr ';'
    ;

cond
    :   If '(' comma_expr ')' statement optional_else_part
    ;

optional_else_part
    :   /* empty */
    |   Else statement
    ;

argument
    :   /* empty */
    |   argument_list
    |   argument_list Ellipsis
    ;

argument_list
    :   new_arg
    |   argument_list ',' new_arg
    ;

new_arg
    :   basic_type optional_star
    |   basic_type optional_star new_local_name
    |   new_local_name
    ;

inheritance
    :   type_modifier_list Inherit string_con1 ';'
    ;

data_type
    :   type_modifier_list opt_basic_type
    ;

opt_basic_type
    :   basic_type
    |   /* empty */
    ;

optional_star
    :   /* empty */
    |   '*'
    ;

identifier
    :   DefinedName
    |   Identifier
    ;
