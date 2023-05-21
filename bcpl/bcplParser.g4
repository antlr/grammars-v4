parser grammar bcplParser;

options { tokenVocab=bcplLexer; superClass=bcplParserBase; }

program : directive* declaration_part EOF ;

number : Octal_number | Hex_number | Digits | Binary_number ;
identifier : Identifier;
name : identifier ;

// 8.8.2 Operators
mult_op : '*' | '/' | 'REM' ;
add_op : '+' | '-' ;
shift_op : '<<' | '>>' ;
and_op : '&' ;
or_op : '|' ;

// 8.8.3 Expressions

expression : e0 ;

// 8.8.4 Constant-expressions
c_element : Character_constant | number | identifier | 'TRUE' | 'FALSE' | '(' constant_expression ')' | '?' ;
c_mult_E : c_mult_E mult_op c_element | c_element ;
c_add_E : c_add_E add_op c_mult_E | add_op c_mult_E | c_mult_E ;
c_shift_E : c_shift_E shift_op c_add_E | c_add_E ;
c_and_E : c_and_E and_op c_shift_E | c_shift_E ;
constant_expression : constant_expression or_op c_and_E | c_and_E ;

// 8.8.5 List of expressions and identifiers
expression_list : expression (',' expression)* ;
name_list : name (',' name)* ;

// 8.8.6 Declarations
manifest_item : identifier (('='|'EQ') constant_expression)? ;
manifest_list : manifest_item (semi manifest_item)* ;
manifest_declaration : 'MANIFEST' OB manifest_list CB ;
static_declaration : 'STATIC' OB manifest_list CB ;
global_item : identifier (':' constant_expression)? ;
global_list : global_item (semi global_item)* ;
global_declaration : 'GLOBAL' OB global_list CB ;
simple_definition : name_list ('='|'EQ') expression_list ;
vector_definition : identifier ('='|'EQ') 'VEC' constant_expression ;
function_definition : identifier '(' name_list ')' ('='|'EQ') expression | identifier '(' ')' ('='|'EQ') expression ;
routine_definition : identifier '(' name_list ')' 'BE' command | identifier '(' ')' 'BE' command ;
definition : simple_definition | vector_definition | function_definition | routine_definition ;
simultaneous_declaration : 'LET' definition ('AND' definition)* ;
declaration : simultaneous_declaration | manifest_declaration | static_declaration | global_declaration ;

// 8.8.7 Left-hand side expressions
lhse : e0 ;
left_hand_side_list : lhse (',' lhse)* ;

// 8.8.8 Unlabeled commands
assignment : left_hand_side_list assop expression_list ;
simple_command : 'BREAK' | 'LOOP' | 'ENDCASE' | 'RETURN' | 'FINISH' ;
goto_command : 'GOTO' expression ;
routine_command : bexp '(' expression_list ')' | bexp '(' ')' ;
resultis_command : 'RESULTIS' expression ;
switchon_command : 'SWITCHON' expression 'INTO' compound_command ;
repeatable_command : assignment | simple_command | goto_command | routine_command | resultis_command | repeatable_command 'REPEAT' | repeatable_command 'REPEATUNTIL' expression | repeatable_command 'REPEATWHILE' expression | switchon_command | compound_command | block ;
repeated_command : repeatable_command 'REPEAT' | repeatable_command 'REPEATUNTIL' expression | repeatable_command 'REPEATWHILE' expression ;
until_command : 'UNTIL' expression 'DO' command ;
while_command : 'WHILE' expression 'DO' command ;
for_command : 'FOR' identifier ('='|'EQ') expression 'TO' expression ('BY' constant_expression)? 'DO'? command ;
repetitive_command : repeated_command | until_command | while_command | for_command | unless_command ;
test_command : 'TEST' expression ('THEN'|'DO')? command ('ELSE'|'OR') command ;
if_command : 'IF' expression ('THEN'|'DO')? command ;
unless_command : 'UNLESS' expression ('THEN'|'DO')? command ;
unlabelled_command : repeatable_command | repetitive_command | test_command | if_command ;

// 8.8.9 Labeled commands
label_prefix : identifier ':' ;
case_prefix : 'CASE' constant_expression ':' ;
default_prefix : 'DEFAULT' ':' ;
prefix_ : label_prefix | case_prefix | default_prefix ;
command : prefix_* unlabelled_command | prefix_ prefix_* ;

// 8.8.10 Blocks and compound commands
command_list : command (semi command)* ;
declaration_part : declaration (semi declaration)* ;
semi : ';'+ | nl;

block : OB declaration_part semi command_list? CB ;
compound_command : OB command_list? CB ;

// Extended
directive : 'GET' String_constant | 'SECTION' String_constant ;

fname : 'FLT' identifier ;
const : number | Character_constant | 'TRUE' | 'FALSE' | '?' ;
bpat : '+' number | '-' number | 'TRUE' | 'FALSE' | '?' | Character_constant ;
string : String_constant ;
relop : '=' | 'EQ' | '~=' | 'NE' | '<' | '<=' | '>' | '>=' | '#=' | '#~=' | '#<' | '#<=' | '#>' | '#>=' ;
fcond : '->' | '#->' ;
range : '..' | '#..' ;
jcom : 'NEXT' | 'EXIT' | 'BREAK' | 'LOOP' | 'ENDCASE' ;
assop : ':=' | '*:=' | '/:=' | 'MOD:=' | '+:=' | '~:=' | '#:=' | '#*:=' | '#/:=' | '#MOD:=' | '#+:=' | '#~:=' | '<<:=' | '>>:=' | '&:=' | '|:=' | 'EQV:=' | 'NEQV:=' | 'XOR:=' ;
nonl : { IsNotNl() }? ;
mlist : (':' p0? ('->' e0 | 'BE' command))+ '.'? ;
p0 : sp (sp0 | sp1 | p3)* ;
p1 : sp (sp1 | p3)* ;
p2 : sp p3* ;
p3 : p2 ;
sp0 : ',' p1 ;
sp1 : sp0 | '|' p2 ;
sp
 : relop (bpat | '(' e0 ')')
 | jcom
 | '(' p0 ')'
 | '[' p0 ']'
 | bpat (range bpat)?
 | fname
 ;
bexp
 : name
 | const
 | string
 | 'SLCT' e9 (':' e9 (':' e9)? )?
 | 'NEXT' | 'EXIT' | 'BREAK' | 'LOOP' | 'ENDCASE'
 | '(' e0 ')'
 | ('FLOAT' | 'FIX' | '!' | '@') e7
 | ('+' | '-' | 'ABS' | '#+' | '#-' | '#ABS') e5
 | ('NOT'|'~') e3
 | 'TABLE' e0 (',' e0)*
 | ('MATCH' | 'EVERY') '(' e0 (',' e0)* ')' mlist
 | 'VALOF' command
 ;
se9 :'(' (e0 (',' e0)*)? ')' | '#(' e0 (',' e0)* ')' | '[' e0 ']' ;
se8 : ('OF' | '::' | '!' | '%') e8 ;
se6 : ('*' | '/' | 'MOD' | '#*' | '#MOD' | 'REM' | '#REM') e6 ;
se5 : ('=' | 'EQ' | '~=' | 'NE' | '<' | '<=' | '>' | '>=' | '#=' | '#~=' | '#<' | '#<=' | '#>' | '#>=') e4 (relop e4)* | ('+' | '-' | '#+' | '#-') e5 ;
se4 : ('<<' | '>>') e4 ;
se3 : '&' e3 ;
se2 : '|' e2 ;
se1: ('EQV' | 'XOR' | 'NEQV') e1 | fcond e0 ',' e0 ;

e9 : bexp ;
e8 : bexp (nonl se9 )* ;
e7 : bexp (nonl ( se9 | se8 ))* ;
e6 : bexp (nonl ( se9 | se8 ))* ;
e5 : bexp (nonl ( se9 | se8 | se6 ))* ;
e4 : bexp (nonl ( se9 | se8 | se6 | se5 ))* ;
e3 : bexp (nonl ( se9 | se8 | se6 | se5 | se4 ))* ;
e2 : bexp (nonl ( se9 | se8 | se6 | se5 | se4 | se3 ))* ;
e1 : bexp (nonl ( se9 | se8 | se6 | se5 | se4 | se3 | se2 ))* ;
e0 : bexp (nonl ( se9 | se8 | se6 | se5 | se4 | se3 | se2 | se1 ))* ;

nl : { IsNl() }? ;

