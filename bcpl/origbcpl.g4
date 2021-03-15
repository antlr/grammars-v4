grammar bcpl;

// 8.8.1 Identifier, Strings, Numbers.
letter : 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' 
// extended
 | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' 
 ;
octal_digit : '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' ;
hex_digit : '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' ;
digit : '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ;
// extended
string_constant : '"' ~'"'* /* 255 or fewer characters */ '"' ;
character_constant : '\'' one_character '\'' ;
octal_number : '#' octal_digit octal_digit* ;
hex_number : '#X' hex_digit hex_digit* ;
number : octal_number | hex_number | digit digit* ;
identifier : letter (letter | digit | '.')* ;

// missing
one_character : letter | digit ;
name : identifier ;

// 8.8.2 Operators
address_op : '@' | '!' ;
mult_op : '*' | '/' | 'REM' ;
add_op : '+' | '-' ;
rel_op : '=' | '¬=' | '<=' | '>=' | '<' | '>' ;
shift_op : '<<' | '>>' ;
and_op : '&' ;
or_op : '|' ;
eqv_op : 'EQV' | 'NEQV' ;
not_op : '¬' ;

// 8.8.3 Expressions
element : character_constant | string_constant | number | identifier | 'TRUE' | 'FALSE' ;
primary_E : primary_E '(' expression_list ')' | primary_E '(' ')' | '(' expression ')' | element ;
vector_E : vector_E '!' primary_E | primary_E ;
address_E : address_op address_E | vector_E ;
mult_E : mult_E mult_op address_E | address_E ;
add_E : add_E add_op mult_E | add_op mult_E | mult_E ;
rel_E : add_E (rel_op add_E)* ;
shift_E : shift_E shift_op add_E | rel_E ;
not_E : not_op shift_E | shift_E ;
and_E : not_E (and_op not_E)* ;
or_E : and_E (or_op and_E)* ;
eqv_E : or_E (eqv_op or_E)* ;
conditional_E : eqv_E '->' conditional_E ',' conditional_E | eqv_E ;
expression : conditional_E | 'TABLE' constant_expression (',' constant_expression)* | 'VALOF' command ;

// 8.8.4 Constant-expressions
c_element : character_constant | number | identifier | 'TRUE' | 'FALSE' | '(' constant_expression ')' ;
c_mult_E : c_mult_E mult_op c_element | c_element ;
c_add_E : c_add_E add_op c_mult_E | add_op c_mult_E | c_mult_E ;
c_shift_E : c_shift_E shift_op c_add_E | c_add_E ;
c_and_E : c_and_E and_op c_shift_E | c_shift_E ;
constant_expression : constant_expression or_op c_and_E | c_and_E ;

// 8.8.5 List of expressions and identifiers
expression_list : expression (',' expression)* ;
name_list : name (',' name)* ;

// 8.8.6 Declarations
manifest_item : identifier '=' constant_expression ;
manifest_list : manifest_item (';' manifest_item)* ;
manifest_declaration : 'MANIFEST' '$(' manifest_list '$)' ;
static_declaration : 'STATIC' '$(' manifest_list '$)' ;
global_item : identifier ':' constant_expression ;
global_list : global_item (';' global_item)* ;
global_declaration : 'GLOBAL' '$(' global_list '$)' ;
simple_definition : name_list '=' expression_list ;
vector_definition : identifier '=' 'VEC' constant_expression ;
function_definition : identifier '(' name_list ')' '=' expression | identifier '(' ')' '=' expression ;
routine_definition : identifier '(' name_list ')' 'BE' command | identifier '(' ')' 'BE' command ;
definition : simple_definition | vector_definition | function_definition | routine_definition ;
simultaneous_declaration : 'LET' definition ('AND' definition)* ;
declaration : simultaneous_declaration | manifest_declaration | static_declaration | global_declaration ;

// 8.8.7 Left-hand side expressions
lhse : identifier | vector_E '!' primary_E | '!' primary_E ;
left_hand_side_list : lhse (',' lhse)* ;

// 8.8.8 Unlabeled commands
assignment : left_hand_side_list ':=' expression_list ;
simple_command : 'BREAK' | 'LOOP' | 'ENDCASE' | 'RETURN' | 'FINISH' ;
goto_command : 'GOTO' expression ;
routine_command : primary_E '(' expression_list ')' | primary_E '(' ')' ;
resultis_command : 'RESULTIS' expression ;
switchon_command : 'SWITCHON' expression 'INTO' compound_command ;
repeatable_command : assignment | simple_command | goto_command | routine_command | resultis_command | repeated_command | switchon_command | compound_command | block ;
repeated_command : repeatable_command 'REPEAT' | repeatable_command 'REPEATUNTIL' expression | repeatable_command 'REPEATWHILE' expression ;
until_command : 'UNTIL' expression 'DO' command ;
while_command : 'WHILE' expression 'DO' command ;
for_command : 'FOR' identifier '=' expression 'TO' expression 'BY' constant_expression 'DO' command | 'FOR' identifier '=' expression 'TO' expression 'DO' command ;
repetitive_command : repeated_command | until_command | while_command | for_command ;
test_command : 'TEST' expression 'THEN' command 'ELSE' command ;
if_command : 'IF' expression 'THEN' command ;
unless_command : 'UNLESS' expression 'THEN' command ;
unlabelled_command : repeatable_command | repetitive_command | test_command | if_command ;

// 8.8.9 Labeled commands
label_prefix : identifier ':' ;
case_prefix : 'CASE' constant_expression ':' ;
default_prefix : 'DEFAULT' ':' ;
prefix : label_prefix | case_prefix | default_prefix ;
command : unlabelled_command | prefix command | prefix ;

// 8.8.10 Blocks and compound commands
command_list : command (';' command)* ;
declaration_part : declaration (';' declaration)* ;
block : '$(' declaration_part ';' command_list '$)' ;
compound_command : '$(' command_list '$)' ;
// Extended
// program : declaration_part ;
program : (declaration_part | directive) ;

// Extended
directive : 'GET' string_constant | 'SECTION' string_constant ;

// Extended

Comment : ('/*' .*? '*/' | '//' ~('\n' | '\r')*) -> channel(HIDDEN) ;
WS : [ \r\n] -> channel(HIDDEN) ;

Rem : 'REM' ;
Eqv : 'EQV' ;
Neqv : 'NEQV' ;
True : 'TRUE' ;
False : 'FALSE' ;
Table : 'TABLE' ;

Left_dollar_open : '$(' ;
