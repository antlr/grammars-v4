/*
BSD License

Copyright (c) 2020, Tom Everett
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Tom Everett nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

grammar pl1;

program
   : block '.'
   ;




statements	=	{ "(" { condition-prefix }","+ ")" ":" }* { label-prefix ":" }*
[ statement_body ] ";"




percent-statements	=	"%" { label-prefix ":" }* statement_body ";"




attribute	=	( "BINARY" | "DECIMAL" )

attribute	=	"TASK"

attribute	=	"CONDITION"

attribute	=	"PRINT"

attribute	=	"KEYED"

attribute	=	"EXCLUSIVE"

attribute	=	"ENVIRONMENT" "(" characteristic-list ")"

attribute	=	"BACKWARDS"

attribute	=	( "BUFFERED" | "UNBUFFERED" )

attribute	=	( "DIRECT" | "SEQUENTIAL" | "TRANSIENT" )

attribute	=	( "INPUT" | "OUTPUT" | "UPDATE" )

attribute	=	( "RECORD" | "STREAM" )

attribute	=	"FILE"

attribute	=	generic-name "GENERIC"
"(" { entry-reference "WHEN" "(" { generic-descriptor }","* ")" }","+ ")"

attribute	=	"BUILTIN"

attribute	=	[ ( "IRREDUCIBLE" | "REDUCIBLE" ) ]

attribute	=	"RETURNS" "(" { attribute }+ ")"

attribute	=	"OPTIONS" "(" characteristic-list ")"

attribute	=	"ENTRY" [ "(" [ parameter-descriptor-list ] ")" ]

attribute	=	"INITIAL" "(" { item }","+ ")"

attribute	=	"CONNECTED"

attribute	=	"POSITION" "(" expression ")"

attribute	=	"DEFINED" ( reference | "(" reference ")" )

attribute	=	"OFFSET" [ "(" area-variable ")" ]

attribute	=	"AREA" [ ( "(" "*" ")" | "(" expression [ "REFER" "(" variable ")" ] ")" ) ]

attribute	=	"POINTER"

attribute	=	"BASED" [ "(" locator-reference ")" ]

attribute	=	"CONTROLLED"

attribute	=	"AUTOMATIC"

attribute	=	"STATIC"

attribute	=	( "INTERNAL" | "EXTERNAL" )

attribute	=	"LIKE" structure-variable

attribute	=	"(" { bound }","+ ")"

attribute	=	( "ALIGNED" | "UNALIGNED" )

attribute	=	"VARIABLE"

attribute	=	"LABEL" [ "(" { label-constant }","+ ")" ]

attribute	=	"EVENT"

attribute	=	( "FIXED" | "FLOAT" )

attribute	=	"(" number-of-digits [ "," scaling-factor ] ")"

attribute	=	( "REAL" | "COMPLEX" )

attribute	=	"BIT" [ "(" length [ "REFER" "(" variable ")" ] ")" ]

attribute	=	"CHARACTER" [ "(" length [ "REFER" "(" variable ")" ] ")" ]

attribute	=	"GRAPHIC" [ "(" length [ "REFER" "(" variable ")" ] ")" ]

attribute	=	"VARYING"

attribute	=	"PICTURE" "'" picture-specification "'"




constant	=	"'" { ( character | """ ) }* "'"

constant	=	"'" { hex_digit hex_digit }* "'X"

constant	=	"'" { ( "0" | "1" ) }* "'B"

constant	=	"'" { hex_digit }* ( "'BX" | "'B4" )

constant	=	"'" { graphic-symbol }* "'G"

constant	=	"'" { hex_digit hex_digit hex_digit hex_digit }* "'GX"

constant	=	"'" { ( character | "'" "'" | "<" { graphic_symbol }+ ">" ) }* "'M"




attribute-bound	=	( [ lower-bound ":" ] upper-bound | "*" )




attribute-upper-bound	=	expression [ "REFER" "(" variable ")" ]




attribute-lower-bound	=	expression [ "REFER" "(" variable ")" ]




expression	=	unary-expression { infix-operator unary-expression }*




expression-unary-expression	=	{ prefix-operator }* elementary-expression




expression-elementary-expression	=	( "(" expression ")" | reference | constant | isub6 )




expression-reference	=	[ locator-qualifier ] basic-reference
[ "(" subscript-list ")" ] [ "(" [ argument-list ] ")" ]




expression-locator-qualifier	=	reference "->"




expression-subscript-list	=	{ ( expression | "*" ) }","+




expression-argument-list	=	{ expression }","+




expression-basic-reference	=	[ structure-qualification ] name




expression-structure-qualification	=	basic-reference [ "(" subscript-list ")" ]




statement	=	"FETCH" { entry-constant }","+ ";"

statement	=	"%" ";"

statement	=	"%" { label ":" }* "NOTE" "(" message [ "," code ] ")" ";"

statement	=	"%" { label ":" }* "IF" preprocessor_expression
"%" "THEN" preprocessor-unit1 [ "%" "ELSE" preprocessor-unit2 ]

statement	=	"%" { label ":" }* "GO_TO" label ";"

statement	=	"%" { label ":" }* "END" [ label ] ";"

statement	=	"%" { label ":" }* "DO" preprocessor-do-specification ";"

statement	=	"%" { label ":" }* "DO" ";"

statement	=	"%" { label ":" }* "DECLARE" { identifier ( "FIXED" | "CHARACTER" | "ENTRY" | "BUILTIN" ) }","+ ";"

statement	=	"%" { label ":" }* "DEACTIVATE" { identifier }","+ ";"

statement	=	"%" { label ":" }*
preprocessor-variable "=" preprocessor_expression ";"

statement	=	"%" { label ":" }* "ACTIVATE" { identifier [ ( "RESCAN" | "NORESCAN" ) ] }","+ ";"

statement	=	{ label ":" }* "RETURN" "(" preprocessor-expression ")" ";"

statement	=	"%" { entry_name ":" }+ "PROCEDURE" [ "(" { parameter }","+ ")" ]
[ "STATEMENT" ] "RETURNS" "(" ( "CHARACTER" | "FIXED" ) ")" ";"

statement	=	"EXIT" ";"

statement	=	"%" "SKIP" [ "(" n ")" ] ";"

statement	=	"%" "PRINT" ";"

statement	=	"%" "PAGE" ";"

statement	=	"%" "NOPRINT" ";"

statement	=	"%" "INCLUDE" { ( dataset "(" member-name ")" | member-name ) }","+ ";"

statement	=	"SIGNAL" condition ";"

statement	=	"REVERT" condition ";"

statement	=	"ON" condition [ "SNAP" ] ( "SYSTEM" ";" | on-unit )

statement	=	{ label ":" }+ "FORMAT" "(" format-list ")" ";"

statement	=	"PUT-STRING" "(" character-reference ")" data-specification ";"

statement	=	"PUT" [ "FILE" "(" file_reference ")" ] [ data_specification ]
[ ( "PAGE" [ "LINE" "(" expression ")" ] | "SKIP" [ "(" expression ")" ] | "LINE" "(" expression ")" ) ] ";"

statement	=	"GET-STRING" "(" expression ")" data-specification ";"

statement	=	"GET" [ "FILE" "(" file-reference ")" ] [ data-specification ]
[ "COPY" "(" file-reference ")" ]
[ "SKIP" [ "(" expression ")" ] ] ";"

statement	=	"UNLOCK" "FILE" "(" file-reference ")" "KEY" "(" expression ")" ";"

statement	=	"DELETE" "FILE" "(" file-reference ")" [ "KEY" "(" expression ")" ]
[ "EVENT" "(" event_reference ")" ] ";"

statement	=	"LOCATE" based-variable
( "FILE" "(" file_reference ")" | "SET" "(" pointer_reference | "KEYFROM" "(" expression ")" ) ";"

statement	=	"REWRITE_FILE" "(" file_reference ")" [ "FROM" "(" reference ")" ]
[ "KEY" "(" expression ")" ] [ "EVENT" "(" event_reference ] ";"

statement	=	"WRITE" "FILE" "(" file_reference ")" "FROM" "(" reference ")"
[ ( "KEYFROM" "(" expression ")" | "KEYTO" "(" reference ")" ) ] [ "EVENT" "(" event_reference ] ";"

statement	=	"READ" "FILE" "(" file_reference ")"
( "INTO" "(" ref ")" [ ( "KEY" "(" expression ")" [ "NOLOCK" ] | "KEYTO" "(" reference ")" ) ] | "SET" "(" pointer_ref ")" [ ( "KEY" "(" expression ")" | "KEYTO" "(" reference ")" ) ] | "IGNORE" "(" expression ")" )
[ "EVENT" "(" reference ")" ] ";"

statement	=	"CLOSE"
{ "FILE" "(" file_reference ")" [ "ENVIRONMENT" "(" ( "LEAVE" | "REREAD" ) ")" ] }","+ ";"

statement	=	"OPEN" { options-group }","+ ";"

statement	=	"RETURN" [ "(" expression ")" ] ";"

statement	=	"CALL" ( entry-reference | generic-name | built-in-name ) [ "(" { argument }","* ")" ]
[ "TASK" [ "(" task_reference ")" ] ] [ "EVENT" "(" event-reference ")" ]
[ "PRIORITY" "(" expression ")" ] ";"

statement	=	( "COBOL" | "FORTRAN" ) [ "NOMAP" [ "(" parameter-list ")" ] ]
[ "NOMAPIN" [ "(" parameter_list ")" ] ]
[ "NOMAPOUT" [ "(" parameter_list ")" ] ]

statement	=	( "MAIN" [ "NOEXECOPS" ] [ "REENTRANT" ] [ "TASK" ] | "COBOL" [ additional_options ] | "FORTRAN" [ additional_options ] )

statement	=	{ entry-constant ":" }+ "ENTRY" [ "(" { parameter }","+ ")" ]
[ "RETURNS" "(" { attribute }+ ")" ]
[ "OPTIONS" "(" characteristic-list ")" ] [ ( "REDUCIBLE" | "IRREDUCIBLE" ) ] ";"

statement	=	{ entry-constant ":" }+ "PROCEDURE" [ "(" { parameter }","+ ")" ]
[ "RETURNS" "(" { attribute }+ ")" ]
[ "OPTIONS" "(" characteristic-list ")" ] [ ( "REDUCIBLE" | "IRREDUCIBLE" ) ]
[ "RECURSIVE" ] [ ( "ORDER" | "REORDER" ) ] [ ( "CHARGRAPHIC" | "NOCHARGRAPHIC" ) ] ";"

statement	=	"WAIT" "(" { event-reference }","+ ")" [ "(" expression ")" ] ";"

statement	=	"STOP" ";"

statement	=	"SELECT" [ "(" exp1 ")" ] ";"
{ "WHEN" "(" { exp2 }","+ ")" unit }* [ "OTHERWISE" unit ]

statement	=	";"

statement	=	"LEAVE" [ label-constant ] ";"

statement	=	"IF" expression "THEN" unit1 [ "ELSE" unit2 ]

statement	=	( "GO" "TO" | "GOTO" ) label_reference ";"

statement	=	"END" [ statement-label ] ";"

statement	=	"DO" reference "=" { specification }","+ ";"

statement	=	"DO" ( "WHILE" "(" exp4 ")" [ "UNTIL" "(" exp5 ")" ] | "UNTIL" "(" exp5 ")" [ "WHILE" "(" exp4 ")" ] ) ";"

statement	=	"DO" ";"

statement	=	"DISPLAY" "(" expression ")"
( "REPLY" "(" character_reference ")" [ "EVENT" "(" event_reference ")" ] | "EVENT" "(" event_reference ")" reply "(" character_reference ")" ) ";"

statement	=	"DISPLAY" "(" expression ")" ";"

statement	=	"DELAY" "(" expression ")" ";"

statement	=	{ reference }","+ "=" expression [ "," "BY" "NAME" ] ";"

statement	=	"FREE" { option }","+ ";"

statement	=	"ALLOCATE" { based_variable [ reference ] }","+ ";"

statement	=	"FREE" { controlled-variable }","+ ";"

statement	=	"ALLOCATE" { [ level ] controlled-variable [ attribute ] }","+

statement	=	"RELEASE" { entry-constant }+ ";"

statement	=	"BEGIN" [ ( "ORDER" | "REORDER" ) ] [ ( "CHARGRAPHIC" | "NOCHARGRAPHIC" ) ] ";"

statement	=	"DECLARE" { [ level ] name { attribute }* [ "SYSTEM" ] }","+ ";"

statement	=	"DEFAULT" { ( simple_specification | factored_specification ) }","+ ";"




statement-simple-specification	=	( "RANGE" "(" { ( { letter }+ | letter ":" letter ) }+ ")" [ attribute_specification ] | "RANGE" "(" "*" ")" [ attribute_specification ] | "DESCRIPTORS" [ attribute_specifications ] )




statement-factored-specification	=	"(" { ( simple_specification | factored_specification ) }","+ ")" [ attribute_specification ]




statement-attribute-specification	=	( { attribute }+ [ "VALUE" "(" { value_specification }","+ ")" ] | "VALUE" "(" { value_specification }","+ ")" )




statement-value-specification	=	( "AREA" "(" size ")" | "BIT" "(" length ")" | "CHARACTER" "(" length ")" | "GRAPHIC" "(" length ")" | additional_options )




statement-additional-options	=	[ ( "REAL" | "COMPLEX" ) ] [ ( "FIXED" | "FLOAT" ) ] [ ( "BINARY" | "DECIMAL" ) ]
[ "(" precision [ "," scaling_factor ] ")" ]

statement-additional-options	=	[ "NOMAP" [ "(" parameter_list ")" ] ]
[ "NOMAPIN" [ "(" parameter_list ")" ] ]
[ "NOMAPOUT" [ "(" parameter_list ")" ] ]



statement-attribute	=	( dimension | "CHARACTER" "(" length | "BIT" "(" length ")" | "GRAPHIC" "(" length ")" | "AREA" "(" size ")" | "INITIAL" "(" { "ITEM" }","+ ")" | initial_call_entry_reference [ "(" { argument }","+ ")" ] )



locator-qualification	=	locator-reference "->" { based-locator-reference "->" }*
based-reference



statement-reference	=	[ "IN" "(" area_reference ")" ] [ "SET" "(" locator_reference ")" ]



statement-option	=	[ locator-qualifier ">" ] based-variable
[ "IN" "(" area-reference ")" ]



option	=	expression "REFER" "(" variable ")"

option	=	"PRIORITY" "(" expression ")"

option	=	"EVENT" "(" event-reference ")"

option	=	"TASK" [ "(" task-reference ")" ]



attribute-item	=	( "*" | initial_constant | reference | "(" expression ")" | iteration_specification )



attribute-iteration_specification	=	"(" iteration_factor ")" iteration_item



attribute-iteration_item	=	( "*" | initial_constant | reference | "(" { item }","+ ")" )



attribute-initial-constant	=	( [ ( "+" | "-" ) ] arithmetic_constant | bit_constant | character_constant | graphic_constant | entry_constant | file_constant | label_constant | [ ( "+" | "-" ) ] real_constant ( "+" | "-" ) imaginary_constant )

attribute-initial-constant	=	"INITIAL" "CALL" entry-reference [ "(" { argument }","+ ")" ]



statement-specification	=	exp1 [ ( "TO" exp2 [ "BY" exp3 ] | "BY" exp3 [ "TO" exp2 ] | "REPEAT" exp6 ) ]
[ ( "WHILE" "(" exp4 ")" [ "(" "UNTIL" "(" exp5 ")" ] | "UNTIL" "(" exp5 ")" [ "(" "WHILE" "(" exp4 ")" ] ) ]



entry-data	=	"ENTRY" [ "(" parameter-descriptor-list ")" ]
[ "RETURNS" "(" attribute-list ")" ]
[ "OPTIONS" "(" characteristic-list ")" ] [ "EXTERNAL" ]
[ ( "IRREDUCIBLE" | "REDUCIBLE" ) ]

entry-data	=	"ENTRY" [ "(" parameter_descriptor_list ")" ] [ "VARIABLE" ]
[ "RETURNS" "(" attribute_list ")" ]
[ "OPTIONS" "(" characteristic_list ")" ] [ ( "IRREDUCIBLE" | "REDUCIBLE" ) ]
[ scope-attribute ] [ storage-attribute ]
[ alignment-attribute ] [ parameter ]
[ "DEFINED" ] [ "INITIAL" ]




attribute-parameter-descriptor-list	=	{ parameter-descriptor }","+




attribute-parameter-descriptor	=	[ ( { attribute }+ | "*" | structure-parameter-descriptor ) ]




attribute-structure-parameter-descriptor	=	"1" { attribute }* "," { level { attribute }* }","+




attribute-characteristic-list	=	( "COBOL" | "FORTRAN" | "ASSEMBLER" ) [ additional-options ]




attribute-additional-options	=	[ "NOMAP" [ "(" { argi }","+ ")" ] ] [ "NOMAPIN" [ "(" { argi }+ ")" ] ]
[ "NOMAPOUT" [ "(" { argi }","+ ")" ] ] [ "INTER" ] [ "RETCODE" ]



built-in-function	=	name [ "(" { argument }","* ")" ]

built-in-function	=	"PARMSET" "(" x ")"

built-in-function	=	"COUNTER"

built-in-function	=	"COMPILETIME"

built-in-function	=	"VERIFY" "(" x "," y ")"

built-in-function	=	"UNSPEC" "(" x ")"

built-in-function	=	"TRUNC" "(" x ")"

built-in-function	=	"TRANSLATE" "(" x "," y [ "," z ] ")"

built-in-function	=	"TIME" [ "(" ")" ]

built-in-function	=	"TANH" "(" x ")"

built-in-function	=	"TAND" "(" x ")"

built-in-function	=	"TAN" "(" x ")"

built-in-function	=	"SYSNULL" [ "(" ")" ]

built-in-function	=	"SUM" "(" x ")"

built-in-function	=	"SUBSTR" "(" x "," y [ "," z ] ")"

built-in-function	=	"STRING" "(" x ")"

built-in-function	=	"STORAGE" "(" x ")"

built-in-function	=	"STATUS" [ "(" x ")" ]

built-in-function	=	"SQRT" "(" x ")"

built-in-function	=	"SINH" "(" x ")"

built-in-function	=	"SIND" "(" x ")"

built-in-function	=	"SIN" "(" x ")"

built-in-function	=	"SIGN" "(" x ")"

built-in-function	=	"SAMEKEY" "(" x ")"

built-in-function	=	"ROUND" "(" x "," y ")"

built-in-function	=	"REPEAT" "(" x "," y ")"

built-in-function	=	"REAL" "(" x ")"

built-in-function	=	"PROD" "(" x ")"

built-in-function	=	"PRIORITY" "(" x ")"

built-in-function	=	"PRECISION" "(" x "," p [ "," q ] ")"

built-in-function	=	"POLY" "(" x "," y ")"

built-in-function	=	"POINTERVALUE" "(" x ")"

built-in-function	=	"POINTERADD" "(" x "," y ")"

built-in-function	=	"POINTER" "(" x "," y ")"

built-in-function	=	"PLIRETV" [ "(" ")" ]

built-in-function	=	"ONSOURCE" [ "(" ")" ]

built-in-function	=	"ONLOC" [ "(" ")" ]

built-in-function	=	"ONKEY" [ "(" ")" ]

built-in-function	=	"ONFILE" [ "(" ")" ]

built-in-function	=	"ONCOUNT" [ "(" ")" ]

built-in-function	=	"ONCODE" [ "(" ")" ]

built-in-function	=	"ONCHAR" [ "(" ")" ]

built-in-function	=	"OFFSET" "(" x "," y ")"

built-in-function	=	"NULL" [ "(" ")" ]

built-in-function	=	"MULTIPLY" "(" x "," y "," p [ "," q ] ")"

built-in-function	=	"MPSTR" "(" x "," r [ "," y ] ")"

built-in-function	=	"MOD" "(" x "," y ")"

built-in-function	=	"MIN" "(" { x }","+ ")"

built-in-function	=	"MAX" "(" { x }","+ ")"

built-in-function	=	"LOW" "(" x ")"

built-in-function	=	"LOG10" "(" x ")"

built-in-function	=	"LOG2" "(" x ")"

built-in-function	=	"LOG" "(" x ")"

built-in-function	=	"LINENO" "(" x ")"

built-in-function	=	"LENGTH" "(" x ")"

built-in-function	=	"LBOUND" "(" x "," y ")"

built-in-function	=	"INDEX" "(" x "," y ")"

built-in-function	=	"IMAG" "(" x ")"

built-in-function	=	"HIGH" "(" x ")"

built-in-function	=	"HBOUND" "(" x "," y ")"

built-in-function	=	"GRAPHIC" "(" x [ "," y ] ")"

built-in-function	=	"FLOOR" "(" x ")"

built-in-function	=	"FLOAT" "(" x [ "," p ] ")"

built-in-function	=	"FIXED" "(" x [ "," p [ "," q ] ] ")"

built-in-function	=	"EXP" "(" x ")"

built-in-function	=	"ERFC" "(" x ")"

built-in-function	=	"ERF" "(" x ")"

built-in-function	=	"ENTRYADDR" "(" x ")"

built-in-function	=	"EMPTY" [ "(" ")" ]

built-in-function	=	"DIVIDE" "(" x "," y "," p [ "," q ] ")"

built-in-function	=	"DIM" "(" x "," y ")"

built-in-function	=	"DECIMAL" "(" x [ "," p [ "," q ] ] ")"

built-in-function	=	"DATETIME" [ "(" ")" ]

built-in-function	=	"DATE" [ "(" ")" ]

built-in-function	=	"DATAFIELD" [ "(" ")" ]

built-in-function	=	"CURRENTSTORAGE" "(" x ")"

built-in-function	=	"COUNT" "(" x ")"

built-in-function	=	"COSH" "(" x ")"

built-in-function	=	"COSD" "(" x ")"

built-in-function	=	"COS" "(" x ")"

built-in-function	=	"CONJG" "(" x ")"

built-in-function	=	"COMPLEX" "(" x "," y ")"

built-in-function	=	"COMPLETION" "(" x ")"

built-in-function	=	"CHAR" "(" x [ "," y ] ")"

built-in-function	=	"CEIL" "(" x ")"

built-in-function	=	"BOOL" "(" x "," y "," z ")"

built-in-function	=	"BIT" "(" x [ "," y ] ")"

built-in-function	=	"BINARYVALUE" "(" x ")"

built-in-function	=	"BINARY" "(" x [ "," p [ "," q ] ] ")"

built-in-function	=	"ATANH" "(" x ")"

built-in-function	=	"ATAND" "(" x [ "," y ] ")"

built-in-function	=	"ATAN" "(" x [ "," y ] ")"

built-in-function	=	"ASIN" "(" x ")"

built-in-function	=	"ANY" "(" x ")"

built-in-function	=	"ALLOCATION" "(" x ")"

built-in-function	=	"ALL" "(" x ")"

built-in-function	=	"ADDR" "(" x ")"

built-in-function	=	"ADD" "(" x "," y "," p [ "," q ] ")"

built-in-function	=	"ACOS" "(" x ")"

built-in-function	=	"ABS" "(" x ")"




built-in-subroutine	=	call-name [ "(" { argument }","* ")" ] ";"

built-in-subroutine	=	"PLITEST" [ "(" command-list ")" ]

built-in-subroutine	=	"PLITDLI" "(" { argument }","+ ")"

built-in-subroutine	=	"PLISRTD" "(" { argument }","+ ")"

built-in-subroutine	=	"PLISRTC" "(" { argument }","+ ")"

built-in-subroutine	=	"PLISRTB" "(" { argument }","+ ")"

built-in-subroutine	=	"PLISRTA" "(" { argument }","+ ")"

built-in-subroutine	=	"PLIRETC" "(" return-code ")"

built-in-subroutine	=	"PLIREST"

built-in-subroutine	=	"PLIDUMP" "(" { argument }","+ ")"

built-in-subroutine	=	"PLICKPT" "(" { argument }","+ ")"

built-in-subroutine	=	"PLICANC"

built-in-subroutine	=	"ASMTDLI" "(" { argument }","+ ")"




pseudovariable	=	"COMPLETION" "(" x ")"

pseudovariable	=	"COMPLEX" "(" x "," y ")"

pseudovariable	=	"ENTRYADDR" "(" x ")"

pseudovariable	=	"IMAG" "(" x ")"

pseudovariable	=	"ONCHAR" [ "(" ")" ]

pseudovariable	=	"ONSOURCE" [ "(" ")" ]

pseudovariable	=	"PRIORITY" [ "(" x ")" ]

pseudovariable	=	"REAL" "(" x ")"

pseudovariable	=	"STATUS" [ "(" x ")" ]

pseudovariable	=	"STRING" "(" x ")"

pseudovariable	=	"SUBSTR" "(" x "," y [ "," z ] ")"

pseudovariable	=	"UNSPEC" "(" x ")"



statement-options-group	=	"FILE" "(" file-reference ")" [ ( "STREAM" | "RECORD" ) ]
[ ( "INPUT" | "OUTPUT" | "UPDATE" ) ] [ ( "DIRECT" | "SEQUENTIAL" | "TRANSIENT" ) ] [ ( "BUFFERED" | "UNBUFFERED" ) ]
[ "BACKWARDS" ] [ "EXCLUSIVE" ] [ "KEYED" ]
[ "PRINT" ] [ "TITLE" "(" expression ")" ]
[ "LINESIZE" "(" expression ")" ]
[ "PAGESIZE" "(" expression ")" ]




data-specification	=	( [ "LIST" ] "(" data-specification-data-list ")" | "DATA" [ "(" { data-specification-data-list }","+ ")" ] | "EDIT" { "(" data-specification-data-list ")" "(" data-specification-format-list ")" }+ )




data-specification-data-list	=	( { data-list-item }","+ | { "(" data-list-item-type-3-do ")" }+ )




data-specification-format-list	=	{ ( format-item | n-format-item | n "(" format-list ")" ) }","+




list-directed-data-value	=	( [ ( "+" | "-" ) ] arithmetic-constant | [ ( "+" | "-" ) ] real-constant ( "+" | "-" ) imaginary-constant | character-constant | bit-constant | graphic-constant )




data-directed-data-specification	=	"DATA" [ "(" data_list ")" ]




data-directed-element-assignment	=	{ { element-variable "=" data-value }+ }","+ ";"




edit-directed-data-specification	=	"EDIT" { "(" data_list ")" "(" format_list ")" }+




edit-directed-data-specification-format-list	=	( { format_item }","+ | { n_format_item }","+ | { n "(" format_list ")" }","+ )




format-item	=	"A" [ "(" field-width ")" ]

format-item	=	"B" [ "(" field-width ")" ]

format-item	=	"C" "(" { real-format-item }","+ ")"

format-item	=	"COLUMN" "(" character-position ")"

format-item	=	"E" "(" field-width "," fractional-digits
[ "," significant-digits ] ")"

format-item	=	"F" "(" field-width
[ "," fractional-digits [ "," scaling-factor ] ] ")"

format-item	=	"G" [ "(" field-width ")" ]

format-item	=	"LINE" "(" line-number ")"

format-item	=	"P" "'" picture-specification "'"

format-item	=	"PAGE"

format-item	=	"R" "(" label-reference ")"

format-item	=	"SKIP" [ "(" relative-line ")" ]

format-item	=	"X" "(" field-width ")"




scaling-factor-character	=	"F" "(" [ ( "+" | "-" ) ] integer ")"




condition	=	"AREA"

condition	=	"ATTENTION"

condition	=	"CHECK" [ "(" { name }","+ ")" ]

condition	=	"CONDITION" "(" name ")"

condition	=	"CONVERSION"

condition	=	"ENDFILE" "(" file-reference ")"

condition	=	"ENDPAGE" "(" file-reference ")"

condition	=	"ERROR"

condition	=	"FINISH"

condition	=	"FIXEDOVERFLOW"

condition	=	"KEY" "(" file-reference ")"

condition	=	"NAME" "(" file-reference ")"

condition	=	"OVERFLOW"

condition	=	"PENDING" "(" file-reference ")"

condition	=	"RECORD" "(" file-reference ")"

condition	=	"SIZE"

condition	=	"STRINGRANGE"

condition	=	"STRINGSIZE"

condition	=	"SUBSCRIPTRANGE"

condition	=	"TRANSMIT" "(" file-reference ")"

condition	=	"UNDEFINEDFILE" "(" file-reference ")"

condition	=	"UNDERFLOW"

condition	=	"ZERODIVIDE"




statement-preprocessor-do-specification	=	preprocessor-variable "=" preprocessor-exp1
[ ( "TO" preprocessor-exp2 [ "BY" preprocessor-exp3 ] | "BY" preprocessor-exp3 [ "TO" preprocessor-exp2 ] ) ]



fragment A
   : ('a' | 'A')
   ;


fragment B
   : ('b' | 'B')
   ;


fragment C
   : ('c' | 'C')
   ;


fragment D
   : ('d' | 'D')
   ;


fragment E
   : ('e' | 'E')
   ;


fragment F
   : ('f' | 'F')
   ;


fragment G
   : ('g' | 'G')
   ;


fragment H
   : ('h' | 'H')
   ;


fragment I
   : ('i' | 'I')
   ;


fragment J
   : ('j' | 'J')
   ;


fragment K
   : ('k' | 'K')
   ;


fragment L
   : ('l' | 'L')
   ;


fragment M
   : ('m' | 'M')
   ;


fragment N
   : ('n' | 'N')
   ;


fragment O
   : ('o' | 'O')
   ;


fragment P
   : ('p' | 'P')
   ;


fragment Q
   : ('q' | 'Q')
   ;


fragment R
   : ('r' | 'R')
   ;


fragment S
   : ('s' | 'S')
   ;


fragment T
   : ('t' | 'T')
   ;


fragment U
   : ('u' | 'U')
   ;


fragment V
   : ('v' | 'V')
   ;


fragment W
   : ('w' | 'W')
   ;


fragment X
   : ('x' | 'X')
   ;


fragment Y
   : ('y' | 'Y')
   ;


fragment Z
   : ('z' | 'Z')
   ;

COMMENT	:	'/*'.*? '*/' -> skip;

STRING
   : [a-zA-Z] [a-zA-Z]*
   ;


NUMBER
   : [0-9] +
   ;


WS
   : [ \t\r\n] -> skip
   ;
