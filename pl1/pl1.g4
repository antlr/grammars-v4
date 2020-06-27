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
   : statements
   ;




statements	:	( '(' { condition-prefix }','+ ')' ':' )* ( label_prefix ':' )*
 statement_body ? ';';

percent_statements	:	'%' ( label_prefix ':' )* statement_body ';';

attribute	:	( 'BINARY' | 'DECIMAL' );

attribute	:	'TASK';

attribute	:	'CONDITION';

attribute	:	'PRINT';

attribute	:	'KEYED';

attribute	:	'EXCLUSIVE';

attribute	:	'ENVIRONMENT' '(' characteristic_list ')';

attribute	:	'BACKWARDS';

attribute	:	( 'BUFFERED' | 'UNBUFFERED' );

attribute	:	( 'DIRECT' | 'SEQUENTIAL' | 'TRANSIENT' );

attribute	:	( 'INPUT' | 'OUTPUT' | 'UPDATE' );

attribute	:	( 'RECORD' | 'STREAM' );

attribute	:	'FILE';

attribute	:	generic_name 'GENERIC'
'(' { entry_reference 'WHEN' '(' { generic_descriptor }','* ')' }','+ ')';

attribute	:	'BUILTIN';

attribute	:	 ( 'IRREDUCIBLE' | 'REDUCIBLE' ) ?;

attribute	:	'RETURNS' '(' ( attribute )+ ')';

attribute	:	'OPTIONS' '(' characteristic_list ')';

attribute	:	'ENTRY' ( '('  parameter_descriptor_list ? ')' )?;

attribute	:	'INITIAL' '(' { item }  ','+ ')';

attribute	:	'CONNECTED';

attribute	:	'POSITION' '(' expression ')';

attribute	:	'DEFINED' ( reference | '(' reference ')' );

attribute	:	'OFFSET' ( '(' area_variable ')' )?;

attribute	:	'AREA' ( ( '(' '*' ')' | '(' expression ( 'REFER' '(' variable ')' )? ')' ) )?;

attribute	:	'POINTER';

attribute	:	'BASED' ( '(' locator_reference ')' )?;

attribute	:	'CONTROLLED';

attribute	:	'AUTOMATIC';

attribute	:	'STATIC';

attribute	:	( 'INTERNAL' | 'EXTERNAL' );

attribute	:	'LIKE' structure_variable;

attribute	:	'(' { bound }','+ ')';

attribute	:	( 'ALIGNED' | 'UNALIGNED' );

attribute	:	'VARIABLE';

attribute	:	'LABEL' ( '(' { label_constant }','+ ')' )?;

attribute	:	'EVENT';

attribute	:	( 'FIXED' | 'FLOAT' );

attribute	:	'(' number_of_digits ( ',' scaling_factor )? ')';

attribute	:	( 'REAL' | 'COMPLEX' );

attribute	:	'BIT' ( '(' length ( 'REFER' '(' variable ')' )? ')' )?;

attribute	:	'CHARACTER' ( '(' length ( 'REFER' '(' variable ')' )? ')' )?;

attribute	:	'GRAPHIC' ( '(' length ('REFER' '(' variable ')' )? ')' )?;

attribute	:	'VARYING';

attribute	:	'PICTURE' '/'' picture_specification '/'';

constant	:	'/'' ( ( character | '/"' ) )* '/'';

constant	:	'/'' ( hex_digit hex_digit )* '/'X';

constant	:	'/'' ( ( '0' | '1' ) )* '/'B';

constant	:	'/'' ( hex_digit )* ( '/'BX' | '/'B4' )';

constant	:	'/'' ( graphic_symbol )* '/'G';

constant	:	'/'' ( hex_digit hex_digit hex_digit hex_digit )* '/'GX';

constant	:	'/'' { ( character | '/'' '/'' | '<' { graphic_symbol }+ '>' ) }* '/'M';

attribute_bound	:	( ( lower_bound ':' )? upper_bound | '*' );

attribute_upper_bound	:	expression ( 'REFER' '(' variable ')' )?;

attribute_lower_bound	:	expression ( 'REFER' '(' variable ')' )?;

expression	:	unary_expression ( infix_operator unary_expression )*;

expression_unary_expression	:	( prefix_operator )* elementary_expression;

expression_elementary_expression	:	( '(' expression ')' | reference | constant | isub6 );

expression_reference	:	 locator_qualifier ? basic_reference
( '(' subscript_list ')' )? ( '('  argument_list ? ')' )?;

expression_locator_qualifier	:	reference '->';

expression_subscript_list	:	{ ( expression | '*' ) }','+;

expression_argument_list	:	{ expression }','+;

expression_basic_reference	:	 structure_qualification ? name;

expression_structure_qualification	:	basic_reference ('(' subscript_list ')' )?;

statement	:	'FETCH' { entry_constant }','+ ';';

statement	:	'%' ';';

statement	:	'%' ( label ':' )* 'NOTE' '(' message ( ',' code )? ')' ';';

statement	:	'%' ( label ':' )* 'IF' preprocessor_expression
'%' 'THEN' preprocessor_unit1 ( '%' 'ELSE' preprocessor_unit2 )?;

statement	:	'%' ( label ':' )* 'GO_TO' label ';';

statement	:	'%' ( label ':' )* 'END' label ? ';';

statement	:	'%' ( label ':' )* 'DO' preprocessor_do_specification ';';
statement	:	'%' ( label ':' )* 'DO' ';';

statement	:	'%' ( label ':' )* 'DECLARE' { identifier ( 'FIXED' | 'CHARACTER' | 'ENTRY' | 'BUILTIN' ) }','+ ';';

statement	:	'%' ( label ':' )* 'DEACTIVATE' { identifier }','+ ';';

statement	:	'%' ( label ':' )*
preprocessor_variable '=' preprocessor_expression ';';

statement	:	'%' ( label ':' )* 'ACTIVATE' { identifier ( ( 'RESCAN' | 'NORESCAN' ) )? }','+ ';';

statement	:	( label ':' )* 'RETURN' '(' preprocessor_expression ')' ';';

statement	:	'%' ( entry_name ':' )+ 'PROCEDURE' ( '(' { parameter }','+ ')' )?
 'STATEMENT'?  'RETURNS' '(' ( 'CHARACTER' | 'FIXED' ) ')' ';';

statement	:	'EXIT' ';';

statement	:	'%' 'SKIP' ( '(' n ')' )? ';';

statement	:	'%' 'PRINT' ';';

statement	:	'%' 'PAGE' ';';

statement	:	'%' 'NOPRINT' ';';

statement	:	'%' 'INCLUDE' { ( dataset '(' member_name ')' | member_name ) }','+ ';'
;
statement	:	'SIGNAL' condition ';';

statement	:	'REVERT' condition ';';

statement	:	'ON' condition  'SNAP' ? ( 'SYSTEM' ';' | on_unit );

statement	:	( label ':' )+ 'FORMAT' '(' format_list ')' ';';

statement	:	'PUT-STRING' '(' character_reference ')' data_specification ';';

statement	:	'PUT' ( 'FILE' '(' file_reference ')' )?  data_specification ?
( ( 'PAGE' ( 'LINE' '(' expression ')' )? | 'SKIP' ( '(' expression ')' )? | 'LINE' '(' expression ')' ) )? ';';

statement	:	'GET-STRING' '(' expression ')' data_specification ';';

statement	:	'GET' ( 'FILE' '(' file_reference ')' )?  data_specification ?
( 'COPY' '(' file_reference ')' )?
( 'SKIP' ( '(' expression ')' )? )? ';';

statement	:	'UNLOCK' 'FILE' '(' file_reference ')' 'KEY' '(' expression ')' ';';

statement	:	'DELETE' 'FILE' '(' file_reference ')' ( 'KEY' '(' expression ')' )?
( 'EVENT' '(' event_reference ')' )? ';';

statement	:	'LOCATE' based_variable
( 'FILE' '(' file_reference ')' | 'SET' '(' pointer_reference | 'KEYFROM' '(' expression ')' ) ';';

statement	:	'REWRITE_FILE' '(' file_reference ')' ( 'FROM' '(' reference ')' )?
( 'KEY' '(' expression ')' )? ( 'EVENT' '(' event_reference )? ';';

statement	:	'WRITE' 'FILE' '(' file_reference ')' 'FROM' '(' reference ')'
( ( 'KEYFROM' '(' expression ')' | 'KEYTO' '(' reference ')' ) )? ( 'EVENT' '(' event_reference )? ';';

statement	:	'READ' 'FILE' '(' file_reference ')'
( 'INTO' '(' ref ')' ( ( 'KEY' '(' expression ')'  'NOLOCK' ? | 'KEYTO' '(' reference ')' ) )? | 'SET' '(' pointer_ref ')' ( ( 'KEY' '(' expression ')' | 'KEYTO' '(' reference ')' ) )? | 'IGNORE' '(' expression ')' )
( 'EVENT' '(' reference ')' )? ';';

statement	:	'CLOSE'
{ 'FILE' '(' file_reference ')' ( 'ENVIRONMENT' '(' ( 'LEAVE' | 'REREAD' ) ')' )? }','+ ';';

statement	:	'OPEN' { options_group }','+ ';';

statement	:	'RETURN' ( '(' expression ')' )? ';';

statement	:	'CALL' ( entry_reference | generic_name | built_in_name ) ( '(' { argument }','* ')' )?
( 'TASK' ( '(' task_reference ')' )? )? ( 'EVENT' '(' event_reference ')' )?
( 'PRIORITY' '(' expression ')' )? ';';

statement	:	( 'COBOL' | 'FORTRAN' ) ( 'NOMAP' ( '(' parameter_list ')' )? )?
( 'NOMAPIN' ( '(' parameter_list ')' )? )?
( 'NOMAPOUT' ( '(' parameter_list ')' )? )?;

statement	:	( 'MAIN'  'NOEXECOPS'?   'REENTRANT' ?  'TASK' ? | 'COBOL'  additional_options  | 'FORTRAN'  additional_options  );

statement	:	{ entry_constant ':' }+ 'ENTRY' ( '(' { parameter }','+ ')' )?
( 'RETURNS' '(' { attribute }+ ')' )?
( 'OPTIONS' '(' characteristic_list ')' )? ( ( 'REDUCIBLE' | 'IRREDUCIBLE' ) )? ';';

statement	:	{ entry_constant ':' }+ 'PROCEDURE' ( '(' { parameter }','+ ')' )?
( 'RETURNS' '(' { attribute }+ ')' )?
( 'OPTIONS' '(' characteristic_list ')' )? ( ( 'REDUCIBLE' | 'IRREDUCIBLE' ) )?
 'RECURSIVE'? ( ( 'ORDER' | 'REORDER' ) )? ( ( 'CHARGRAPHIC' | 'NOCHARGRAPHIC' ) )? ';';

statement	:	'WAIT' '(' { event_reference }','+ ')' ( '(' expression ')' )? ';';

statement	:	'STOP' ';';

statement	:	'SELECT' ( '(' exp1 ')' )? ';'
{ 'WHEN' '(' { exp2 }','+ ')' unit }* ( 'OTHERWISE' unit )?;

statement	:	';';

statement	:	'LEAVE' label_constant?  ';';

statement	:	'IF' expression 'THEN' unit1 ( 'ELSE' unit2 )?;

statement	:	( 'GO' 'TO' | 'GOTO' ) label_reference ';';

statement	:	'END'  statement_label?  ';';

statement	:	'DO' reference '=' { specification }','+ ';';

statement	:	'DO' ( 'WHILE' '(' exp4 ')' ( 'UNTIL' '(' exp5 ')' )? | 'UNTIL' '(' exp5 ')' ( 'WHILE' '(' exp4 ')' )? ) ';';

statement	:	'DO' ';';

statement	:	'DISPLAY' '(' expression ')'
( 'REPLY' '(' character_reference ')' ( 'EVENT' '(' event_reference ')' )? | 'EVENT' '(' event_reference ')' reply '(' character_reference ')' ) ';';

statement	:	'DISPLAY' '(' expression ')' ';';

statement	:	'DELAY' '(' expression ')' ';';

statement	:	{ reference }','+ '=' expression ( ',' 'BY' 'NAME' )? ';';

statement	:	'FREE' { option }','+ ';';

statement	:	'ALLOCATE' { based_variable  reference?  }','+ ';';

statement	:	'FREE' { controlled_variable }','+ ';';

statement	:	'ALLOCATE' {  level?  controlled_variable  attribute?  }','+;

statement	:	'RELEASE' { entry_constant }+ ';';

statement	:	'BEGIN' ( ( 'ORDER' | 'REORDER' ) )? ( ( 'CHARGRAPHIC' | 'NOCHARGRAPHIC' ) ? ';';

statement	:	'DECLARE' {  level?  name { attribute }*  'SYSTEM'?s  }','+ ';';

statement	:	'DEFAULT' { ( simple_specification | factored_specification ) }','+ ';';

statement_simple_specification	:	( 'RANGE' '(' { ( { letter }+ | letter ':' letter ) }+ ')'  attribute_specification? | 'RANGE' '(' '*' ')'  attribute_specification?  | 'DESCRIPTORS'  attribute_specifications ? );

statement_factored_specification	:	'(' { ( simple_specification | factored_specification ) }','+ ')'  attribute_specification? ;

statement_attribute_specification	:	( { attribute }+ ( 'VALUE' '(' { value_specification }','+ ')' )? | 'VALUE' '(' { value_specification }','+ ')' );

statement_value_specification	:	( 'AREA' '(' size ')' | 'BIT' '(' length ')' | 'CHARACTER' '(' length ')' | 'GRAPHIC' '(' length ')' | additional_options );

statement_additional_options	:	( ( 'REAL' | 'COMPLEX' ) )? ( ( 'FIXED' | 'FLOAT' ) )? ( ( 'BINARY' | 'DECIMAL' ) )?
( '(' precision ( ',' scaling_factor )? ')' )?;

statement_additional_options	:	( 'NOMAP' ( '(' parameter_list ')' )? )?
( 'NOMAPIN' ( '(' parameter_list ')' )? )?
( 'NOMAPOUT' ( '(' parameter_list ')' )? )?;

statement_attribute	:	( dimension | 'CHARACTER' '(' length | 'BIT' '(' length ')' | 'GRAPHIC' '(' length ')' | 'AREA' '(' size ')' | 'INITIAL' '(' { 'ITEM' }','+ ')' | initial_call_entry_reference ( '(' { argument }','+ ')' )? );

locator_qualification	:	locator_reference '->' { based_locator_reference '->' }*
based_reference;

statement_reference	:	( 'IN' '(' area_reference ')' )? ( 'SET' '(' locator_reference ')' )?;

statement_option	:	( locator_qualifier '>' )? based_variable
( 'IN' '(' area_reference ')' )?;

option	:	expression 'REFER' '(' variable ')';

option	:	'PRIORITY' '(' expression ')';

option	:	'EVENT' '(' event_reference ')';

option	:	'TASK' ( '(' task_reference ')' )?;

attribute_item	:	( '*' | initial_constant | reference | '(' expression ')' | iteration_specification );

attribute_iteration_specification	:	'(' iteration_factor ')' iteration_item;

attribute_iteration_item	:	( '*' | initial_constant | reference | '(' { item }','+ ')' );

attribute_initial_constant	:	( ( ( '+' | '-' ) )? arithmetic_constant | bit_constant | character_constant | graphic_constant | entry_constant | file_constant | label_constant | [ ( '+' | '-' ) ] real_constant ( '+' | '-' ) imaginary_constant );

attribute_initial_constant	:	'INITIAL' 'CALL' entry_reference ( '(' { argument }','+ ')' )?;

statement_specification	:	exp1 ( ( 'TO' exp2 ( 'BY' exp3 )? | 'BY' exp3 ( 'TO' exp2 )? | 'REPEAT' exp6 ) )?
( ( 'WHILE' '(' exp4 ')' ( '(' 'UNTIL' '(' exp5 ')' )? | 'UNTIL' '(' exp5 ')' ( '(' 'WHILE' '(' exp4 ')' )? ) )?;

entry_data	:	'ENTRY' ( '(' parameter_descriptor_list ')' )?
( 'RETURNS' '(' attribute_list ')' )?
( 'OPTIONS' '(' characteristic_list ')' )?  'EXTERNAL' ?
( ( 'IRREDUCIBLE' | 'REDUCIBLE' ) )?;

entry_data	:	'ENTRY' ( '(' parameter_descriptor_list ')' )?  'VARIABLE' ?
( 'RETURNS' '(' attribute_list ')' )?
( 'OPTIONS' '(' characteristic_list ')' )? ( ( 'IRREDUCIBLE' | 'REDUCIBLE' ) )?
 scope_attribute ?  storage_attribute ?
 alignment_attribute ? ? parameter ?
? 'DEFINED'  'INITIAL'? ;

attribute_parameter_descriptor_list	:	{ parameter_descriptor }','+;

attribute_parameter_descriptor	:	( ( { attribute }+ | '*' | structure_parameter_descriptor ) )?;

attribute_structure_parameter_descriptor	:	'1' { attribute }* ',' { level { attribute }* }','+;

attribute_characteristic_list	:	( 'COBOL' | 'FORTRAN' | 'ASSEMBLER' )  additional_options ?;

attribute_additional_options	:	( 'NOMAP' ( '(' { argi }','+ ')' )? )? ( 'NOMAPIN' ( '(' { argi }+ ')' )? )?
( 'NOMAPOUT' ( '(' { argi }','+ ')' )? )?  'INTER' ?  'RETCODE' ?;

built_in_function	:	name ( '(' { argument }','* ')' )?;

built_in_function	:	'PARMSET' '(' x ')';

built_in_function	:	'COUNTER';

built_in_function	:	'COMPILETIME';

built_in_function	:	'VERIFY' '(' x ',' y ')';

built_in_function	:	'UNSPEC' '(' x ')';

built_in_function	:	'TRUNC' '(' x ')';

built_in_function	:	'TRANSLATE' '(' x ',' y ( ',' z )? ')';

built_in_function	:	'TIME' ( '(' ')' )?;

built_in_function	:	'TANH' '(' x ')';

built_in_function	:	'TAND' '(' x ')';

built_in_function	:	'TAN' '(' x ')';

built_in_function	:	'SYSNULL' ( '(' ')' )?;

built_in_function	:	'SUM' '(' x ')';

built_in_function	:	'SUBSTR' '(' x ',' y ( ',' z )? ')';

built_in_function	:	'STRING' '(' x ')';

built_in_function	:	'STORAGE' '(' x ')';

built_in_function	:	'STATUS' ( '(' x ')' )?;

built_in_function	:	'SQRT' '(' x ')';

built_in_function	:	'SINH' '(' x ')';

built_in_function	:	'SIND' '(' x ')';

built_in_function	:	'SIN' '(' x ')';

built_in_function	:	'SIGN' '(' x ')';

built_in_function	:	'SAMEKEY' '(' x ')';

built_in_function	:	'ROUND' '(' x ',' y ')';

built_in_function	:	'REPEAT' '(' x ',' y ')';

built_in_function	:	'REAL' '(' x ')';

built_in_function	:	'PROD' '(' x ')';

built_in_function	:	'PRIORITY' '(' x ')';

built_in_function	:	'PRECISION' '(' x ',' p ( ',' q )? ')';

built_in_function	:	'POLY' '(' x ',' y ')';

built_in_function	:	'POINTERVALUE' '(' x ')';

built_in_function	:	'POINTERADD' '(' x ',' y ')';

built_in_function	:	'POINTER' '(' x ',' y ')';

built_in_function	:	'PLIRETV' ( '(' ')' )?;

built_in_function	:	'ONSOURCE' ( '(' ')' )?;

built_in_function	:	'ONLOC' ( '(' ')' )?;

built_in_function	:	'ONKEY' ( '(' ')' )?;

built_in_function	:	'ONFILE' ( '(' ')' )?;

built_in_function	:	'ONCOUNT' ( '(' ')' );

built_in_function	:	'ONCODE' ( '(' ')' );

built_in_function	:	'ONCHAR' ( '(' ')' );

built_in_function	:	'OFFSET' '(' x ',' y ')';

built_in_function	:	'NULL' ( '(' ')' );

built_in_function	:	'MULTIPLY' '(' x ',' y ',' p ( ',' q )? ')';

built_in_function	:	'MPSTR' '(' x ',' r ( ',' y )? ')';

built_in_function	:	'MOD' '(' x ',' y ')';

built_in_function	:	'MIN' '(' { x }','+ ')';

built_in_function	:	'MAX' '(' { x }','+ ')';

built_in_function	:	'LOW' '(' x ')';

built_in_function	:	'LOG10' '(' x ')';

built_in_function	:	'LOG2' '(' x ')';

built_in_function	:	'LOG' '(' x ')';

built_in_function	:	'LINENO' '(' x ')';

built_in_function	:	'LENGTH' '(' x ')';

built_in_function	:	'LBOUND' '(' x ',' y ')';

built_in_function	:	'INDEX' '(' x ',' y ')';

built_in_function	:	'IMAG' '(' x ')';

built_in_function	:	'HIGH' '(' x ')';

built_in_function	:	'HBOUND' '(' x ',' y ')';

built_in_function	:	'GRAPHIC' '(' x ( ',' y )? ')';

built_in_function	:	'FLOOR' '(' x ')';

built_in_function	:	'FLOAT' '(' x ( ',' p )? ')';

built_in_function	:	'FIXED' '(' x ( ',' p ( ',' q )? )? ')';

built_in_function	:	'EXP' '(' x ')';

built_in_function	:	'ERFC' '(' x ')';

built_in_function	:	'ERF' '(' x ')';

built_in_function	:	'ENTRYADDR' '(' x ')';

built_in_function	:	'EMPTY' ( '(' ')' )?;

built_in_function	:	'DIVIDE' '(' x ',' y ',' p ( ',' q )? ')';

built_in_function	:	'DIM' '(' x ',' y ')';

built_in_function	:	'DECIMAL' '(' x ( ',' p ( ',' q )? )? ')';

built_in_function	:	'DATETIME' ( '(' ')' )?;

built_in_function	:	'DATE' ( '(' ')' )?;

built_in_function	:	'DATAFIELD' ( '(' ')' )?;

built_in_function	:	'CURRENTSTORAGE' '(' x ')';

built_in_function	:	'COUNT' '(' x ')';

built_in_function	:	'COSH' '(' x ')';

built_in_function	:	'COSD' '(' x ')';

built_in_function	:	'COS' '(' x ')';

built_in_function	:	'CONJG' '(' x ')';

built_in_function	:	'COMPLEX' '(' x ',' y ')';

built_in_function	:	'COMPLETION' '(' x ')';

built_in_function	:	'CHAR' '(' x ( ',' y )? ')';

built_in_function	:	'CEIL' '(' x ')';

built_in_function	:	'BOOL' '(' x ',' y ',' z ')';

built_in_function	:	'BIT' '(' x ( ',' y )? ')';

built_in_function	:	'BINARYVALUE' '(' x ')';

built_in_function	:	'BINARY' '(' x () ',' p ( ',' q )? )? ')';

built_in_function	:	'ATANH' '(' x ')';

built_in_function	:	'ATAND' '(' x ( ',' y )? ')';

built_in_function	:	'ATAN' '(' x ( ',' y )? ')';

built_in_function	:	'ASIN' '(' x ')';

built_in_function	:	'ANY' '(' x ')';

built_in_function	:	'ALLOCATION' '(' x ')';

built_in_function	:	'ALL' '(' x ')';

built_in_function	:	'ADDR' '(' x ')';

built_in_function	:	'ADD' '(' x ',' y ',' p ( ',' q )? ')';

built_in_function	:	'ACOS' '(' x ')';

built_in_function	:	'ABS' '(' x ')';

built_in_subroutine	:	call_name ( '(' { argument }','* ')' )? ';';

built_in_subroutine	:	'PLITEST' ( '(' command_list ')' )?;

built_in_subroutine	:	'PLITDLI' '(' { argument }','+ ')';

built_in_subroutine	:	'PLISRTD' '(' { argument }','+ ')';

built_in_subroutine	:	'PLISRTC' '(' { argument }','+ ')';

built_in_subroutine	:	'PLISRTB' '(' { argument }','+ ')';

built_in_subroutine	:	'PLISRTA' '(' { argument }','+ ')';

built_in_subroutine	:	'PLIRETC' '(' return_code ')';

built_in_subroutine	:	'PLIREST';

built_in_subroutine	:	'PLIDUMP' '(' { argument }','+ ')';

built_in_subroutine	:	'PLICKPT' '(' { argument }','+ ')';

built_in_subroutine	:	'PLICANC';

built_in_subroutine	:	'ASMTDLI' '(' { argument }','+ ')';

pseudovariable	:	'COMPLETION' '(' x ')';

pseudovariable	:	'COMPLEX' '(' x ',' y ')';

pseudovariable	:	'ENTRYADDR' '(' x ')';

pseudovariable	:	'IMAG' '(' x ')';

pseudovariable	:	'ONCHAR' ( '(' ')' )?;

pseudovariable	:	'ONSOURCE' ( '(' ')' )?;

pseudovariable	:	'PRIORITY' ( '(' x ')' )?;

pseudovariable	:	'REAL' '(' x ')';

pseudovariable	:	'STATUS' ( '(' x ')' )?;

pseudovariable	:	'STRING' '(' x ')';

pseudovariable	:	'SUBSTR' '(' x ',' y ( ',' z )? ')';

pseudovariable	:	'UNSPEC' '(' x ')';

statement_options_group	:	'FILE' '(' file_reference ')'  ( 'STREAM' | 'RECORD' ) ?
( ( 'INPUT' | 'OUTPUT' | 'UPDATE' ) )? ( ( 'DIRECT' | 'SEQUENTIAL' | 'TRANSIENT' ) )?  ( 'BUFFERED' | 'UNBUFFERED' ) ?
 'BACKWARDS' ? 'EXCLUSIVE' ?  'KEYED' ?
 'PRINT'? ( 'TITLE' '(' expression ')' )?
( 'LINESIZE' '(' expression ')' )?
( 'PAGESIZE' '(' expression ')' )?
;

data_specification	:	(  'LIST' ? '(' data_specification_data_list ')' | 'DATA' ( '(' { data_specification_data_list }','+ ')' )? | 'EDIT' { '(' data_specification_data_list ')' '(' data_specification_format_list ')' }+ )
;

data_specification_data_list	:	( { data_list_item }','+ | { '(' data_list_item_type_3_do ')' }+ )
;

data_specification_format_list	:	{ ( format_item | n_format_item | n '(' format_list ')' ) }','+
;

list_directed_data_value	:	( ( ( '+' | '-' ) )? arithmetic_constant | ( ( '+' | '-' ) )? real_constant ( '+' | '-' ) imaginary_constant | character_constant | bit_constant | graphic_constant )
;

data_directed_data_specification	:	'DATA' ( '(' data_list ')' )?
;

data_directed_element_assignment	:	{ { element_variable '=' data_value }+ }','+ ';'
;

edit_directed_data_specification	:	'EDIT' { '(' data_list ')' '(' format_list ')' }+
;

edit_directed_data_specification_format_list	:	( { format_item }','+ | { n_format_item }','+ | { n '(' format_list ')' }','+ )
;

format_item	:	'A' ( '(' field_width ')' )?
;
format_item	:	'B' ( '(' field_width ')' )?
;
format_item	:	'C' '(' { real_format_item }','+ ')'
;
format_item	:	'COLUMN' '(' character_position ')'
;
format_item	:	'E' '(' field_width ',' fractional_digits
( ',' significant_digits )? ')'
;
format_item	:	'F' '(' field_width
( ',' fractional_digits (',' scaling_factor )? )? ')'
;
format_item	:	'G' ( '(' field_width ')' )?
;
format_item	:	'LINE' '(' line_number ')'
;
format_item	:	'P' '/'' picture_specification '/''
;
format_item	:	'PAGE'
;
format_item	:	'R' '(' label_reference ')'
;
format_item	:	'SKIP' ( '(' relative_line ')' )?
;
format_item	:	'X' '(' field_width ')'
;

scaling_factor_character	:	'F' '(' ( ( '+' | '-' ) )? integer ')'
;

condition	:	'AREA'
;
condition	:	'ATTENTION'
;
condition	:	'CHECK' ( '(' { name }','+ ')' )?
;
condition	:	'CONDITION' '(' name ')'
;
condition	:	'CONVERSION'
;
condition	:	'ENDFILE' '(' file_reference ')'
;
condition	:	'ENDPAGE' '(' file_reference ')'
;
condition	:	'ERROR'
;
condition	:	'FINISH'
;
condition	:	'FIXEDOVERFLOW'
;
condition	:	'KEY' '(' file_reference ')'
;
condition	:	'NAME' '(' file_reference ')'
;
condition	:	'OVERFLOW'
;
condition	:	'PENDING' '(' file_reference ')'
;
condition	:	'RECORD' '(' file_reference ')'
;
condition	:	'SIZE'
;
condition	:	'STRINGRANGE'
;
condition	:	'STRINGSIZE'
;
condition	:	'SUBSCRIPTRANGE'
;
condition	:	'TRANSMIT' '(' file_reference ')'
;
condition	:	'UNDEFINEDFILE' '(' file_reference ')'
;
condition	:	'UNDERFLOW'
;
condition	:	'ZERODIVIDE'
;

statement_preprocessor_do_specification	:	preprocessor_variable '=' preprocessor_exp1
( ( 'TO' preprocessor_exp2 ( 'BY' preprocessor_exp3 )? | 'BY' preprocessor_exp3 ( 'TO' preprocessor_exp2 )? ) )?
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
