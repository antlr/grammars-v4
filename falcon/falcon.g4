/*
BSD License

Copyright (c) 2023, Tom Everett
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

grammar falcon;

program_ : [interpreter marker, EOL ], { module level statement | statement} EOF;

interpreter_marker : ? unix/sh She-bang interpreter directive ?	 ;

module_level_statement :
	load_statement |
	import_statement |
	export_statement |
	compiler_directive |
	declaration_statement;

statement : autoexpression |
		  if_statement |
		  while_statement |
		  forto_statement |
		  forin_statement |
		  loop_statement |
		  switch_statement |
		  select_statement |
		  try_statement |
		  const_statement |
		  def_statement |
		  empty;

empty : terminator; 


terminator : ';' | EOL;

EOL : ? any valid end-of-line sequence in any served system ?
 				  
white space : ? ASCII space, tab, fixed-width unicode space & similar ?


load statement : 'load', ( module name | module path );

import statement : 
        'import', [name list], terminator |
	'import', [name list], 'from', ( module name | module path ), ['in', 'self' | namespace ], terminator |
	'import', [name], 'from', ( module name | module path ) , [ 'as', name ], terminator;

module name : ['self.' | '.' ] , name { '.' , name };
module path : ? a string representing a complete URI, in single or double quotes ?;
 
export statement : 'export', name list, terminator;
	
compiler directive : 'directive' { 
		('lang', '=' , language code ) |
		('strict' , '=' , 'on' | 'off' ) |
		('version', '=' , integer number ) };

name list : name { ',' , name };

name : ? sequence of any printable UNICODE character NOT STARTING with ciphers '0'..'9' ?

language code : ? ISO 5 chars language code in format xx_XX ?


autoexpression : (assignment expression | call expression | incdec expression), terminator;

expression : 
   immediate |
   lvalue |
   compare expression |
   incdec expression |
   call expression |
   math expression |
   out of band expression |
   logic expression |
   string expression |
   choice expression |
   reference expression |
   array declaration |
   dictionary declaration |
   anonymous function declaration |
   fast eval |
   '(', expression, ')';

immediate : number | string | value token;

(* Rules for number parsing are complex, but completely resolved at lexer level *)
number : ? any number in decimal, binary, octal or hexadecimal notation, with "_" separator, or in scientific 'e' notation ?

(* Rules for string parsing are complex, but completely resolved at lexer level *)
string : ? Sequence of possibly escaped characters enclosed in '"' or single quote ?

value token : 'nil' | 'true' | 'false' | 'self' | 'fself';

lvalue : 
	name |
	assignment expression |
	access expression;

assignment expression : 
	simple assignment |
	auto assignment;
	
simple assignment : ( lvalue | lvalue list ), ':', expression , { ',' , expression };  

access expression :
	lvalue, '.', name |				(* dot accessor *)
	lvalue, '[', expression ,']';    (* array accessor *)
	
lvalue list : lvalue , ',' , lvalue, { [ ',' ,  lvalue ] };

auto assignment :
	lvalue, '+=', expression |    (* auto add *)
	lvalue, '-=', expression |    (* auto subtract *)
	lvalue, '*=', expression |    (* auto multiply *)
	lvalue, '/=', expression |    (* auto divide *)
	lvalue, '**=', expression |   (* auto power *)
	lvalue, '%=', expression |    (* auto modulo *)
	lvalue, '>>=', expression |   (* auto shift left *)
	lvalue, '<<=', expression |   (* auto shift right *)
	lvalue, '^&=', expression |   (* auto binary and *)
	lvalue, '^|=', expression |   (* auto or *)
	lvalue, '^^=', expression |   (* auto xor *)
	lvalue, '^=', expression;    (* auto not *)

compare expression :
	expression, '==', expression |
	expression, '!=', expression |
	expression, '>', expression |
	expression, '<', expression |
	expression, '>=', expression |
	expression, '<=', expression;
	
incdec expression :
	'++', expression |		(* increment value and return it *)
	expression, '++' | 		(* return the value, then increment it *)
	'--', expression |		(* decrement the value, then return it *)
	expression --;			(* return the value, then decrement it *)
	  				  
call expression : expression, '(', expression list, ')';

(* it's a possibly empty list of comma-separated expressions *)
expression list : [ expression, { ',', expression } ];
 
math expression :
	expression, '+', expression |
	expression, '-', expression |
	'-', expression |			(* unary minus; - -12 is valid and resolves in 12 *)
	expression, '*', expression |
	expression, '/', expression |
	expression, '**', expression |
	expression, '%', expression |
	expression, '>>', expression |
	expression, '<<', expression |
	expression, '^&', expression |		(* bitfield and *)
	expression, '^|', expression |		(* bitfield or *)
	expression, '^^', expression |		(* bitfield xor *)
	'^~', expression |			(* bitfield not *)

out of band expression :
	'^?' , expression |		(* Is out of band? *)
	'^+' , expression |		(* Set out of band *)
	'^-' , expression |		(* Reset out of band *)
	'^%' , expression;              (* Switch out of band *)

string expression :
	'@' , expression |			(* String expansion ($name) *)
	'#' , expression;			(* Indirect operator *)
	
logic expression :
	expression, 'and', expression |
	expression, 'or', expression |
	expression, 'in', expression |
	expression, 'notin', expression |
        expression, 'provides', expression |
	'not', expression ;							

choice expression : expression, '?', expression , ':' , expression;

reference expression : 
	'$', '0' |			(* Break reference *) 
	'$', lvalue |			(* References an lvalue *)
        '&', name ;			(* Symbol item, like LISP 'name *)

fast eval : '^*', expression;        (* Equivalent to eval(expression) *)	

array declaration : 
	'[', expression list, ']' |
	'.[', expression sequence , ']';

(* List of whitespace separated expressions *)
expression sequence : [ expression, { expression } ];

(* '^[' and '^.[' declare blessed dictionaries *)

dictionary declaration : 
    ( '[' | '^[' | '.[' | '^.[' ) , '=>' , ']' | 					(* Empty dictionary *)
	 ( '[' | '^[' )  expression pair list ']' |
	 ( '.[' | '^.[' )  expression pair sequence ']';
	 
(* List of comma separated pairs of expressions *) 
expression pair list : expression pair , { ',', expression pair };

(* List of white space separated pairs of expressions *) 
expression pair sequence : expression pair , { expression pair };

expression pair : expression , '=>' , expression;

anonymous function declaration :
	'function', '(', [ name list ], ')', terminator, { statement }, 'end' |
	'{', [ name list ], '=>', expression , '}' 				(* Automatically returns the value of the expression *)
	'{', [ name list ], '=>', { statement } , '}';
	


declaration statement :
	function declaration |
	class declaration |
	enum declaration |
	object declaration;
	
function declaration : 
	'function' , name, '(', [ name list ] , ')', terminator
		{statement}
	'end', terminator | 
	
	'function' , name, '(', [ name list ] , ')', ':', statement;

enum declaration :
	'enum', name, terminator
		enum entry list , terminator
	'end', terminator;
	
enum entry list :  [ name , [ '=', immediate ] , { ',' , [terminator] , name , [ '=', immediate ] } 

class declaration :
	'class' , name , ['(', name list , ')'] ,  [from clause] , terminator,
		{class element declaration}
	'end' , terminator
	
from clause : 
	'from' , 
		namespaced name , ['(' , expression list , ')'] , 
		{ ',' , namespaced name, ['(' , expression list , ')'] };
		
namespaced name :
	name , {'.' , name };
	
class element declaration : 
	init declaration |
	property declaration |
	function declaration |
        state declaration;
	
init declaration :
	'init', terminator ,
		{statement},
	'end', terminator |
	'init', ':' , statement;

property declaration : name , '=' , expression, terminator;

state declaration : '[' , name , ']', terminator,
      {function declaration},
      'end', terminator; 


if statement :
	'if' , expression , ':' , statement  |
	'if' , expression , terminator,
		{statement},
	['elif' , expression, terminator,
		{statement} ,
	],
	[ 'else', terminator, 
		{statement}, 
	],
	'end', terminator;
	

while statement :
	'while' , expression , ( ':' , statement |
                terminator, {statement in loop} , 'end', terminator );

forto statement :
	forto heading, ( ':', statement |
               terminator, {statement in for } , 'end', terminator );
	
forto heading : 'for' , name , '=' , number , 'to' , number , ['step', number ];

forin statement :
	'for' , name list , 'in' , expression, 
           (':', statement |
	    terminator , {statement in for } , 'end', terminator);

loop statement :
	'loop', terminator,
		{statement in loop} ,
	'end', [expression], terminator;
		

statement in loop : 
    statement |
    'continue' |
    'break';

statement in for :
    forfirst block | formiddle block | forlast block |
    {statement in loop};

forfirst block : 'forfirst' ,
         ( ':', statement in loop | 
            terminator , { statement in loop }, end , terminator )

formiddle block : 'formiddle' ,
         ( ':', statement in loop | 
            terminator , { statement in loop }, end , terminator )

forlast block : 'forlast' ,
         ( ':', statement in loop | 
            terminator , { statement in loop }, end , terminator )

switch statement :
	'switch', expression, terminator,
		{case statements},
		[default statement],
	'end', terminator;
	
select statement :
	'select', expression, terminator,
		{case statement},
		[default statement],
	'end', terminator;

case statement :
	'case' , case limits , (':', statement | terminator, {statement});

case limits : 
	'expression' | 
	'expression', 'to', 'expression' , { ',' , case limits };
	

try statement :
	'try', terminator
		{statement},
	{'catch' , [ name ] , [ 'in' , name ], terminator, 
		{statement}},
	'end', terminator;


const statement : 'const', multiple assignment, terminator;

def statement : 'def', multiple assignment, terminator;

multiple assignment : name, '=', expression, { ',' , name, '=', expression};
 


WS
   : [ \r\n\t] + -> skip
   ;
