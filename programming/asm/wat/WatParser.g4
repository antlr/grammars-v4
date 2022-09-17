/*
Copyright (c) 2019 Renata Hodovan.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

parser grammar WatParser;

options { tokenVocab=WatLexer; }


value
  : INT | FLOAT
  ;

/* Auxiliaries */

name
  : STRING_
  ;

/* Types */

value_type
  : VALUE_TYPE
  ;

elem_type
  : FUNCREF
  ;

global_type
  : value_type | LPAR MUT value_type RPAR
  ;

def_type
  : LPAR FUNC func_type RPAR
  ;

func_type
  : (LPAR (RESULT value_type* | PARAM value_type* | PARAM bind_var value_type) RPAR)*
  ;

table_type
  : NAT NAT? elem_type
  ;

memory_type
  : NAT NAT?
  ;

type_use
  : LPAR TYPE var_ RPAR
  ;

/* Immediates */

literal
  : NAT | INT | FLOAT
  ;

var_
  : NAT | VAR
  ;

bind_var
  : VAR
  ;

/* Instructions & Expressions */

instr
  : plain_instr
  | call_instr_instr
  | block_instr
  | expr
  ;

plain_instr
  : UNREACHABLE
  | NOP
  | DROP
  | SELECT
  | BR var_
  | BR_IF var_
  | BR_TABLE var_+
  | RETURN
  | CALL var_
  | LOCAL_GET var_
  | LOCAL_SET var_
  | LOCAL_TEE var_
  | GLOBAL_GET var_
  | GLOBAL_SET var_
  | LOAD OFFSET_EQ_NAT? ALIGN_EQ_NAT?
  | STORE OFFSET_EQ_NAT? ALIGN_EQ_NAT?
  | MEMORY_SIZE
  | MEMORY_GROW
  | CONST literal
  | TEST
  | COMPARE
  | UNARY
  | BINARY
  | CONVERT
  ;

call_instr
  : CALL_INDIRECT type_use? call_instr_params
  ;

call_instr_params
  : (LPAR PARAM value_type* RPAR)* (LPAR RESULT value_type* RPAR)*
  ;

call_instr_instr
  : CALL_INDIRECT type_use? call_instr_params_instr
  ;

call_instr_params_instr
  : (LPAR PARAM value_type* RPAR)* call_instr_results_instr
  ;

call_instr_results_instr
  : (LPAR RESULT value_type* RPAR)* instr
  ;

block_instr
  : (BLOCK | LOOP) bind_var? block END bind_var?
  | IF bind_var? block (ELSE bind_var? instr_list)? END bind_var?
  ;

block_type
  : LPAR RESULT value_type RPAR
  ;

block
  : block_type? instr_list
  ;

expr
  : LPAR expr1 RPAR
  ;

expr1
  : plain_instr expr*
  | CALL_INDIRECT call_expr_type
  | BLOCK bind_var? block
  | LOOP bind_var? block
  | IF bind_var? if_block
  ;

call_expr_type
  : type_use? call_expr_params
  ;

call_expr_params
  : (LPAR PARAM value_type* RPAR)* call_expr_results
  ;

call_expr_results
  : (LPAR RESULT value_type* RPAR)* expr*
  ;

if_block
  : block_type if_block
  | expr* LPAR THEN instr_list RPAR (LPAR ELSE instr_list RPAR)?
  ;

instr_list
  : instr* call_instr?
  ;

const_expr
  : instr_list
  ;

/* Functions */

func_
  : LPAR FUNC bind_var? func_fields RPAR
  ;

func_fields
  : type_use? func_fields_body
  | inline_import type_use? func_fields_import
  | inline_export func_fields
  ;

func_fields_import
  : (LPAR PARAM value_type* RPAR | LPAR PARAM bind_var value_type RPAR) func_fields_import_result
  ;

func_fields_import_result
  : (LPAR RESULT value_type* RPAR)*
  ;

func_fields_body
  : (LPAR PARAM value_type* RPAR | LPAR PARAM bind_var value_type RPAR)* func_result_body
  ;

func_result_body
  : (LPAR RESULT value_type* RPAR)* func_body
  ;

func_body
  : (LPAR LOCAL value_type* RPAR | LPAR LOCAL bind_var value_type RPAR)* instr_list
  ;

/* Tables, Memories & Globals */

offset
  : LPAR OFFSET const_expr RPAR
  | expr
  ;

elem
  : LPAR ELEM var_? offset var_* RPAR
  ;

table
  : LPAR TABLE bind_var? table_fields RPAR
  ;

table_fields
  : table_type
  | inline_import table_type
  | inline_export table_fields
  | elem_type LPAR ELEM var_* RPAR
  ;

data
  : LPAR DATA var_? offset STRING_* RPAR
  ;

memory
  : LPAR MEMORY bind_var? memory_fields RPAR
  ;

memory_fields
  : memory_type
  | inline_import memory_type
  | inline_export memory_fields
  | LPAR DATA STRING_* RPAR
  ;

sglobal
  : LPAR GLOBAL bind_var? global_fields RPAR
  ;

global_fields
  : global_type const_expr
  | inline_import global_type
  | inline_export global_fields
  ;

/* Imports & Exports */

import_desc
  : LPAR FUNC bind_var? type_use RPAR
  | LPAR FUNC bind_var? func_type RPAR
  | LPAR TABLE bind_var? table_type RPAR
  | LPAR MEMORY bind_var? memory_type RPAR
  | LPAR GLOBAL bind_var? global_type RPAR
  ;

simport
  :  LPAR IMPORT name name import_desc RPAR
  ;

inline_import
  : LPAR IMPORT name name RPAR
  ;

export_desc
  : LPAR FUNC var_ RPAR
  | LPAR TABLE var_ RPAR
  | LPAR MEMORY var_ RPAR
  | LPAR GLOBAL var_ RPAR
  ;

export_
  : LPAR EXPORT name export_desc RPAR
  ;

inline_export
  : LPAR EXPORT name RPAR
  ;

/* Modules */

type_
  : def_type
  ;

type_def
  : LPAR TYPE bind_var? type_ RPAR
  ;

start_
  : LPAR START_ var_ RPAR
  ;

module_field
  : type_def
  | sglobal
  | table
  | memory
  | func_
  | elem
  | data
  | start_
  | simport
  | export_
  ;

module_
  : LPAR MODULE VAR? module_field* RPAR
  ;

/* Scripts */

script_module
  : module_
  | LPAR MODULE VAR? (BIN | QUOTE) STRING_* RPAR
  ;

action_
  : LPAR INVOKE VAR? name const_list RPAR
  | LPAR GET VAR? name RPAR
  ;

assertion
  : LPAR ASSERT_MALFORMED script_module STRING_ RPAR
  | LPAR ASSERT_INVALID script_module STRING_ RPAR
  | LPAR ASSERT_UNLINKABLE script_module STRING_ RPAR
  | LPAR ASSERT_TRAP script_module STRING_ RPAR
  | LPAR ASSERT_RETURN action_ const_list RPAR
  | LPAR ASSERT_RETURN_CANONICAL_NAN action_ RPAR
  | LPAR ASSERT_RETURN_ARITHMETIC_NAN action_ RPAR
  | LPAR ASSERT_TRAP action_ STRING_ RPAR
  | LPAR ASSERT_EXHAUSTION action_ STRING_ RPAR
  ;

cmd
  : action_
  | assertion
  | script_module
  | LPAR REGISTER name VAR? RPAR
  | meta
  ;

meta
  : LPAR SCRIPT VAR? cmd* RPAR
  | LPAR INPUT VAR? STRING_ RPAR
  | LPAR OUTPUT VAR? STRING_ RPAR
  | LPAR OUTPUT VAR? RPAR
  ;

wconst
  : LPAR CONST literal RPAR
  ;

const_list
  : wconst*
  ;

script
  : cmd* EOF
  | module_field+ EOF
  ;

module
  : module_ EOF
  | module_field* EOF
  ;
