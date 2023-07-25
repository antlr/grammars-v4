/* Bison Grammar Parser                             -*- C -*-

   Copyright (C) 2002-2015, 2018-2020 Free Software Foundation, Inc.

   This file is part of Bison, the GNU Compiler Compiler.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */


/*==========\
| Grammar.  |
\==========*/


parser grammar BisonParser;

options { tokenVocab=BisonLexer; }

input_
    : prologue_declarations '%%' bison_grammar epilogue_opt EOF
    ;

/*------------------------------------.
| Declarations: before the first %%.  |
`------------------------------------*/

prologue_declarations
    : prologue_declaration*
    ;

prologue_declaration
    : grammar_declaration
    | PROLOGUE
    | PERCENT_DEBUG
    | LOCATIONS
    | DEFINE variable value
    | DEFINES
    | DEFINES STRING
    | OBS_PERCENT_ERROR_VERBOSE
    | EXPECT INT
    | EXPECT_RR INT
    | PERCENT_FILE_PREFIX STRING
    | GLR_PARSER
    | INITIAL_ACTION actionBlock
    | LANGUAGE STRING
    | PERCENT_NAME_PREFIX STRING
    | NO_LINES
    | NONDETERMINISTIC_PARSER
    | OBS_OUTPUT STRING
    | PARAM params
    | PERCENT_PURE_PARSER
    | PARSE actionBlock+
    | LEX actionBlock
    | REQUIRE STRING
    | SKELETON STRING
    | TOKEN_TABLE
    | VERBOSE
    | PERCENT_YACC
    | SEMICOLON
    ;

params
    : params actionBlock
    | actionBlock
    ;


/*----------------------.
| grammar_declaration.  |
`----------------------*/

grammar_declaration
    : symbol_declaration
    | PERCENT_START symbol
    | code_props_type actionBlock generic_symlist
    | DEFAULT_PREC
    | NO_DEFAULT_PREC
    | CODE actionBlock
    | CODE ID actionBlock
    | PERCENT_UNION union_name actionBlock
    ;

code_props_type
    : DESTRUCTOR
    | PRINTER
    ;

/*---------.
| %union.  |
`---------*/

union_name
    :
    | ID
    ;

symbol_declaration
    : NTERM nterm_decls
    | PERCENT_TOKEN token_decls
    | PERCENT_TYPE symbol_decls
    | precedence_declarator token_decls_for_prec
    ;

precedence_declarator
    : PERCENT_LEFT
    | PERCENT_RIGHT
    | PERCENT_NONASSOC
    | PRECEDENCE
    ;

tag_opt
    :
    | TAG
    ;

generic_symlist
    : generic_symlist_item+
    ;

generic_symlist_item
    : symbol
    | tag
    ;

tag
    : TAG
    | TAG_ANY
    | TAG_NONE
    ;

/*-----------------------.
| nterm_decls (%nterm).  |
`-----------------------*/

// A non empty list of possibly tagged symbols for %nterm.
//
// Can easily be defined like symbol_decls but restricted to ID, but
// using token_decls allows to reudce the number of rules, and also to
// make nicer error messages on '%nterm 'a'' or '%nterm FOO 'foo''.

nterm_decls
    : token_decls
    ;

/*-----------------------------------.
| token_decls (%token, and %nterm).  |
`-----------------------------------*/

// A non empty list of possibly tagged symbols for %token or %nterm.

token_decls : ( | TAG ) token_decl+ ( TAG token_decl+ )* ;

// One symbol declaration for %token or %nterm.

token_decl
    : id int_opt alias
    | id id LPAREN id RPAREN alias    // Not in Bison, but used in https://github.com/ruby/ruby/parse.y
    ;

int_opt
    :
    | INT
    ;

alias
    :
    | string_as_id
//| TSTRING
    ;


/*-------------------------------------.
| token_decls_for_prec (%left, etc.).  |
`-------------------------------------*/

// A non empty list of possibly tagged tokens for precedence declaration.
//
// Similar to %token (token_decls), but in '%left FOO 1 'foo'', it treats
// FOO and 'foo' as two different symbols instead of aliasing them.

token_decls_for_prec
    : token_decl_for_prec+
    | TAG token_decl_for_prec+
    | token_decls_for_prec TAG token_decl_for_prec+
    ;

// One token declaration for precedence declaration.

token_decl_for_prec
    : id int_opt
    | string_as_id
    ;


/*-----------------------------------.
| symbol_decls (argument of %type).  |
`-----------------------------------*/

// A non empty list of typed symbols (for %type).

symbol_decls
    : symbol+
    | TAG symbol+
    | symbol_decls TAG symbol+
    ;

/*------------------------------------------.
| The grammar section: between the two %%.  |
`------------------------------------------*/

bison_grammar
    : rules_or_grammar_declaration
    | bison_grammar rules_or_grammar_declaration
    ;

/* As a Bison extension, one can use the grammar declarations in the
   body of the grammar.  */

rules_or_grammar_declaration
    : rules
    | grammar_declaration SEMICOLON
    ;

rules
    : id named_ref_opt COLON rhses_1
    ;

rhses_1
    : rhs ('|' rhs)* SEMICOLON
    ;

rhs
    : ( symbol named_ref_opt
        | tag_opt actionBlock named_ref_opt
        | BRACED_PREDICATE
        | EMPTY_RULE
        | PERCENT_PREC symbol
        | DPREC INT
        | MERGE TAG
        | EXPECT INT
        | EXPECT_RR INT)*
    ;

named_ref_opt
    :
    | BRACKETED_ID
    ;


/*---------------------.
| variable and value.  |
`---------------------*/

variable
    : ID
    ;

value
    :
    | ID
    | STRING
    | actionBlock
    ;


/*--------------.
| Identifiers.  |
`--------------*/

/* Identifiers are returned as uniqstr values by the scanner.
   Depending on their use, we may need to make them genuine symbols.  */

id
    : ID
    | CHAR
    ;

symbol
    : id
    | string_as_id
    ;

/* A string used as an ID: quote it.  */

string_as_id
    : STRING
    ;

epilogue_opt
    :
    | '%%' EPILOGUE?
    ;

actionBlock
    : BRACED_CODE
    ;
