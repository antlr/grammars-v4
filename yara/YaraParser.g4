/*
YARA grammar.
The MIT License (MIT).

Copyright (c) 2022, Michał Lorek.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

parser grammar YaraParser;

options { tokenVocab=YaraLexer; }

startRule
    : decl+ EOF
    ;

decl
    : import_decl+
    | include_decl+
    | rule_decl+
    ;

import_decl
    : IMPORT string
    ;

include_decl
    : INCLUDE string
    ;

rule_decl
    : rule_modifier* RULE id_ tags?
        LCB
            meta_section?
            strings_section?
            condition_section
        RCB
    ;

rule_modifier
    : PRIVATE
    | GLOBAL
    ;

tags
    : COLON id_+
    ;

id_
    : ID
    ;

condition_section
    : CONDITION COLON (expr | for_expr)
    ;

for_expr
    : FOR expr COLON LP bool_expr RP
    | FOR ANY id_ (COMMA id_)* IN expr COLON LP bool_expr RP
    ;

bool_expr
    : expr
    | NOT+ bool_expr
    | bool_expr AND bool_expr
    | bool_expr OR bool_expr
    ;

expr
    : expr LSB expr RSB
    | expr DOT expr
    | ('-'|'~') expr
    | fn4 LP expr RP

    | expr ('*'|'\\'|'%') expr
    | expr ('+'|'-') expr
    | expr ('<<'|'>>') expr
    | expr ('&'|'^'|'|') expr
    | expr ('<'|'<='|'>'|'>='|'=='|'!=') expr

    | (number | ALL | ANY | NONE) OF (string_set | LP ID RP | THEM) (IN range)? // number could be an expr
    | (COUNT_REF | STRING_ID) IN range
    | (number | ALL | ANY | NONE) id_? IN range
    | ('$' | STRING_ID) AT expr
    | LP number (COMMA number)* RP
    | expr function expr
    | NOT+ expr
    | expr AND expr
    | expr OR expr
    | LP expr RP
    | literal
    | pos_fn
    | '$' | '#' | '@' | '!'
    ;

literal
    : true_false
    | number
    | STRING_ID
    | HEX_LITERAL
    | COUNT_REF
    | OFFSET_REF
    | LENGTH_REF
    | SIZE_LITERAL
    | ID
    | DOUBLE_QUOTE_STR
    ;

string_set
    : LP string_id_list RP
    ;

string_id_list
    : (STRING_ID | STRING_WILD) (COMMA (STRING_ID | STRING_WILD))*
    ;

function
    : CONTAINS
    | ICONTAINS
    | STARTSWITH
    | ISTARTSWITH
    | ENDSWITH
    | IENDSWITH
    | IEQUALS
    | MATCHES
    ;

pos_fn
    : ENTRYPOINT
    | FILESIZE
    ;

fn4
    : INT16
    | INT16BE
    | UINT16
    | UINT16BE
    | INT32
    | INT32BE
    | UINT32
    | UINT32BE
    | INT8
    | INT8BE
    | UINT8
    | UINT8BE
    ;

range
    : LP expr '..' expr RP
    ;

true_false
    : TRUE | FALSE
    ;

strings_section
    : STRINGS COLON
        string_def+
    ;

meta_section
    : META COLON
        meta_def+
    ;

string_def
    : STRING_ID ASSIGN_S string_value string_modifier*
    ;

string_modifier
    : NOCASE
    | WIDE
    | ASCII
    | XOR
    | BASE64 args?
    | BASE64WIDE args?
    | FULLWORD
    | PRIVATE
    ;

args
    : LP string RP
    ;

meta_def
    : id_ ASSIGN meta_value
    ;

string_value
    : string
    | LCB_S string_construct+ RCB
    | REGEX_STR
    ;

string_construct
    : HEX_STR
    | (HEX_STR | alt) jump+ (HEX_STR | alt)
    | alt
    ;

alt
    : LP HEX_STR+ (PIPE HEX_STR+)+ RP
    ;

jump
    : LSB (DEC? (MINUS DEC?)?) RSB
    ;

meta_value
    : string
    | true_false
    | number
    | HEX_LITERAL
    ;

string
    : DOUBLE_QUOTE_STR
    ;

number
    : DECIMAL_LITERAL
    ;
