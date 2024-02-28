/**
 * Copyright (c) 2017+ LocalStack contributors
 * Copyright (c) 2016 Atlassian Pty Ltd

 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at

 *     http://www.apache.org/licenses/LICENSE-2.0

 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * Amazon States Lanaguage (ASL) Grammar for ANTLR v4
 *
 * Based on:
 * http://github.com/localstack/localstack
 * and
 * https://states-language.net/spec.html
 */

// $antlr-format alignTrailingComments true, columnLimit 150, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine true, allowShortBlocksOnASingleLine true, minEmptyLines 0, alignSemicolons ownLine
// $antlr-format alignColons trailing, singleLineOverrulesHangingColon true, alignLexerCommands true, alignLabels true, alignTrailers true

parser grammar ASLIntrinsicParser;

options {
    tokenVocab = ASLIntrinsicLexer;
}

intrinsic_function: states_func_decl EOF;

states_func_decl: States DOT state_fun_name func_arg_list;

state_fun_name:
    Format
    | StringToJson
    | JsonToString
    | Array
    | ArrayPartition
    | ArrayContains
    | ArrayRange
    | ArrayGetItem
    | ArrayLength
    | ArrayUnique
    | Base64Encode
    | Base64Decode
    | Hash
    | JsonMerge
    | MathRandom
    | MathAdd
    | StringSplit
    | UUID
;

func_arg_list: LPAREN func_arg (COMMA func_arg)* RPAREN | LPAREN RPAREN;

func_arg:
    STRING                # func_arg_string
    | INT                 # func_arg_int
    | NUMBER              # func_arg_float
    | (TRUE | FALSE)      # func_arg_bool
    | CONTEXT_PATH_STRING # func_arg_context_path
    | JSON_PATH_STRING    # func_arg_json_path
    | states_func_decl    # func_arg_func_decl
;