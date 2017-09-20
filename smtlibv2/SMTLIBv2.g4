/**
 * SMT-LIB (v2.6) grammar
 *
 * Grammar is baesd on the following specification:
 * http://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.6-r2017-07-18.pdf
 *
 * The MIT License (MIT)
 *
 * Copyright (c) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 **/

grammar SMTLIBv2;


// Lexer Rules Start


Comment
    : Semicolon ~[\r\n]* -> skip
    ;


ParOpen
    : '('
    ;

ParClose
    : ')'
    ;

Semicolon
    : ';'
    ;

String
    : '"' (PrintableCharNoDquote | WhiteSpaceChar)+ '"'
    ;

QuotedSymbol:
    '|' (PrintableCharNoBackslash | WhiteSpaceChar)+ '|'
    ;


// Predefined Symbols

PS_Not
    : 'not'
    ;
PS_Bool
    : 'Bool'
    ;
PS_ContinuedExecution
    : 'continued-execution'
    ;
PS_Error
    : 'error'
    ;
PS_False
    : 'false'
    ;
PS_ImmediateExit
    : 'immediate-exit'
    ;
PS_Incomplete
    : 'incomplete'
    ;
PS_Logic
    : 'logic'
    ;
PS_Memout
    : 'memout'
    ;
PS_Sat
    : 'sat'
    ;
PS_Success
    : 'success'
    ;
PS_Theory
    : 'theory'
    ;
PS_True
    : 'true'
    ;
PS_Unknown
    : 'unknown'
    ;
PS_Unsupported
    : 'unsupported'
    ;
PS_Unsat
    : 'unsat'
    ;

// RESERVED Words

// Command names


CMD_Assert
    : 'assert'
    ;
CMD_CheckSat
    : 'check-sat'
    ;
CMD_CheckSatAssuming
    : 'check-sat-assuming'
    ;
CMD_DeclareConst
    : 'declare-const'
    ;
CMD_DeclareDatatype
    : 'declare-datatype'
    ;
CMD_DeclareDatatypes
    : 'declare-datatypes'
    ;
CMD_DeclareFun
    : 'declare-fun'
    ;
CMD_DeclareSort
    : 'declare-sort'
    ;
CMD_DefineFun
    : 'define-fun'
    ;
CMD_DefineFunRec
    : 'define-fun-rec'
    ;
CMD_DefineFunsRec
    : 'define-funs-rec'
    ;
CMD_DefineSort
    : 'define-sort'
    ;
CMD_Echo
    : 'echo'
    ;
CMD_Exit
    : 'exit'
    ;
CMD_GetAssertions
    : 'get-assertions'
    ;
CMD_GetAssignment
    : 'get-assignment'
    ;
CMD_GetInfo
    : 'get-info'
    ;
CMD_GetModel
    : 'get-model'
    ;
CMD_GetOption
    : 'get-option'
    ;
CMD_GetProof
    : 'get-proof'
    ;
CMD_GetUnsatAssumptions
    : 'get-unsat-assumptions'
    ;
CMD_GetUnsatCore
    : 'get-unsat-core'
    ;
CMD_GetValue
    : 'get-value'
    ;
CMD_Pop
    : 'pop'
    ;
CMD_Push
    : 'push'
    ;
CMD_Reset
    : 'reset'
    ;
CMD_ResetAssertions
    : 'reset-assertions'
    ;
CMD_SetInfo
    : 'set-info'
    ;
CMD_SetLogic
    : 'set-logic'
    ;
CMD_SetOption
    : 'set-option'
    ;




// General reserved words

GRW_Exclamation
    : '!'
    ;
GRW_Underscore
    : '_'
    ;
GRW_As
    : 'as'
    ;
GRW_Binary
    : 'BINARY'
    ;
GRW_Decimal
    : 'DECIMAL'
    ;
GRW_Exists
    : 'exists'
    ;
GRW_Hexadecimal
    : 'HEXADECIMAL'
    ;
GRW_Forall
    : 'forall'
    ;
GRW_Let
    : 'let'
    ;
GRW_Match
    : 'match'
    ;
GRW_Numeral
    : 'NUMERAL'
    ;
GRW_Par
    : 'par'
    ;
GRW_String
    : 'string'
    ;

Numeral
    : '0'
    | [1-9] Digit*
    ;

Binary:
    BinaryDigit+
    ;

HexDecimal
    : '#x' HexDigit HexDigit HexDigit HexDigit
    ;

Decimal
    : Numeral '.' '0'* Numeral
    ;



fragment HexDigit
    : '0' .. '9' | 'a' .. 'f' | 'A' .. 'F'
    ;


Colon
    : ':'
    ;

fragment Digit
    : [0-9]
    ;

fragment Sym
    : 'a'..'z'
    | 'A' .. 'Z'
    | '+'
    | '='
    | '/'
    | '*'
    | '%'
    | '?'
    | '!'
    | '$'
    | '-'
    | '_'
    | '~'
    | '&'
    | '^'
    | '<'
    | '>'
    | '@'
    | '.'
    ;



fragment BinaryDigit
    : [01]
    ;

fragment PrintableChar
    : '\u0020' .. '\u007E'
    | '\u0080' .. '\uffff'
    | EscapedSpace
    ;

fragment PrintableCharNoDquote
    : '\u0020' .. '\u0021'
    | '\u0023' .. '\u007E'
    | '\u0080' .. '\uffff'
    | EscapedSpace
    ;

fragment PrintableCharNoBackslash
    : '\u0020' .. '\u005B'
    | '\u005D' .. '\u007B'
    | '\u007D' .. '\u007E'
    | '\u0080' .. '\uffff'
    | EscapedSpace
    ;

fragment EscapedSpace
    : '""'
    ;

fragment WhiteSpaceChar
    : '\u0009'
    | '\u000A'
    | '\u000D'
    | '\u0020'
    ;

// Lexer Rules End

// Predefined Keywords



PK_AllStatistics
    : ':all-statistics'
    ;
PK_AssertionStackLevels
    : ':assertion-stack-levels'
    ;
PK_Authors
    : ':authors'
    ;
PK_Category
    : ':category'
    ;
PK_Chainable
    : ':chainable'
    ;
PK_Definition
    : ':definition'
    ;
PK_DiagnosticOutputChannel
    : ':diagnostic-output-channel'
    ;
PK_ErrorBehaviour
    : ':error-behavior'
    ;
PK_Extension
    : ':extensions'
    ;
PK_Funs
    : ':funs'
    ;
PK_FunsDescription
    : ':funs-description'
    ;
PK_GlobalDeclarations
    : ':global-declarations'
    ;
PK_InteractiveMode
    : ':interactive-mode'
    ;
PK_Language
    : ':language'
    ;
PK_LeftAssoc
    : ':left-assoc'
    ;
PK_License
    : ':license'
    ;
PK_Named
    : ':named'
    ;
PK_Name
    : ':name'
    ;
PK_Notes
    : ':notes'
    ;
PK_Pattern
    : ':pattern'
    ;
PK_PrintSuccess
    : ':print-success'
    ;
PK_ProduceAssertions
    : ':produce-assertions'
    ;
PK_ProduceAssignments
    : ':produce-assignments'
    ;
PK_ProduceModels
    : ':produce-models'
    ;
PK_ProduceProofs
    : ':produce-proofs'
    ;
PK_ProduceUnsatAssumptions
    : ':produce-unsat-assumptions'
    ;
PK_ProduceUnsatCores
    : ':produce-unsat-cores'
    ;
PK_RandomSeed
    : ':random-seed'
    ;
PK_ReasonUnknown
    : ':reason-unknown'
    ;
PK_RegularOutputChannel
    : ':regular-output-channel'
    ;
PK_ReproducibleResourceLimit
    : ':reproducible-resource-limit'
    ;
PK_RightAssoc
    : ':right-assoc'
    ;
PK_SmtLibVersion
    : ':smt-lib-version'
    ;
PK_Sorts
    : ':sorts'
    ;
PK_SortsDescription
    : ':sorts-description'
    ;
PK_Source
    : ':source'
    ;
PK_Status
    : ':status'
    ;
PK_Theories
    : ':theories'
    ;
PK_Values
    : ':values'
    ;
PK_Verbosity
    : ':verbosity'
    ;
PK_Version
    : ':version'
    ;

UndefinedSymbol:
    Sym (Digit | Sym)*;



// Parser Rules Start

// Starting rule(s)

start
    : script EOF
    ;

response
    : general_response EOF
    ;

generalReservedWord
    : GRW_Exclamation
    | GRW_Underscore
    | GRW_As
    | GRW_Binary
    | GRW_Decimal
    | GRW_Exists
    | GRW_Hexadecimal
    | GRW_Forall
    | GRW_Let
    | GRW_Match
    | GRW_Numeral
    | GRW_Par
    | GRW_String
    ;


simpleSymbol
    : predefSymbol
    | UndefinedSymbol
    ;

quotedSymbol
    : QuotedSymbol
    ;

predefSymbol
    : PS_Not
    | PS_Bool
    | PS_ContinuedExecution
    | PS_Error
    | PS_False
    | PS_ImmediateExit
    | PS_Incomplete
    | PS_Logic
    | PS_Memout
    | PS_Sat
    | PS_Success
    | PS_Theory
    | PS_True
    | PS_Unknown
    | PS_Unsupported
    | PS_Unsat
    ;

predefKeyword
    : PK_AllStatistics
    | PK_AssertionStackLevels
    | PK_Authors
    | PK_Category
    | PK_Chainable
    | PK_Definition
    | PK_DiagnosticOutputChannel
    | PK_ErrorBehaviour
    | PK_Extension
    | PK_Funs
    | PK_FunsDescription
    | PK_GlobalDeclarations
    | PK_InteractiveMode
    | PK_Language
    | PK_LeftAssoc
    | PK_License
    | PK_Named
    | PK_Name
    | PK_Notes
    | PK_Pattern
    | PK_PrintSuccess
    | PK_ProduceAssertions
    | PK_ProduceAssignments
    | PK_ProduceModels
    | PK_ProduceProofs
    | PK_ProduceUnsatAssumptions
    | PK_ProduceUnsatCores
    | PK_RandomSeed
    | PK_ReasonUnknown
    | PK_RegularOutputChannel
    | PK_ReproducibleResourceLimit
    | PK_RightAssoc
    | PK_SmtLibVersion
    | PK_Sorts
    | PK_SortsDescription
    | PK_Source
    | PK_Status
    | PK_Theories
    | PK_Values
    | PK_Verbosity
    | PK_Version
    ;



symbol
    : simpleSymbol
    | quotedSymbol
    ;

numeral
    : Numeral
    ;

decimal
    : Decimal
    ;

hexadecimal
    : HexDecimal
    ;

binary
    : Binary
    ;

string
    : String
    ;

keyword
    : predefKeyword
    | Colon simpleSymbol
    ;

// S-expression

spec_constant
    : numeral
    | decimal
    | hexadecimal
    | binary
    | string
    ;


s_expr
    : spec_constant
    | symbol
    | keyword
    | ParOpen s_expr* ParClose
    ;

// Identifiers

index
    : numeral
    | symbol
    ;

identifier
    : symbol
    | ParOpen GRW_Underscore symbol index+ ParClose
    ;

// Attributes

attribute_value
    : spec_constant
    | symbol
    | ParOpen s_expr* ParClose
    ;

attribute
    : keyword
    | keyword attribute_value
    ;

// Sorts

sort
    : identifier
    | ParOpen identifier sort+ ParClose
    ;


// Terms and Formulas

qual_identifer
    : identifier
    | ParOpen GRW_As identifier sort ParClose
    ;

var_binding
    : ParOpen symbol term ParClose
    ;

sorted_var
    : ParOpen symbol sort ParClose
    ;

pattern
    : symbol
    | ParOpen symbol symbol+ ParClose
    ;

match_case
    : ParOpen pattern term ParClose
    ;

term
    : spec_constant
    | qual_identifer
    | ParOpen qual_identifer term+ ParClose
    | ParOpen GRW_Let ParOpen var_binding+ ParClose term ParClose
    | ParOpen GRW_Forall ParOpen sorted_var+ ParClose term ParClose
    | ParOpen GRW_Exists ParOpen sorted_var+ ParClose term ParClose
    | ParOpen GRW_Match term ParOpen match_case+ ParClose ParClose
    | ParOpen GRW_Exclamation term attribute+ ParClose
    ;


// Theory Declarations

sort_symbol_decl
    : ParOpen identifier numeral attribute* ParClose;

meta_spec_constant
    : GRW_Numeral
    | GRW_Decimal
    | GRW_String
    ;

fun_symbol_decl
    : ParOpen spec_constant sort attribute* ParClose
    | ParOpen meta_spec_constant sort attribute* ParClose
    | ParOpen identifier sort+ attribute* ParClose
    ;

par_fun_symbol_decl
    : fun_symbol_decl
    | ParOpen GRW_Par ParOpen symbol+ ParClose ParOpen identifier sort+
    attribute* ParClose ParClose
    ;

theory_attribute
    : PK_Sorts ParOpen sort_symbol_decl+ ParClose
    | PK_Funs ParOpen par_fun_symbol_decl+ ParClose
    | PK_SortsDescription string
    | PK_FunsDescription string
    | PK_Definition string
    | PK_Values string
    | PK_Notes string
    | attribute
    ;

theory_decl
    : ParOpen PS_Theory symbol theory_attribute+ ParClose
    ;


// Logic Declarations

logic_attribue
    : PK_Theories ParOpen symbol+ ParClose
    | PK_Language string
    | PK_Extension string
    | PK_Values string
    | PK_Notes string
    | attribute
    ;

logic
    : ParOpen PS_Logic symbol logic_attribue+ ParClose
    ;


// Scripts

sort_dec
    : ParOpen symbol numeral ParClose
    ;

selector_dec
    : ParOpen symbol sort ParClose
    ;

constructor_dec
    : ParOpen symbol selector_dec* ParClose
    ;

datatype_dec
    : ParOpen constructor_dec+ ParClose
    | ParOpen GRW_Par ParOpen symbol+ ParClose ParOpen constructor_dec+
    ParClose ParClose
    ;

function_dec
    : ParOpen symbol ParOpen sorted_var* ParClose sort ParClose
    ;

function_def
    : symbol ParOpen sorted_var* ParClose sort term
    ;

prop_literal
    : symbol
    | ParOpen PS_Not symbol ParClose
    ;


script
    : command*
    ;

cmd_assert
    : CMD_Assert
    ;

cmd_checkSat
    : CMD_CheckSat
    ;

cmd_checkSatAssuming
    : CMD_CheckSatAssuming
    ;

cmd_declareConst
    : CMD_DeclareConst
    ;

cmd_declareDatatype
    : CMD_DeclareDatatype
    ;

cmd_declareDatatypes
    : CMD_DeclareDatatypes
    ;

cmd_declareFun
    : CMD_DeclareFun
    ;

cmd_declareSort
    : CMD_DeclareSort
    ;

cmd_defineFun
    : CMD_DefineFun
    ;

cmd_defineFunRec
    : CMD_DefineFunRec
    ;

cmd_defineFunsRec
    : CMD_DefineFunsRec
    ;

cmd_defineSort
    : CMD_DefineSort
    ;

cmd_echo
    : CMD_Echo
    ;

cmd_exit
    : CMD_Exit
    ;

cmd_getAssertions
    : CMD_GetAssertions
    ;

cmd_getAssignment
    : CMD_GetAssignment
    ;

cmd_getInfo
    : CMD_GetInfo
    ;

cmd_getModel
    : CMD_GetModel
    ;

cmd_getOption
    : CMD_GetOption
    ;

cmd_getProof
    : CMD_GetProof
    ;

cmd_getUnsatAssumptions
    : CMD_GetUnsatAssumptions
    ;

cmd_getUnsatCore
    : CMD_GetUnsatCore
    ;

cmd_getValue
    : CMD_GetValue
    ;

cmd_pop
    : CMD_Pop
    ;

cmd_push
    : CMD_Push
    ;

cmd_reset
    : CMD_Reset
    ;

cmd_resetAssertions
    : CMD_ResetAssertions
    ;

cmd_setInfo
    : CMD_SetInfo
    ;

cmd_setLogic
    : CMD_SetLogic
    ;

cmd_setOption
    : CMD_SetOption
    ;

command
    : ParOpen cmd_assert term ParClose
    | ParOpen cmd_checkSat ParClose
    | ParOpen cmd_checkSatAssuming ParClose
    | ParOpen cmd_declareConst symbol sort ParClose
    | ParOpen cmd_declareDatatype symbol datatype_dec ParClose
    // cardinalitiees for sort_dec and datatype_dec have to be n+1
    | ParOpen cmd_declareDatatypes ParOpen sort_dec+ ParClose ParOpen
    datatype_dec+ ParClose ParClose
    | ParOpen cmd_declareFun symbol ParOpen sort* ParClose sort ParClose
    | ParOpen cmd_declareSort symbol numeral ParClose
    | ParOpen cmd_defineFun function_def ParClose
    | ParOpen cmd_defineFunRec function_def ParClose
    // cardinalitiees for function_dec and term have to be n+1
    | ParOpen cmd_defineFunsRec ParOpen function_dec+ ParClose
    ParOpen term+ ParClose ParClose
    | ParOpen cmd_defineSort symbol ParOpen symbol* ParClose sort ParClose
    | ParOpen cmd_echo string ParClose
    | ParOpen cmd_exit ParClose
    | ParOpen cmd_getAssertions ParClose
    | ParOpen cmd_getAssignment ParClose
    | ParOpen cmd_getInfo info_flag ParClose
    | ParOpen cmd_getModel ParClose
    | ParOpen cmd_getOption keyword ParClose
    | ParOpen cmd_getProof ParClose
    | ParOpen cmd_getUnsatAssumptions ParClose
    | ParOpen cmd_getUnsatCore ParClose
    | ParOpen cmd_getValue ParOpen term+ ParClose ParClose
    | ParOpen cmd_pop numeral ParClose
    | ParOpen cmd_push numeral ParClose
    | ParOpen cmd_reset ParClose
    | ParOpen cmd_resetAssertions ParClose
    | ParOpen cmd_setInfo attribute ParClose
    | ParOpen cmd_setLogic symbol ParClose
    | ParOpen cmd_setOption option ParClose
    ;


b_value
    : PS_True
    | PS_False
    ;

option
    : PK_DiagnosticOutputChannel string
    | PK_GlobalDeclarations b_value
    | PK_InteractiveMode b_value
    | PK_PrintSuccess b_value
    | PK_ProduceAssertions b_value
    | PK_ProduceAssignments b_value
    | PK_ProduceModels b_value
    | PK_ProduceProofs b_value
    | PK_ProduceUnsatAssumptions b_value
    | PK_ProduceUnsatCores b_value
    | PK_RandomSeed numeral
    | PK_RegularOutputChannel string
    | PK_ReproducibleResourceLimit numeral
    | PK_Verbosity numeral
    | attribute
    ;

info_flag
    : PK_AllStatistics
    | PK_AssertionStackLevels
    | PK_Authors
    | PK_ErrorBehaviour
    | PK_Name
    | PK_ReasonUnknown
    | PK_Version
    | keyword
    ;

// responses

error_behaviour
    : PS_ImmediateExit
    | PS_ContinuedExecution
    ;

reason_unknown
    : PS_Memout
    | PS_Incomplete
    | s_expr
    ;

model_response
    : ParOpen CMD_DefineFun function_def ParClose
    | ParOpen CMD_DefineFunRec function_def ParClose
    // cardinalitiees for function_dec and term have to be n+1
    | ParOpen CMD_DefineFunsRec ParOpen function_dec+ ParClose ParOpen term+
    ParClose ParClose
    ;

info_response
    : PK_AssertionStackLevels numeral
    | PK_Authors string
    | PK_ErrorBehaviour error_behaviour
    | PK_Name string
    | PK_ReasonUnknown reason_unknown
    | PK_Version string
    | attribute
    ;

valuation_pair
    : ParOpen term term ParClose
    ;

t_valuation_pair
    : ParOpen symbol b_value ParClose
    ;

check_sat_response
    : PS_Sat
    | PS_Unsat
    | PS_Unknown
    ;

echo_response
    : string
    ;

get_assertions_response
    : ParOpen term* ParClose
    ;

get_assignment_response
    : ParOpen t_valuation_pair* ParClose
    ;

get_info_response
    : ParOpen info_response+ ParClose
    ;

get_model_response
    : ParOpen model_response* ParClose
    ;

get_option_response
    : attribute_value
    ;

get_proof_response
    : s_expr
    ;

get_unsat_assump_response
    : ParOpen symbol* ParClose
    ;

get_unsat_core_response
    : ParOpen symbol* ParClose
    ;

get_value_response
    : ParOpen valuation_pair+ ParClose
    ;

specific_success_response
    : check_sat_response
    | echo_response
    | get_assertions_response
    | get_assignment_response
    | get_info_response
    | get_model_response
    | get_option_response
    | get_proof_response
    | get_unsat_assump_response
    | get_unsat_core_response
    | get_value_response
    ;

general_response
    : PS_Success
    | specific_success_response
    | PS_Unsupported
    | ParOpen PS_Error string ParClose
    ;


// Parser Rules End

WS  :  [ \t\r\n]+ -> skip
    ;
