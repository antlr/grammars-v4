/*
Zig language grammar.
The MIT License (MIT).

Copyright (c) 2025, Michał Lorek.

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

parser grammar ZigParser;

options {
    tokenVocab = ZigLexer;
}

root
    : container_members EOF;

//# *** Top level ***
//ContainerMembers <- container_doc_comment? ContainerDeclaration* (ContainerField COMMA)* (ContainerField / ContainerDeclaration*)
container_members
    : Container_doc_comment? container_declaration* (container_field ',')* (container_field | container_declaration*)
    ;
//ContainerDeclaration <- TestDecl / ComptimeDecl / doc_comment? KEYWORD_pub? Decl
container_declaration
    : test_decl
    | comptime_decl
    | Doc_comment? PUB? decl
    ;
//TestDecl <- KEYWORD_test (STRINGLITERALSINGLE / IDENTIFIER)? Block
test_decl
    : TEST (STRINGLITERALSINGLE | IDENTIFIER) block
    ;
//ComptimeDecl <- KEYWORD_comptime Block
comptime_decl
    : COMPTIME block
    ;
//Decl
//    <- (KEYWORD_export / KEYWORD_extern STRINGLITERALSINGLE? / KEYWORD_inline / KEYWORD_noinline)? FnProto (SEMICOLON / Block)
//     / (KEYWORD_export / KEYWORD_extern STRINGLITERALSINGLE?)? KEYWORD_threadlocal? GlobalVarDecl
decl
    : (EXPORT | EXTERN STRINGLITERALSINGLE? | INLINE | NOINLINE)? fn_proto (';' | block)
    | (EXPORT | EXTERN STRINGLITERALSINGLE)? THREADLOCAL? global_var_decl
    ;
//FnProto <- KEYWORD_fn IDENTIFIER? LPAREN ParamDeclList RPAREN ByteAlign? AddrSpace? LinkSection? CallConv? EXCLAMATIONMARK? TypeExpr
fn_proto
    : FN IDENTIFIER? '(' param_decl_list ')' byte_align? addr_space? link_section? call_conv? '!' type_expr
    ;
//VarDeclProto <- (KEYWORD_const / KEYWORD_var) IDENTIFIER (COLON TypeExpr)? ByteAlign? AddrSpace? LinkSection?
var_decl_proto
    : (CONST | VAR) IDENTIFIER (':' type_expr)? byte_align? addr_space? link_section?
    ;
//GlobalVarDecl <- VarDeclProto (EQUAL Expr)? SEMICOLON
global_var_decl
    : var_decl_proto ('=' expr)? ';'
    ;
//ContainerField <- doc_comment? KEYWORD_comptime? !KEYWORD_fn (IDENTIFIER COLON)? TypeExpr ByteAlign? (EQUAL Expr)?
container_field
    : Doc_comment? COMPTIME?
    ;
//Statement
//    <- KEYWORD_comptime ComptimeStatement
//     / KEYWORD_nosuspend BlockExprStatement
//     / KEYWORD_suspend BlockExprStatement
//     / KEYWORD_defer BlockExprStatement
//     / KEYWORD_errdefer Payload? BlockExprStatement
//     / IfStatement
//     / LabeledStatement
//     / VarDeclExprStatement
statement
    : COMPTIME comptime_statement
    | NOSUSPEND block_expr_statement
    | SUSPEND block_expr_statement
    | DEFER block_expr_statement
    | ERRDEFER payload? block_expr_statement
    | if_statement
    | labeled_statement
    | var_decl_expr_statement
    ;
//ComptimeStatement
//    <- BlockExpr
//     / VarDeclExprStatement
comptime_statement
    : block_expr
    | var_decl_expr_statement
    ;
//IfStatement
//    <- IfPrefix BlockExpr ( KEYWORD_else Payload? Statement )?
//     / IfPrefix AssignExpr ( SEMICOLON / KEYWORD_else Payload? Statement )
//
if_statement
    : if_prefix block_expr (ELSE payload? statement)?
    | if_prefix assign_expr (';' | ELSE payload? statement)
    ;
//LabeledStatement <- BlockLabel? (Block / LoopStatement / SwitchExpr)
labeled_statement
    : block_label? (block | loop_statement | switch_expr)
    ;
//LoopStatement <- KEYWORD_inline? (ForStatement / WhileStatement)
loop_statement
    : INLINE? (for_statement | while_statement)
    ;
//ForStatement
//    <- ForPrefix BlockExpr ( KEYWORD_else Statement )?
//     / ForPrefix AssignExpr ( SEMICOLON / KEYWORD_else Statement )
for_statement
    : for_prefix block_expr (ELSE statement)?
    | for_prefix assign_expr (';' ELSE statement)
    ;
//WhileStatement
//    <- WhilePrefix BlockExpr ( KEYWORD_else Payload? Statement )?
//     / WhilePrefix AssignExpr ( SEMICOLON / KEYWORD_else Payload? Statement )
while_statement
    : while_prefix block_expr (ELSE payload? statement)?
    | while_prefix assign_expr (';' | ELSE payload? statement)
    ;
//BlockExprStatement
//    <- BlockExpr
//     / AssignExpr SEMICOLON
block_expr_statement
    : block_expr
    | assign_expr ';'
    ;
//BlockExpr <- BlockLabel? Block
block_expr
    : block_label? block
    ;
//VarDeclExprStatement
//    <- VarDeclProto (COMMA (VarDeclProto / Expr))* EQUAL Expr SEMICOLON
//     / Expr (AssignOp Expr / (COMMA (VarDeclProto / Expr))+ EQUAL Expr)? SEMICOLON
var_decl_expr_statement
    : var_decl_proto (',' (var_decl_proto | expr))* '=' expr ';'
    | expr (assign_op expr | (',' (var_decl_proto | expr))+ '=' expr)? ';'
    ;
//# *** Expression Level ***
//
//# An assignment or a destructure whose LHS are all lvalue expressions.
//AssignExpr <- Expr (AssignOp Expr / (COMMA Expr)+ EQUAL Expr)?
assign_expr
    : expr (assign_op expr | (',' expr)+ '=' expr)?
    ;
//SingleAssignExpr <- Expr (AssignOp Expr)?
simple_assign_expr
    : expr (assign_op expr)?
    ;
//Expr <- BoolOrExpr
expr
    : bool_or_expr
    ;
//BoolOrExpr <- BoolAndExpr (KEYWORD_or BoolAndExpr)*
bool_or_expr
    : bool_and_expr (OR bool_and_expr)*
    ;
//BoolAndExpr <- CompareExpr (KEYWORD_and CompareExpr)*
bool_and_expr
    : compare_expr (AND compare_expr)*
    ;
//CompareExpr <- BitwiseExpr (CompareOp BitwiseExpr)?
compare_expr
    : bitwise_expr (compare_op bitwise_expr)?
    ;
//BitwiseExpr <- BitShiftExpr (BitwiseOp BitShiftExpr)*
bitwise_expr
    : bit_shift_expr (bitwise_op bit_shift_expr)*
    ;
//BitShiftExpr <- AdditionExpr (BitShiftOp AdditionExpr)*
bit_shift_expr
    : addition_expr (bit_shift_op addition_expr)*
    ;
//AdditionExpr <- MultiplyExpr (AdditionOp MultiplyExpr)*
addition_expr
    : multiply_expr (addition_op multiply_expr)*
    ;
//MultiplyExpr <- PrefixExpr (MultiplyOp PrefixExpr)*
multiply_expr
    : prefix_expr (multiply_op prefix_expr)*
    ;
//PrefixExpr <- PrefixOp* PrimaryExpr
prefix_expr
    : prefix_op* primary_expr
    ;
//PrimaryExpr
//    <- AsmExpr
//     / IfExpr
//     / KEYWORD_break BreakLabel? Expr?
//     / KEYWORD_comptime Expr
//     / KEYWORD_nosuspend Expr
//     / KEYWORD_continue BreakLabel? Expr?
//     / KEYWORD_resume Expr
//     / KEYWORD_return Expr?
//     / BlockLabel? LoopExpr
//     / Block
//     / CurlySuffixExpr
primary_expr
    : asm_expr
    | if_expr
    | BREAK break_label? expr?
    | COMPTIME expr
    | NOSUSPEND expr
    | CONTINUE break_label? expr?
    | RESUME expr
    | RETURN expr?
    | block_label? loop_expr
    | block
    | curly_suffix_expr
    ;
//IfExpr <- IfPrefix Expr (KEYWORD_else Payload? Expr)?
if_expr
    : if_prefix expr (ELSE payload? expr)?
    ;
//Block <- LBRACE Statement* RBRACE
block
    : '{' statement* '}'
    ;
//LoopExpr <- KEYWORD_inline? (ForExpr / WhileExpr)
loop_expr
    : INLINE? (for_expr | while_expr)
    ;
//ForExpr <- ForPrefix Expr (KEYWORD_else Expr)?
for_expr
    : for_prefix expr (ELSE expr)?
    ;
//WhileExpr <- WhilePrefix Expr (KEYWORD_else Payload? Expr)?
while_expr
    : while_prefix expr (ELSE payload? expr)?
    ;
//CurlySuffixExpr <- TypeExpr InitList?
curly_suffix_expr
    : type_expr init_list?
    ;
//InitList
//    <- LBRACE FieldInit (COMMA FieldInit)* COMMA? RBRACE
//     / LBRACE Expr (COMMA Expr)* COMMA? RBRACE
//     / LBRACE RBRACE
init_list
    : '{' field_init (',' field_init)* ','? '}'
    | '{' expr (',' expr)* ','? '}'
    | '{' '}'
    ;
//TypeExpr <- PrefixTypeOp* ErrorUnionExpr
type_expr
    : prefix_type_op* error_union_expr
    ;
// ErrorUnionExpr <- SuffixExpr (EXCLAMATIONMARK TypeExpr)?
error_union_expr
    : suffix_expr ('!' type_expr)?
    ;
// SuffixExpr
//     <- PrimaryTypeExpr (SuffixOp / FnCallArguments)*
suffix_expr
    : primary_type_expr (suffix_op | fn_call_arguments)*
    ;
// PrimaryTypeExpr
//     <- BUILTINIDENTIFIER FnCallArguments
//      / CHAR_LITERAL
//      / ContainerDecl
//      / DOT IDENTIFIER
//      / DOT InitList
//      / ErrorSetDecl
//      / FLOAT
//      / FnProto
//      / GroupedExpr
//      / LabeledTypeExpr
//      / IDENTIFIER
//      / IfTypeExpr
//      / INTEGER
//      / KEYWORD_comptime TypeExpr
//      / KEYWORD_error DOT IDENTIFIER
//      / KEYWORD_anyframe
//      / KEYWORD_unreachable
//      / STRINGLITERAL
primary_type_expr
    : BUILTINIDENTIFIER fn_call_arguments
    | CHAR_LITERAL
    | container_decl
    | '.' IDENTIFIER
    | '.' init_list
    | error_set_decl
    | FLOAT
    | fn_proto
    | grouped_expr
    | labeled_type_expr
    | IDENTIFIER
    | if_type_expr
    | INTEGER
    | COMPTIME type_expr
    | ERROR '.' IDENTIFIER
    | ANYFRAME
    | UNREACHABLE
    | STRINGLITERAL
    ;
// ContainerDecl <- (KEYWORD_extern / KEYWORD_packed)? ContainerDeclAuto
container_decl
    : (EXTERN | PACKED)? container_decl_auto
    ;
// ErrorSetDecl <- KEYWORD_error LBRACE IdentifierList RBRACE
error_set_decl
    : ERROR '{' identifier_list '}'
    ;
// GroupedExpr <- LPAREN Expr RPAREN
grouped_expr
    : '(' expr ')'
    ;
// IfTypeExpr <- IfPrefix TypeExpr (KEYWORD_else Payload? TypeExpr)?
if_type_expr
    : if_prefix type_expr (ELSE payload? type_expr)?
    ;
// LabeledTypeExpr
//     <- BlockLabel Block
//      / BlockLabel? LoopTypeExpr
//      / BlockLabel? SwitchExpr
labeled_type_expr
    : block_label block
    | block_label? loop_type_expr
    | block_label? switch_expr
    ;
// LoopTypeExpr <- KEYWORD_inline? (ForTypeExpr / WhileTypeExpr)
loop_type_expr
    : for_prefix type_expr (ELSE type_expr)?
    ;
// ForTypeExpr <- ForPrefix TypeExpr (KEYWORD_else TypeExpr)?
for_type_expr
    : for_prefix type_expr (ELSE type_expr)?
    ;
// WhileTypeExpr <- WhilePrefix TypeExpr (KEYWORD_else Payload? TypeExpr)?
while_type_expr
    : while_prefix type_expr (ELSE payload? type_expr)?
    ;
// SwitchExpr <- KEYWORD_switch LPAREN Expr RPAREN LBRACE SwitchProngList RBRACE
switch_expr
    : SWITCH '(' expr ')' '{' switch_prong_list '}'
    ;
//# *** Assembly ***
//AsmExpr <- KEYWORD_asm KEYWORD_volatile? LPAREN Expr AsmOutput? RPAREN
asm_expr
    : ASM VOLATILE? '(' expr asm_output? ')'
    ;
//AsmOutput <- COLON AsmOutputList AsmInput?
asm_output
    : ':' asm_output_list asm_input?
    ;
//AsmOutputItem <- LBRACKET IDENTIFIER RBRACKET STRINGLITERAL LPAREN (MINUSRARROW TypeExpr / IDENTIFIER) RPAREN
asm_output_item
    : '[' IDENTIFIER ']' STRINGLITERAL '(' ('>' type_expr | IDENTIFIER) ')'
    ;
//AsmInput <- COLON AsmInputList AsmClobbers?
asm_input
    : ':' asm_input_list asm_clobbers?
    ;
//AsmInputItem <- LBRACKET IDENTIFIER RBRACKET STRINGLITERAL LPAREN Expr RPAREN
asm_input_item
    : '[' IDENTIFIER ']' STRINGLITERAL '(' expr ')'
    ;
//AsmClobbers <- COLON Expr
asm_clobbers
    : ':' expr
    ;
//# *** Helper grammar ***
//BreakLabel <- COLON IDENTIFIER
break_label
    : ':' IDENTIFIER
    ;
//BlockLabel <- IDENTIFIER COLON
block_label
    : IDENTIFIER ':'
    ;
//FieldInit <- DOT IDENTIFIER EQUAL Expr
field_init
    : '.' IDENTIFIER '=' expr
    ;
//WhileContinueExpr <- COLON LPAREN AssignExpr RPAREN
while_continue_expr
    : ':' '(' assign_expr ')'
    ;
//LinkSection <- KEYWORD_linksection LPAREN Expr RPAREN
link_section
    : LINKSECTION '(' expr ')'
    ;
//AddrSpace <- KEYWORD_addrspace LPAREN Expr RPAREN
addr_space
    : ADDRSPACE '(' expr ')'
    ;
//# Fn specific
//CallConv <- KEYWORD_callconv LPAREN Expr RPAREN
call_conv
    : CALLCONV '(' expr ')'
    ;
//ParamDecl
//    <- doc_comment? (KEYWORD_noalias / KEYWORD_comptime)? (IDENTIFIER COLON)? ParamType
//     / DOT3
param_decl
    : Doc_comment? (NOALIAS | COMPTIME)? (IDENTIFIER ':')? param_type
    | '...'
    ;
//ParamType
//    <- KEYWORD_anytype
//     / TypeExpr
param_type
    : ANYTYPE
    | type_expr
    ;
//# Control flow prefixes
//IfPrefix <- KEYWORD_if LPAREN Expr RPAREN PtrPayload?
if_prefix
    : IF '(' expr ')' ptr_payload?
    ;
//WhilePrefix <- KEYWORD_while LPAREN Expr RPAREN PtrPayload? WhileContinueExpr?
while_prefix
    : WHILE '(' expr ')' ptr_payload? while_continue_expr?
    ;
//ForPrefix <- KEYWORD_for LPAREN ForArgumentsList RPAREN PtrListPayload
for_prefix
    : FOR '(' for_arguments_list ')' ptr_list_payload
    ;
//# Payloads
//Payload <- PIPE IDENTIFIER PIPE
payload
    : '|' IDENTIFIER '|'
    ;
//PtrPayload <- PIPE ASTERISK? IDENTIFIER PIPE
ptr_payload
    : '|' '*'? IDENTIFIER '|'
    ;
//PtrIndexPayload <- PIPE ASTERISK? IDENTIFIER (COMMA IDENTIFIER)? PIPE
ptr_index_payload
    : '|' '*'? IDENTIFIER (',' IDENTIFIER)? '|'
    ;
//PtrListPayload <- PIPE ASTERISK? IDENTIFIER (COMMA ASTERISK? IDENTIFIER)* COMMA? PIPE
ptr_list_payload
    : '|' '*'? IDENTIFIER (',' '*'? IDENTIFIER)* ','? '|'
    ;
//# Switch specific
//SwitchProng <- KEYWORD_inline? SwitchCase EQUALRARROW PtrIndexPayload? SingleAssignExpr
switch_prong
    : INLINE? switch_case '=>' ptr_index_payload? simple_assign_expr
    ;
//SwitchCase
//    <- SwitchItem (COMMA SwitchItem)* COMMA?
//     / KEYWORD_else
switch_case
    : switch_item (',' switch_item)* ','?
    | ELSE
    ;
//SwitchItem <- Expr (DOT3 Expr)?
switch_item
    : expr ('...' expr)?
    ;
//# For specific
//ForArgumentsList <- ForItem (COMMA ForItem)* COMMA?
for_arguments_list
    : for_item (',' for_item)* ','?
    ;
//ForItem <- Expr (DOT2 Expr?)?
for_item
    : expr ('..' expr?)?
    ;
//# Operators
//AssignOp
//    <- ASTERISKEQUAL
//     / ASTERISKPIPEEQUAL
//     / SLASHEQUAL
//     / PERCENTEQUAL
//     / PLUSEQUAL
//     / PLUSPIPEEQUAL
//     / MINUSEQUAL
//     / MINUSPIPEEQUAL
//     / LARROW2EQUAL
//     / LARROW2PIPEEQUAL
//     / RARROW2EQUAL
//     / AMPERSANDEQUAL
//     / CARETEQUAL
//     / PIPEEQUAL
//     / ASTERISKPERCENTEQUAL
//     / PLUSPERCENTEQUAL
//     / MINUSPERCENTEQUAL
//     / EQUAL
assign_op
    : '*='
    | '*|='
    | '/='
    | '%='
    | '+='
    | '+|='
    | '-='
    | '-|='
    | '<<='
    | '<<|='
    | '>>='
    | '&='
    | '^='
    | '|='
    | '*%='
    | '+%='
    | '-%='
    | '='
    ;
//CompareOp
//    <- EQUALEQUAL
//     / EXCLAMATIONMARKEQUAL
//     / LARROW
//     / RARROW
//     / LARROWEQUAL
//     / RARROWEQUAL
compare_op
    : '=='
    | '!='
    | '<'
    | '>'
    | '<='
    | '>='
    ;
//BitwiseOp
//    <- AMPERSAND
//     / CARET
//     / PIPE
//     / KEYWORD_orelse
//     / KEYWORD_catch Payload?
bitwise_op
    : '&'
    | '^'
    | '|'
    | ORELSE
    | CATCH payload?
    ;
//BitShiftOp
//    <- LARROW2
//     / RARROW2
//     / LARROW2PIPE
bit_shift_op
    : '<<'
    | '>>'
    | '<<|'
    ;
//AdditionOp
//    <- PLUS
//     / MINUS
//     / PLUS2
//     / PLUSPERCENT
//     / MINUSPERCENT
//     / PLUSPIPE
//     / MINUSPIPE
addition_op
    : '+'
    | '-'
    | '++'
    | '+%'
    | '-%'
    | '+|'
    | '-|'
    ;
//MultiplyOp
//    <- PIPE2
//     / ASTERISK
//     / SLASH
//     / PERCENT
//     / ASTERISK2
//     / ASTERISKPERCENT
//     / ASTERISKPIPE
multiply_op
    : '||'
    | '*'
    | '/'
    | '%'
    | '**'
    | '*%'
    | '*|'
    ;
//PrefixOp
//    <- EXCLAMATIONMARK
//     / MINUS
//     / TILDE
//     / MINUSPERCENT
//     / AMPERSAND
//     / KEYWORD_try
prefix_op
    : '!'
    | '-'
    | '~'
    | '-%'
    | '&'
    | TRY
    ;
//PrefixTypeOp
//    <- QUESTIONMARK
//     / KEYWORD_anyframe MINUSRARROW
//     / SliceTypeStart (ByteAlign / AddrSpace / KEYWORD_const / KEYWORD_volatile / KEYWORD_allowzero)*
//     / PtrTypeStart (AddrSpace / KEYWORD_align LPAREN Expr (COLON Expr COLON Expr)? RPAREN / KEYWORD_const / KEYWORD_volatile / KEYWORD_allowzero)*
//     / ArrayTypeStart
prefix_type_op
    : '?'
    | ANYFRAME '->'
    | slice_type_start
    | ptr_type_start
    | array_type_start
    ;
//SuffixOp
//    <- LBRACKET Expr (DOT2 (Expr? (COLON Expr)?)?)? RBRACKET
//     / DOT IDENTIFIER
//     / DOTASTERISK
//     / DOTQUESTIONMARK
suffix_op
    : '[' expr ('..' (expr? (':' expr)?)?)? ']'
    | DOT IDENTIFIER
    | '.*'
    | '.?'
    ;
//FnCallArguments <- LPAREN ExprList RPAREN
fn_call_arguments
    : '(' expr_list ')'
    ;

//# Ptr specific
//SliceTypeStart <- LBRACKET (COLON Expr)? RBRACKET
slice_type_start
    : '[' (':' expr)? ']'
    ;
//PtrTypeStart
//    <- ASTERISK
//     / ASTERISK2
//     / LBRACKET ASTERISK (LETTERC / COLON Expr)? RBRACKET
ptr_type_start
    : '*'
    | '**'
    | '[' '*' (LETTERC | ':' expr)? ']'
    ;
//ArrayTypeStart <- LBRACKET Expr (COLON Expr)? RBRACKET
array_type_start
    : '[' expr (':' expr)? ']'
    ;
//# ContainerDecl specific
//ContainerDeclAuto <- ContainerDeclType LBRACE ContainerMembers RBRACE
container_decl_auto
    : container_decl_type '{' container_members '}'
    ;
//ContainerDeclType
//    <- KEYWORD_struct (LPAREN Expr RPAREN)?
//     / KEYWORD_opaque
//     / KEYWORD_enum (LPAREN Expr RPAREN)?
//     / KEYWORD_union (LPAREN (KEYWORD_enum (LPAREN Expr RPAREN)? / Expr) RPAREN)?
container_decl_type
    : STRUCT ('(' expr ')')?
    | OPAQUE
    | ENUM ('(' expr ')')?
    | UNION ('(' (ENUM ('(' expr ')')? | expr) ')')?
    ;
//# Alignment
//ByteAlign <- KEYWORD_align LPAREN Expr RPAREN
byte_align
    : ALIGN '(' expr ')'
    ;
//# Lists
//IdentifierList <- (doc_comment? IDENTIFIER COMMA)* (doc_comment? IDENTIFIER)?
identifier_list
    : (Doc_comment? IDENTIFIER ',')* (Doc_comment? IDENTIFIER)?
    ;
//SwitchProngList <- (SwitchProng COMMA)* SwitchProng?
switch_prong_list
    : (switch_prong ',')* switch_prong?
    ;
//AsmOutputList <- (AsmOutputItem COMMA)* AsmOutputItem?
asm_output_list
    : (asm_output_item ',')* asm_output_item?
    ;
//AsmInputList <- (AsmInputItem COMMA)* AsmInputItem?
asm_input_list
    : (asm_input_item ',')* asm_input_item?
    ;
//ParamDeclList <- (ParamDecl COMMA)* ParamDecl?
param_decl_list
    : (param_decl ',')* param_decl?
    ;
//ExprList <- (Expr COMMA)* Expr?
expr_list
    : (expr ',')* expr?
    ;
