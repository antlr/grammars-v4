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

lexer grammar WatLexer;

LPAR : '(' ;
RPAR : ')' ;

NAT : Nat ;
INT : Int ;
FLOAT : Float ;
STRING_ : String_ ;
VALUE_TYPE : NXX ;
CONST : NXX '.const' ;

FUNCREF: 'funcref' ;
MUT: 'mut' ;

NOP: 'nop' ;
UNREACHABLE: 'unreachable' ;
DROP: 'drop' ;
BLOCK: 'block' ;
LOOP: 'loop' ;
END: 'end' ;
BR: 'br' ;
BR_IF: 'br_if' ;
BR_TABLE: 'br_table' ;
RETURN: 'return' ;
IF: 'if' ;
THEN: 'then' ;
ELSE: 'else' ;
SELECT: 'select' ;
CALL: 'call' ;
CALL_INDIRECT: 'call_indirect' ;

LOCAL_GET: 'local.get' ;
LOCAL_SET: 'local.set' ;
LOCAL_TEE: 'local.tee' ;
GLOBAL_GET: 'global.get' ;
GLOBAL_SET: 'global.set' ;

LOAD : NXX '.load' (MEM_SIZE '_' SIGN)? ;
STORE : NXX '.store' (MEM_SIZE)? ;

OFFSET_EQ_NAT : 'offset=' Nat ;
ALIGN_EQ_NAT : 'align=' Nat ;

UNARY
  : IXX '.clz'
  | IXX '.ctz'
  | IXX '.popcnt'
  | FXX '.neg'
  | FXX '.abs'
  | FXX '.sqrt'
  | FXX '.ceil'
  | FXX '.floor'
  | FXX '.trunc'
  | FXX '.nearest'
  ;

BINARY
  : IXX '.add'
  | IXX '.sub'
  | IXX '.mul'
  | IXX '.div_s'
  | IXX '.div_u'
  | IXX '.rem_s'
  | IXX '.rem_u'
  | IXX '.and'
  | IXX '.or'
  | IXX '.xor'
  | IXX '.shl'
  | IXX '.shr_s'
  | IXX '.shr_u'
  | IXX '.rotl'
  | IXX '.rotr'
  | FXX '.add'
  | FXX '.sub'
  | FXX '.mul'
  | FXX '.div'
  | FXX '.min'
  | FXX '.max'
  | FXX '.copysign'
  ;

TEST
  : IXX '.eqz'
  ;

COMPARE
  : IXX '.eq'
  | IXX '.ne'
  | IXX '.lt_s'
  | IXX '.lt_u'
  | IXX '.le_s'
  | IXX '.le_u'
  | IXX '.gt_s'
  | IXX '.gt_u'
  | IXX '.ge_s'
  | IXX '.ge_u'
  | FXX '.eq'
  | FXX '.ne'
  | FXX '.lt'
  | FXX '.le'
  | FXX '.gt'
  | FXX '.ge'
  ;

CONVERT
  : 'i32.wrap_i64'
  | 'i64.extend_i32_s'
  | 'i64.extend_i32_u'
  | 'f32.demote_f64'
  | 'f64.promote_f32'
  | IXX '.trunc_f32_s'
  | IXX '.trunc_f32_u'
  | IXX '.trunc_f64_s'
  | IXX '.trunc_f64_u'
  | FXX '.convert_i32_s'
  | FXX '.convert_i32_u'
  | FXX '.convert_i64_s'
  | FXX '.convert_i64_u'
  | 'f32.reinterpret_i32'
  | 'f64.reinterpret_i64'
  | 'i32.reinterpret_f32'
  | 'i64.reinterpret_f64'
  ;

MEMORY_SIZE : 'memory.size' ;
MEMORY_GROW : 'memory.grow' ;

TYPE: 'type' ;
FUNC: 'func' ;
START_: 'start' ;
PARAM: 'param' ;
RESULT: 'result' ;
LOCAL: 'local' ;
GLOBAL: 'global' ;
TABLE: 'table' ;
MEMORY: 'memory' ;
ELEM: 'elem' ;
DATA: 'data' ;
OFFSET: 'offset' ;
IMPORT: 'import' ;
EXPORT: 'export' ;

MODULE : 'module' ;
BIN : 'binary' ;
QUOTE : 'quote' ;

SCRIPT: 'script' ;
REGISTER: 'register' ;
INVOKE: 'invoke' ;
GET: 'get' ;
ASSERT_MALFORMED: 'assert_malformed' ;
ASSERT_INVALID: 'assert_invalid' ;
ASSERT_UNLINKABLE: 'assert_unlinkable' ;
ASSERT_RETURN: 'assert_return' ;
ASSERT_RETURN_CANONICAL_NAN: 'assert_return_canonical_nan' ;
ASSERT_RETURN_ARITHMETIC_NAN: 'assert_return_arithmetic_nan' ;
ASSERT_TRAP: 'assert_trap' ;
ASSERT_EXHAUSTION: 'assert_exhaustion' ;
INPUT: 'input' ;
OUTPUT: 'output' ;

VAR : Name ;

SPACE
  : [ \t\r\n] -> skip
  ;

COMMENT
  : ( '(;' .*? ';)'
  | ';;' .*? '\n')-> skip
  ;

fragment Symbol
  : '.' | '+' | '-' | '*' | '/' | '\\' | '^' | '~' | '=' | '<' | '>' | '!' | '?' | '@' | '#' | '$' | '%' | '&' | '|' | ':' | '\'' | '`'
  ;

fragment Num
  : Digit ('_'? Digit)*
  ;

fragment HexNum
  : HexDigit ('_'? HexDigit)*
  ;

fragment Sign
  : '+' | '-'
  ;

fragment Digit
  : [0-9]
  ;

fragment HexDigit
  : [0-9a-fA-F]
  ;

fragment Letter
  : [a-zA-Z]
  ;

fragment Nat : Num | ('0x' HexNum) ;
fragment Int : Sign Nat ;
fragment Frac : Num ;
fragment HexFrac : HexNum ;

fragment Float
  : Sign? Num '.' Frac?
  | Sign? Num ('.' Frac?)? ('e' | 'E') Sign? Num
  | Sign? '0x' HexNum '.' HexFrac?
  | Sign? '0x' HexNum ('.' HexFrac?)? ('p' | 'P') Sign? Num
  | Sign? 'inf'
  | Sign? 'nan'
  | Sign? 'nan:' '0x' HexNum
  ;

fragment String_
  : '"' ( Char | '\n' | '\t' | '\\' | '\'' | '\\' HexDigit HexDigit | '\\u{' HexDigit+ '}' )* '"'
  ;

fragment Name
  : '$' (Letter | Digit | '_' | Symbol)+
  ;

fragment Escape : [nrt'"\\] ;

fragment IXX : 'i' ('32' | '64') ;
fragment FXX : 'f' ('32' | '64') ;
fragment NXX : IXX | FXX ;
fragment MIXX : 'i' ('8' | '16' | '32' | '64') ;
fragment MFXX : 'f' ('32' | '64') ;
fragment SIGN : 's' | 'u' ;
fragment MEM_SIZE : '8' | '16' | '32' ;

fragment Char : ~["'\\\u0000-\u001f\u007f-\u00ff] ;
fragment Ascii : [\u0000-\u007f] ;
fragment Ascii_no_nl : [\u0000-\u0009\u000b-\u007f] ;
fragment Utf8Cont : [\u0080-\u00bf] ;
fragment Utf8 : Ascii | Utf8Enc ;
fragment Utf8_no_nl : Ascii_no_nl | Utf8Enc ;

fragment Utf8Enc
  : [\u00c2-\u00df] Utf8Cont
  | [\u00e0] [\u00a0-\u00bf] Utf8Cont
  | [\u00ed] [\u0080-\u009f] Utf8Cont
  | [\u00e1-\u00ec\u00ee-\u00ef] Utf8Cont Utf8Cont
  | [\u00f0] [\u0090-\u00bf] Utf8Cont Utf8Cont
  | [\u00f4] [\u0080-\u008f] Utf8Cont Utf8Cont
  | [\u00f1-\u00f3] Utf8Cont Utf8Cont Utf8Cont
  ;
