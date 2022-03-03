/*
     [The "BSD licence"]
     Copyright (c) 2013 Sam Harwell
     All rights reserved.
     Redistribution and use in source and binary forms, with or without
     modification, are permitted provided that the following conditions
     are met:
     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
     3. The name of the author may not be used to endorse or promote products
        derived from this software without specific prior written permission.
     THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
     IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
     OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
     IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
     INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
     NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
     DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
     THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
     (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
     THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

// Grammar for KLEE KQuery parsing.
// Ported to Antlr4 by Sumit Lahiri.
grammar KQuery;

kqueryExpression 
    : ktranslationUnit* EOF
    ;

ktranslationUnit
    : arrayDeclaration
    | queryCommand
    ;

queryCommand 
    : LeftParen Query evalExprList queryExpr RightParen 
    ;
        
queryExpr 
    : expression 
    | expression evalExprList
    | expression evalExprList evalArrayList
    ;
    
evalExprList 
    : LeftBracket expression* RightBracket  
    ;
    
evalArrayList 
    : LeftBracket Identifier* RightBracket  
    ;

arrayDeclaration
    : Array arrName LeftBracket arrayElemsStub RightBracket 
        Colon domain Arrow rangeLimit Equal arrayInitializer
    ;
    
arrayElemsStub
    : Constant
    ;
    
arrayInitializer
    : Symbolic 
    | LeftBracket numberList RightBracket
    ;
    
expression
    : Identifier
    | Identifier Colon expression   
    | LeftParen WidthType number RightParen
    | LeftParen arithmeticExpr WidthType expression expression RightParen
    | LeftParen NOT LeftBracket WidthType RightBracket expression RightParen
    | LeftParen bitwiseExpr WidthType expression expression RightParen 
    | LeftParen SHL WidthType expression expression RightParen
    | LeftParen LSHR WidthType expression expression RightParen
    | LeftParen ASHR WidthType expression expression RightParen
    | LeftParen comparisonExpr WidthType expression expression RightParen 
    | LeftParen comparisonExpr expression expression RightParen 
    | LeftParen CONCAT WidthType expression expression RightParen
    | LeftParen CONCAT expression expression RightParen
    | LeftParen EXTRACT WidthType number expression RightParen
    | LeftParen ZEXT WidthType expression RightParen
    | LeftParen SEXT WidthType expression RightParen
    | LeftParen READ WidthType expression version RightParen
    | LeftParen READ WidthType expression RightParen
    | LeftParen SELECT WidthType expression expression expression RightParen  
    | LeftParen NEGETION WidthType expression RightParen
    | LeftParen NEGETION expression RightParen
    | LeftParen READLSB WidthType expression version RightParen  
    | LeftParen READMSB WidthType expression version RightParen  
    | LeftParen READLSB WidthType expression RightParen  
    | LeftParen READMSB WidthType expression RightParen 
    | version
    | number
    ;
    
version
    : Identifier 
    | Identifier Colon expression
    | LeftBracket updateList RightBracket ATR version
    | LeftBracket RightBracket ATR version
    ;
    
updateList 
    : expression Equal expression COMMA updateList
    | expression Equal expression
    ;

bitwiseExpr 
    : BITWISEAND 
    | BITWISEOR 
    | BITWISEXOR 
    | SHL 
    | LSHR 
    | ASHR
    ;
    
comparisonExpr 
    : EQ 
    | NEQ 
    | ULT
    | UGT
    | ULE 
    | UGE 
    | SLT 
    | SLE 
    | SGT 
    | SGE
    ;
    
arithmeticExpr 
    : ADD
    | SUB 
    | MUL 
    | UDIV 
    | UREM 
    | SDIV 
    | SREM
    ;
    
domain : WidthType ;
rangeLimit : WidthType ;
arrName : Identifier ;

numberList
    : number
    | number numberList
    ;

number 
    : TrueMatch 
    | FalseMatch 
    | SignedConstant
    | Constant
    ;
    
SignedConstant 
    : (PLUS | MINUS)Constant
    ;
    
Constant
    : DIGIT+ 
    | BinConstant 
    | OctConstant 
    | HexConstant
    ;
    
BinConstant 
    : BinId BIN_DIGIT+  
    ;
    
OctConstant 
    : OctId OCTAL_DIGIT+  
    ;
    
HexConstant 
    : HexId HEX_DIGIT+
    ;

FloatingPointType 
    : FP DIGIT ((.).*?)?  
    ;
    
IntegerType 
    : INT DIGIT+
    ;

WidthType 
    : WIDTH DIGIT+
    ;
    
BinId : '0b';
OctId : '0o';
WIDTH : 'w';
HexId : '0x';
TrueMatch : 'true';
FalseMatch : 'false';
Query : 'query';
Array : 'array';
Symbolic : 'symbolic';
Colon : ':';
Arrow : '->';
Equal : '=';
COMMA : ',';
NOT : 'Not';
SHL : 'Shl';
LSHR : 'LShr';
ASHR : 'AShr';
CONCAT : 'Concat';
EXTRACT: 'Extract';
ZEXT: 'ZExt';
SEXT: 'SExt';
READ: 'Read';
SELECT: 'Select';
NEGETION: 'Neg';
READLSB: 'ReadLSB';
READMSB: 'ReadMSB';
PLUS : '+';
MINUS : '-';
INT : 'i';
ATR : '@';
FP : 'fp';
BITWISEAND : 'And';
BITWISEOR : 'Or';
BITWISEXOR : 'Xor';
EQ : 'Eq';
NEQ : 'Ne' ; 
ULT : 'Ult' ; 
ULE : 'Ule' ;
UGT : 'Ugt' ;
UGE : 'Uge' ;
SLT : 'Slt' ;
SLE : 'Sle' ; 
SGT : 'Sgt' ;
SGE : 'Sge' ;
ADD : 'Add' ;
SUB : 'Sub' ;
MUL : 'Mul' ;
UDIV : 'UDiv'; 
UREM : 'URem'; 
SDIV : 'SDiv'; 
SREM : 'SRem';

fragment
DIGIT 
    : ('0'..'9')  
    ;

BIN_DIGIT
    : ('0' | '1' | '_')
    ;
    
OCTAL_DIGIT
    : ('0'..'7' | '_')
    ;

HEX_DIGIT 
    : ('0'..'9'|'a'..'f'|'A'..'F'|'_') 
    ;
    
Identifier
    :  ('a'..'z' | 'A'..'Z' | '_')('a'..'z' | 'A'..'Z' | '_' | '0'..'9' | '.' )*
    ;
    
Whitespace
    :   [ \t]+ -> skip
    ;

Newline
    :   (   '\r' '\n'?
        |   '\n'
        )
        -> skip
    ;

BlockComment
    : '/*' .*? '*/' -> skip
    ;

LineComment
    : '#' ~[\r\n]* -> skip
    ;
    
LeftParen : '(';
RightParen : ')';
LeftBracket : '[';
RightBracket : ']';
LeftBrace : '{';
RightBrace : '}';