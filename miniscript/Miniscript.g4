/*
MIT License

Copyright (c) 2026 Nuhiat Arefin

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

// Derived from BIP-379: Miniscript.
//
// This grammar recognizes the surface syntax of Miniscript fragments and
// wrappers. It keeps minimal syntactic bounds such as positive numeric fields
// and the legacy 20-key limit for multi(). Key expressions are accepted as
// opaque tokens: concrete BIP-380 key forms parse, but so do the symbolic key
// names (pk(A), multi(2,key_1,key_2)) used throughout BIP-379 itself and by
// policy compilers. Validating that a key is a well-formed BIP-380 key
// expression appropriate for its context, like the Miniscript type system,
// k <= n checks, the multi_a() key-count limit, and timelock mixing rules, is
// intentionally left to downstream analyzers.

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar Miniscript;

miniscript
    : expression EOF
    ;

expression
    : wrappedExpression
    | baseExpression
    ;

baseExpression
    : constant
    | keyExpression
    | timeExpression
    | hashExpression
    | multisigExpression
    | binaryExpression
    | ternaryExpression
    | thresholdExpression
    ;

constant
    : ZERO
    | ONE
    ;

keyExpression
    : PK LPAREN key RPAREN
    | PKH LPAREN key RPAREN
    | PK_K LPAREN key RPAREN
    | PK_H LPAREN key RPAREN
    ;

timeExpression
    : OLDER LPAREN positiveInteger RPAREN
    | AFTER LPAREN positiveInteger RPAREN
    ;

hashExpression
    : SHA256 LPAREN hash32 RPAREN
    | HASH256 LPAREN hash32 RPAREN
    | HASH160 LPAREN hash20 RPAREN
    | RIPEMD160 LPAREN hash20 RPAREN
    ;

multisigExpression
    : MULTI LPAREN positiveInteger COMMA keyList1To20 RPAREN
    | MULTI_A LPAREN positiveInteger COMMA keyList RPAREN
    ;

binaryExpression
    : AND_V LPAREN expression COMMA expression RPAREN
    | AND_B LPAREN expression COMMA expression RPAREN
    | AND_N LPAREN expression COMMA expression RPAREN
    | OR_B LPAREN expression COMMA expression RPAREN
    | OR_C LPAREN expression COMMA expression RPAREN
    | OR_D LPAREN expression COMMA expression RPAREN
    | OR_I LPAREN expression COMMA expression RPAREN
    ;

ternaryExpression
    : ANDOR LPAREN expression COMMA expression COMMA expression RPAREN
    ;

thresholdExpression
    : THRESH LPAREN positiveInteger COMMA expression (COMMA expression)* RPAREN
    ;

wrappedExpression
    : WRAPPER_SEQUENCE baseExpression
    ;

// Hash arguments are fixed-width hex strings: 64 hex characters for 32-byte
// hashes, 40 for 20-byte hashes. An argument consisting only of digits lexes
// as DIGITS_64/DIGITS_40 instead of the HEX tokens (those digit tokens double
// as integers in other rules), so each hash rule accepts both token kinds.
// Both denote the same hex value; neither is a base-10 encoding.
hash32
    : HEX32_BYTES
    | DIGITS_64
    ;

hash20
    : HEX20_BYTES
    | DIGITS_40
    ;

keyList
    : key (COMMA key)*
    ;

// At most 20 keys (the legacy CHECKMULTISIG limit). The optionals are nested
// rather than sequential so that every key count from 1 to 20 has exactly one
// parse; a flat run of optionals would leave ANTLR free to choose which
// optional consumes each comma, making the rule ambiguous.
keyList1To20
    : key (
        COMMA key (
            COMMA key (
                COMMA key (
                    COMMA key (
                        COMMA key (
                            COMMA key (
                                COMMA key (
                                    COMMA key (
                                        COMMA key (
                                            COMMA key (
                                                COMMA key (
                                                    COMMA key (
                                                        COMMA key (
                                                            COMMA key (
                                                                COMMA key (
                                                                    COMMA key (
                                                                        COMMA key (
                                                                            COMMA key (COMMA key)?
                                                                        )?
                                                                    )?
                                                                )?
                                                            )?
                                                        )?
                                                    )?
                                                )?
                                            )?
                                        )?
                                    )?
                                )?
                            )?
                        )?
                    )?
                )?
            )?
        )?
    )?
    ;

key
    : COMPRESSED_PUBLIC_KEY
    | HEX32_BYTES
    | HEX20_BYTES
    | DIGITS_64
    | DIGITS_40
    | KEY
    | ZERO
    | ONE
    | POSITIVE_INTEGER
    | reservedKeyWord
    ;

reservedKeyWord
    : PK
    | PKH
    | PK_K
    | PK_H
    | OLDER
    | AFTER
    | SHA256
    | HASH256
    | HASH160
    | RIPEMD160
    | MULTI
    | MULTI_A
    | AND_V
    | AND_B
    | AND_N
    | OR_B
    | OR_C
    | OR_D
    | OR_I
    | ANDOR
    | THRESH
    ;

// DIGITS_64 and DIGITS_40 are all-digit strings that would otherwise be hex
// hashes; here they are read as ordinary (very large) decimal integers.
positiveInteger
    : ONE
    | DIGITS_64
    | DIGITS_40
    | POSITIVE_INTEGER
    ;

PK
    : 'pk'
    ;

PKH
    : 'pkh'
    ;

PK_K
    : 'pk_k'
    ;

PK_H
    : 'pk_h'
    ;

OLDER
    : 'older'
    ;

AFTER
    : 'after'
    ;

SHA256
    : 'sha256'
    ;

HASH256
    : 'hash256'
    ;

HASH160
    : 'hash160'
    ;

RIPEMD160
    : 'ripemd160'
    ;

MULTI
    : 'multi'
    ;

MULTI_A
    : 'multi_a'
    ;

AND_V
    : 'and_v'
    ;

AND_B
    : 'and_b'
    ;

AND_N
    : 'and_n'
    ;

OR_B
    : 'or_b'
    ;

OR_C
    : 'or_c'
    ;

OR_D
    : 'or_d'
    ;

OR_I
    : 'or_i'
    ;

ANDOR
    : 'andor'
    ;

THRESH
    : 'thresh'
    ;

LPAREN
    : '('
    ;

RPAREN
    : ')'
    ;

COMMA
    : ','
    ;

WRAPPER_SEQUENCE
    : [acdtvjnlsu]+ ':'
    ;

COMPRESSED_PUBLIC_KEY
    : ('02' | '03') HEX64
    ;

// A string of exactly 64 (or 40) decimal digits with no leading zero is
// simultaneously a canonical positive integer and a well-formed 32-byte
// (20-byte) hex value. Such strings get their own tokens so that hash rules,
// integer rules, and key positions can all accept them; the parser rule that
// consumes the token decides the interpretation.
DIGITS_64
    : NON_ZERO_DIGIT DIGIT32 DIGIT16 DIGIT8 DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT
    ;

DIGITS_40
    : NON_ZERO_DIGIT DIGIT32 DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT
    ;

HEX32_BYTES
    : HEX64
    ;

HEX20_BYTES
    : HEX40
    ;

ZERO
    : '0'
    ;

ONE
    : '1'
    ;

POSITIVE_INTEGER
    : [2-9] DIGIT*
    | '1' DIGIT+
    ;

KEY
    : KEY_CHAR+
    ;

fragment HEX64
    : HEX32 HEX32
    ;

fragment HEX40
    : HEX32 HEX HEX HEX HEX HEX HEX HEX HEX
    ;

fragment HEX32
    : HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX
    ;

fragment HEX
    : [0-9a-fA-F]
    ;

fragment DIGIT32
    : DIGIT16 DIGIT16
    ;

fragment DIGIT16
    : DIGIT8 DIGIT8
    ;

fragment DIGIT8
    : DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT
    ;

fragment DIGIT
    : [0-9]
    ;

fragment NON_ZERO_DIGIT
    : [1-9]
    ;

fragment KEY_CHAR
    : [A-Za-z0-9_./*'[\]<>;-]
    ;

WS
    : [ \t\r\n]+ -> skip
    ;
