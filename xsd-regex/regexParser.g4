/*
 * [The "BSD license"]
 *  Copyright (c) 2019 PANTHEON.tech
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. The name of the author may not be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 *  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 *  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 *  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 *  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 *  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * Parser grammar for https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#regexs.
 *
 * This grammar is modified in following ways:
 * - charGroup definition inlines the charClassSub case
 *   This allows us to simplify processing, eliminating one level of nesting. It
 *   also makes this rule consistent with XSD 1.1 definition.
 */
parser grammar regexParser;
options { tokenVocab = regexLexer; }

// Parser root context, ensures all input is matched
root: regExp EOF
    ;

// Regular Expression
regExp : branch (PIPE branch)*
    ;

// Branch
branch : piece*
    ;

// Piece
piece : atom quantifier?
    ;

// Quantifier
quantifier : QUESTION | STAR | PLUS | StartQuantity quantity EndQuantity
    ;
quantity : quantRange | quantMin | QuantExact
    ;
quantRange : QuantExact COMMA QuantExact
    ;
quantMin : QuantExact COMMA
    ;

// Atom
atom : Char | charClass | (LPAREN regExp RPAREN)
    ;

// Character Class
charClass : charClassEsc | charClassExpr | WildcardEsc
    ;

// Character Class Expression
charClassExpr : (NegCharGroup | NestedNegCharGroup | PosCharGroup | NestedPosCharGroup) charGroup EndCharGroup
    ;

// Character Group
// In order to disambiguate the use of DASH's roles in Character Class Subtraction and in posCharGroup
// tail, we explicitly handle it here. ANTLR will consider the subrules in order and they completely
// disambiguate use [a--[f]], [a-[f]], [a-], [a]. We have borrowed some of the clarification from
// https://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/ to make this work
charGroup : posCharGroup? DASH DASH charClassExpr
    | posCharGroup DASH charClassExpr
    | posCharGroup DASH?
    | DASH
    ;

// Positive Character Group
posCharGroup : DASH? (charRange | charClassEsc)+
    ;

// Character Range, sans the DASH possibility
charRange : seRange | XmlChar
    ;
seRange : charOrEsc DASH charOrEsc
    ;
charOrEsc : XmlChar | SingleCharEsc
    ;

// Character Class Escape
charClassEsc : SingleCharEsc | NestedSingleCharEsc | MultiCharEsc | NestedMultiCharEsc | catEsc | complEsc
    ;

// Category Escape
catEsc : (CatEsc | NestedCatEsc) charProp EndCategory
    ;
complEsc : (ComplEsc | NestedComplEsc) charProp EndCategory
    ;
charProp : IsCategory | IsBlock
    ;

