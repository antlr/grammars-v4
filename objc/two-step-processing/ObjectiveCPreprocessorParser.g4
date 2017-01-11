/*
Objective-C Preprocessor grammar.
The MIT License (MIT).
Copyright (c) 2016, Ivan Kochurkin (kvanttt@gmail.com).

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

parser grammar ObjectiveCPreprocessorParser;

options { tokenVocab=ObjectiveCPreprocessorLexer; }

objectiveCDocument
    : text* EOF
    ;

text
    : code
    | SHARP directive (NEW_LINE | EOF)
    ;

code
    : CODE+
    ;

directive
    : (IMPORT | INCLUDE) directive_text     #preprocessorImport
    | IF preprocessor_expression            #preprocessorConditional
    | ELIF preprocessor_expression          #preprocessorConditional
    | ELSE                                  #preprocessorConditional
    | ENDIF                                 #preprocessorConditional
    | IFDEF CONDITIONAL_SYMBOL              #preprocessorDef
    | IFNDEF CONDITIONAL_SYMBOL             #preprocessorDef
    | UNDEF CONDITIONAL_SYMBOL              #preprocessorDef
    | PRAGMA directive_text                           #preprocessorPragma
    | ERROR directive_text                            #preprocessorError
    | DEFINE CONDITIONAL_SYMBOL directive_text?       #preprocessorDefine
    ;

directive_text
    : TEXT+
    ;

preprocessor_expression
    : TRUE                                                                   #preprocessorConstant
    | FALSE                                                                  #preprocessorConstant
    | DECIMAL_LITERAL                                                        #preprocessorConstant
    | DIRECTIVE_STRING                                                       #preprocessorConstant
    | CONDITIONAL_SYMBOL (LPAREN preprocessor_expression RPAREN)?            #preprocessorConditionalSymbol
    | LPAREN preprocessor_expression RPAREN                                  #preprocessorParenthesis
    | BANG preprocessor_expression                                           #preprocessorNot
    | preprocessor_expression op=(EQUAL | NOTEQUAL) preprocessor_expression  #preprocessorBinary
    | preprocessor_expression op=AND preprocessor_expression                 #preprocessorBinary
    | preprocessor_expression op=OR preprocessor_expression                  #preprocessorBinary
    | preprocessor_expression op=(LT | GT | LE | GE) preprocessor_expression #preprocessorBinary
    | DEFINED (CONDITIONAL_SYMBOL | LPAREN CONDITIONAL_SYMBOL RPAREN)         #preprocessorDefined
    ;