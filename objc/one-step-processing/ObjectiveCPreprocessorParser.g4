/*
Objective-C Preprocessor grammar.
The MIT License (MIT).
Copyright (c) 2016-2017, Alex Petuschak (alex@swiftify.io).
Copyright (c) 2016-2017, Ivan Kochurkin (kvanttt@gmail.com).

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

options { tokenVocab=ObjectiveCLexer; }

directive
    : SHARP (DIRECTIVE_IMPORT | DIRECTIVE_INCLUDE) directiveText          #preprocessorImport
    | SHARP DIRECTIVE_IF preprocessorExpression                           #preprocessorConditional
    | SHARP DIRECTIVE_ELIF preprocessorExpression                         #preprocessorConditional
    | SHARP DIRECTIVE_ELSE                                                #preprocessorConditional
    | SHARP DIRECTIVE_ENDIF                                               #preprocessorConditional
    | SHARP DIRECTIVE_IFDEF DIRECTIVE_ID                                  #preprocessorDef
    | SHARP DIRECTIVE_IFNDEF DIRECTIVE_ID                                 #preprocessorDef
    | SHARP DIRECTIVE_UNDEF DIRECTIVE_ID                                  #preprocessorDef
    | SHARP DIRECTIVE_PRAGMA directiveText                                #preprocessorPragma
    | SHARP DIRECTIVE_ERROR directiveText                                 #preprocessorError
    | SHARP DIRECTIVE_WARNING directiveText                               #preprocessorWarning
    | SHARP DIRECTIVE_DEFINE DIRECTIVE_ID directiveText?                  #preprocessorDefine
    ;

directiveText
    : (DIRECTIVE_TEXT | DIRECTIVE_TEXT_NEWLINE)+
    ;

preprocessorExpression
    : DIRECTIVE_TRUE                                                      #preprocessorConstant
    | DIRECTIVE_FALSE                                                     #preprocessorConstant
    | DIRECTIVE_DECIMAL_LITERAL                                           #preprocessorConstant
    | DIRECTIVE_STRING                                                    #preprocessorConstant
    | DIRECTIVE_ID                                                
      (DIRECTIVE_LP preprocessorExpression DIRECTIVE_RP)?                 #preprocessorConditionalSymbol
    | DIRECTIVE_LP preprocessorExpression DIRECTIVE_RP                    #preprocessorParenthesis
    | DIRECTIVE_BANG preprocessorExpression                               #preprocessorNot
    | preprocessorExpression                                              
      op=(DIRECTIVE_EQUAL | DIRECTIVE_NOTEQUAL) preprocessorExpression    #preprocessorBinary
    | preprocessorExpression op=DIRECTIVE_AND preprocessorExpression      #preprocessorBinary
    | preprocessorExpression op=DIRECTIVE_OR preprocessorExpression       #preprocessorBinary
    | preprocessorExpression 
      op=(DIRECTIVE_LT | DIRECTIVE_GT | DIRECTIVE_LE | DIRECTIVE_GE)
      preprocessorExpression                                              #preprocessorBinary
    | DIRECTIVE_DEFINED
      (DIRECTIVE_ID | DIRECTIVE_LP DIRECTIVE_ID DIRECTIVE_RP)             #preprocessorDefined
    ;