/******************************
* Parser for PlantUML class diagrams
* 
* Copyright (c) 2003--2025 Kevin Lano
*
* This program and the accompanying materials are made available under the
* terms of the Eclipse Public License 2.0 which is available at
* http://www.eclipse.org/legal/epl-2.0
*
* SPDX-License-Identifier: EPL-2.0
* *****************************/


parser grammar PlantUML;

options { tokenVocab=PlantUMLLexer; }  // tokens from lexer file

uml:
    (NEWLINE | COMMENT)* STARTUML (IDENT)? (NEWLINE | class_dclr | enum_dclr | association_dclr | associative_class_dclr | COMMENT)* ENDUML (NEWLINE | COMMENT)*
    ;

class_dclr:
    class_type? CLASS ident stereotype? extension_dclr? (NEWLINE? class_body)?
    ;

class_body:
    CLASS_BODY_START body_content* BODY_CLOSE
    ;

body_content:
    BODY_INLINE_BRACES
    | BODY_OPEN
    | BODY_CONTENT
    | BODY_NL
    | nested_body
    ;

nested_body:
    BODY_OPEN body_content* BODY_CLOSE
    ;

class_type:
    (ABSTRACT|INTERFACE)
    ;

stereotype: STEREOTYPE_TEXT;

extension_dclr:
    EXTENDS ident
    ;

ident:
    IDENT
    ;

enum_dclr:
    ENUM ident stereotype? class_body?
    ;

association_dclr:
    left=association_left
    relation
    right=association_right
    association_name?
    ;

relation:
    (GT | LT)? DASH (DASH)* (GT | LT)?
    | (LT)? (DASH)+ STAR
    | STAR (DASH)+ (GT)?
    | (LT)? (DASH)+ O
    | O (DASH)+ (GT)?
    | (LT_PIPE | CARET) (DASH)+ (GT)?
    | (LT)? (DASH)+ (CARET | PIPE_GT)
    ;

association_left:
    ident association_detail?
    ;

association_right:
    association_detail? ident
    ;

association_detail:
    ASSOC_DETAIL
    ;

association_name:
    AFTER_COLON_TEXT
    ;

associative_class_dclr:
    LPAREN left=ident COMMA right=ident RPAREN (DOT | DOUBLE_DOT) target=ident association_name?
    | target=ident (DOT | DOUBLE_DOT) LPAREN left=ident COMMA right=ident RPAREN association_name?
    ;
