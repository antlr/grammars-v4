/*
 YINI grammar in ANTLR 4.
 
 Apache License, Version 2.0, January 2004,
 http://www.apache.org/licenses/
 Copyright 2024-2025 Gothenburg, Marko K. S. (Sweden via
 Finland).
 */

/* 
 This grammar aims to follow, as closely as possible,
 the YINI format specification version:
 1.0.0-rc.2 - 2025 Aug.
 
 Feedback, bug reports and improvements are welcomed here
 https://github.com/YINI-lang/YINI-spec
 
 GitHub:   https://github.com/YINI-lang
 Homepage: http://yini-lang.org
 */

parser grammar YiniParser;
options {
	tokenVocab = YiniLexer;
	caseInsensitive = false;
}

yini:
	SHEBANG? INLINE_COMMENT* NL* 
	YINI_MARKER? INLINE_COMMENT* NL* 
	section+ NL* terminal_line? EOF;

section: SECTION_HEAD? section_members;

terminal_line: TERMINAL_TOKEN (NL+ | INLINE_COMMENT? NL*);

section_members: member+;

// -----------------------
// Key–Value Assignment
// -----------------------
member:
	KEY WS? EQ WS? value? NL+ // Empty value is treated as NULL.
	| member_colon_list
	| SECTION_HEAD section_members?
	| bad_member
	;

member_colon_list: KEY COLON WS? elements? NL+;

value:
	null_literal // NOTE: In specs NULL should be case-insensitive.
	| string_literal
	| number_literal
	| boolean_literal
	| list_in_brackets
	| object_literal;

object_literal
  : OC NL* objectMemberList NL* CC NL*
  | empty_object NL*
  ;

// A memberList is one or more key=value pairs separated by commas.
objectMemberList
    : objectMember ( COMMA NL* objectMember )* ( COMMA )?
	| empty_object NL*
    ;
    
objectMember
    : KEY WS? COLON NL* value
    ;

list: elements | list_in_brackets;

list_in_brackets: OB NL* elements NL* CB | empty_list NL*;

elements: element COMMA? | element COMMA elements;

element: NL* value NL* | NL* list_in_brackets NL*;

number_literal: NUMBER;
null_literal: NULL;

string_literal: STRING string_concat*;
string_concat: NL* PLUS NL* STRING;

// NOTE: In specs boolean literals should be case-insensitive.
boolean_literal: BOOLEAN_FALSE | BOOLEAN_TRUE;

empty_object: EMPTY_OBJECT | '{' NL* '}';
empty_list: EMPTY_LIST | '[' NL* ']';

// For catching bad member syntax.
bad_member:  WS? (REST|value)? WS? EQ (value|REST) NL?;
