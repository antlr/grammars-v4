/*
 This file is the grammar for the nesC of the TinyOS.

 This grammar is free software: you can redistribute it and/or modify it under the terms of the GNU
 Lesser General Public License as published by the Free Software Foundation, either version 3 of the
 License, or (at your option) any later version. This program is distributed in the hope that it
 will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details. You
 should have received a copy of the GNU Lesser General Public License along with this program. If
 not, see <http://www.gnu.org/licenses/>.
 */
/*
 Antlr4 TinyOS(nesC) by Hussein Marah, 2020.
 */

parser grammar TinyosParser;

options {
	tokenVocab = TinyosLexer;
	//superClass = MultiChannelBaseParser;
}

compilationUnit:
	includeDeclarationModule* componentDeclaration? includeDeclarationConfiguration*
		componentDeclaration EOF;

includeDeclarationModule: HASHTAG INCLUDE qualifiedName;

includeDeclarationConfiguration: HASHTAG INCLUDE qualifiedName;

qualifiedName: singleLine;

//Move the character '|' if you want to read in just single file

componentDeclaration:
	moduleDeclaration
	| configurationDeclaration;

//This part is for the module file

moduleDeclaration: moduleSignature moduleImplementation;
moduleSignature:
	MODULE moduleName OPAR? CPAR? moduleSignatureBody;

moduleName: singleLine;

moduleSignatureBody: OBRACE usesOrProvides* CBRACE;

usesOrProvides: usesState | providesState;

usesState:
	USES INTERFACE usesInterfaceDescription* SCOL
	| USES OBRACE (INTERFACE usesInterfaceDescription SCOL)* CBRACE;

providesState:
	PROVIDES INTERFACE providesInterfaceDescription* SCOL
	| PROVIDES OBRACE (
		INTERFACE providesInterfaceDescription SCOL
	)* CBRACE;

usesInterfaceDescription: interfaceNameAs | interfaceName;

providesInterfaceDescription: interfaceNameAs | interfaceName;

interfaceNameAs: interfaceName AS interfaceName;

interfaceName: singleLine;

moduleImplementation:
	IMPLEMENTATION OBRACE moduleImplementationBody CBRACE;

moduleImplementationBody: block;

block: stat*;

stat:
	statement
	| event_stat
	| task_stat
	| static_stat
	| if_stat
	| enum_stat
	| while_stat
	| for_stat
	| switch_stat
	| other_stat
	| atomic_stat
	| define_stat
	| call_stat
	| packet_define
	| OTHER {System.err.println("unknown char: " + $OTHER.text);};

packet_define:
	TYPEDEF common_name singleLine OBRACE statement* CBRACE statement;

call_stat: CALL common_name call_condition_block SCOL?;

call_condition_block: OPAR expr? CPAR;

define_stat:
	HASHTAG DEFINE common_name singleLine OBRACE statement CBRACE statement;

statement: anystatement SCOL? | expr SCOL;

event_stat:
	EVENT VOID? common_name event_condition_block event_stat_block?
	| EVENT VOID? common_name event_condition_block event_stat_block? (
		stat
	)* (EVENT VOID? common_name event_stat_block)?;

event_condition_block: OPAR expr? CPAR;

event_stat_block: OBRACE block CBRACE;

task_stat:
	TASK VOID? common_name task_condition_block task_stat_block?
	| TASK VOID? common_name task_condition_block task_stat_block? (
		stat
	)* (TASK VOID? common_name task_stat_block)?;

task_condition_block: OPAR expr? CPAR SCOL?;

task_stat_block: OBRACE block CBRACE;

static_stat:
	STATIC VOID? common_name static_condition_block static_stat_block?
	| STATIC VOID? common_name static_condition_block static_stat_block? (
		stat
	)* (STATIC VOID? common_name static_stat_block)?;

static_condition_block: OPAR expr? CPAR SCOL?;

static_stat_block: OBRACE block CBRACE;

other_stat:
	VOID? common_name other_condition_block other_stat_block?
	| VOID? common_name other_condition_block other_stat_block? (
		stat
	)* (VOID? common_name other_stat_block)?;

other_condition_block: OPAR expr? CPAR;

other_stat_block: OBRACE block CBRACE;

enum_stat: ENUM OBRACE ( expr COMMA?)* CBRACE SCOL;

common_name: singleLine | name_or_reserved;

if_stat:
	IF if_condition_block (ELSE IF if_condition_block)* (
		ELSE if_stat_block
	)?;

if_condition_block:
	OPAR (name_or_reserved* | expr*) CPAR if_stat_block
    | OPAR CPAR if_stat_block
	| OPAR expr CPAR if_stat_block
	| OPAR symbol CPAR if_stat_block
    ;

if_stat_block: OBRACE block CBRACE | stat;

while_stat: WHILE OPAR expr CPAR while_stat_block;

while_stat_block: OBRACE block CBRACE | stat;

for_stat:
	FOR OPAR ((expr | anystatement) SCOL?)+ CPAR for_stat_block;

for_stat_block: OBRACE block CBRACE | stat;

switch_stat:
	SWITCH switch_condition_block OBRACE (switch_stat_block)* CBRACE;

switch_condition_block: OPAR expr CPAR | OPAR symbol CPAR;

switch_stat_block:
	CASE (expr | anystatement) COLON stat* BREAK SCOL
	| DEFAULT COLON stat* BREAK SCOL;

atomic_stat: ATOMIC atomic_stat_block;

atomic_stat_block:
	OBRACE (statement | if_stat | other_stat) CBRACE
	| (statement | if_stat | other_stat);

expr:
	expr POW expr								# powExpr
	| MINUS expr								# unaryMinusExpr
	| NOT expr									# notExpr
	| expr op = (MULT | DIV | MOD) expr			# multiplicationExpr
	| expr op = (PLUS | MINUS | ASSIGN) expr	# additiveExpr
	| expr op = (LTEQ | GTEQ | LT | GT) expr	# relationalExpr
	| expr op = (EQ | NEQ) expr					# equalityExpr
	| expr AND expr								# andExpr
	| expr OR expr								# orExpr
	| atom (atom)*								# atomExpr
	| singleLine								# singlelineExpr
	| singleDoubleArray							# singleDoubleArrayExpr;

atom:
	STRING				# stringAtom
	| ID				# idAtom
	| (INT | FLOAT)		# numberAtom
	| (TRUE | FALSE)	# booleanAtom
	| HEX				# hexadecimalAtom;

symbol: OTHER # otherchar;

singleDoubleArray: OBRACE arrayElement* CBRACE;

arrayElement:
	atom COMMA?
	| (OBRACE atom COMMA atom CBRACE) COMMA?;

chars: (
		OPAR
		| CPAR
		| INC
		| DEC
		| FORWARDARROW
		| BACKARROW
		| COLONCOLON
		| AT
		| COMMA
		| MULT
		| GT
		| LT
		| DOT
		| ASSIGN
		| BITAND
		| OBRACK
		| CBRACK
	);

chars_no_comma: (
		OPAR
		| CPAR
		| INC
		| DEC
		| FORWARDARROW
		| BACKARROW
		| COLONCOLON
		| AT
		| MULT
		| GT
		| LT
		| DOT
		| ASSIGN
		| BITAND
		| OBRACK
		| CBRACK
	);

reservedwords: (
		VOID
		| RETURN
		| AS
		| POST
		| ATOMIC
		| ERROR
		| ABSTRACT
		| NEW
		| CALL
		| BREAK
	);

singleLine: (atom | symbol | chars) (
		DOT? (atom | symbol | chars)
	)*
	| (atom | symbol | chars | reservedwords) (
		atom
		| symbol
		| chars
		| reservedwords
	)*;

anystatement: (atom | symbol | chars) (atom | symbol | chars)*;

name_or_reserved: (atom | symbol | chars | reservedwords) (
		atom
		| symbol
		| chars
		| reservedwords
	)*;

name_with_char: (atom) (DOT? (chars | symbol | atom))*;

configurationDeclaration:
	configurationSignature configurationImplementation;

configurationSignature:
	COFIGURATION configurationName configurationSignatureBody;
configurationSignatureBody: OBRACE expr? CBRACE;

configurationName: singleLine;

//This part is for the configuration file

configurationImplementation:
	IMPLEMENTATION configurationImplementationBody;

configurationImplementationBody:
	OBRACE configurationImplementationDescription CBRACE;

configurationImplementationDescription: (
		componentsDefinition
		| componentsWiring
		| platformDefinition
	)*;
platformDefinition:
	HASHTAG IF platformDefinitionDescription* HASHTAG ENDIF;
platformDefinitionDescription:
	DEFINED? singleLine componentsDefinition
	| HASHTAG ELIF DEFINED? singleLine componentsDefinition
	| HASHTAG ELSE HASHTAG ERROR singleLine;

componentsDefinition:
	COMPONENTS componentsDefinitionDetails SCOL;

componentsDefinitionDetails: (componentsDefinitionName COMMA?)*;

componentsDefinitionName: componentsName;

componentsWiring: wiring SCOL;

wiring: wiringName;

wiringName:
	componentsName (FORWARDARROW | BACKARROW) componentsName;

componentsName:
	atom
	| name_with_char
	| NEW atom
	| NEW atom OPAR? atom? CPAR?
	| NEW atom AS atom
	| NEW? atom OPAR? atom? CPAR? AS atom;
