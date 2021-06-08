/*
 [The "BSD licence"] Copyright (c) 2014 Leonardo Lucena Copyright (c) 2018 Andrey Stolyarov All
 rights reserved.

 Redistribution and use in source and binary forms, with or without modification, are permitted
 provided that the following conditions are met: 1. Redistributions of source code must retain the
 above copyright notice, this list of conditions and the following disclaimer. 2. Redistributions in
 binary form must reproduce the above copyright notice, this list of conditions and the following
 disclaimer in the documentation and/or other materials provided with the distribution. 3. The name
 of the author may not be used to endorse or promote products derived from this software without
 specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.
 */
/*
 Derived from https://github.com/scala/scala/blob/2.12.x/spec/13-syntax-summary.md
 */
//TODO XML
parser grammar ScalaParser;

@parser::header {
import java.util.Stack;
}

options {
	tokenVocab = ScalaLexer;
}

literal:
	Minus? (IntegerLiteral | FloatingPointLiteral)
	| BooleanLiteral
	| CharacterLiteral
	| interpolatedString
	| symbolLiteral
	| StringLiteral
	| Null;

qualId: identifier (Dot identifier)*;

ids: identifier (Comma identifier)*;

path: stableId | (identifier Dot)? This;

stableId:
	identifier (Dot identifier)*
	| (identifier Dot)? (This | Super classQualifier?) (
		Dot identifier
	)+;

classQualifier: LBracket identifier RBracket;

type_:
	functionArgTypes Arrow type_
	| infixType existentialClause?;

functionArgTypes:
	infixType
	| LParen (paramType (Comma paramType)*)? RParen;

existentialClause:
	ForSome LBrace existentialDcl (semi existentialDcl)* RBrace;

existentialDcl: Type typeDcl | Val valDcl;

infixType: compoundType (identifier NL? compoundType)*;

compoundType:
	annotType (With annotType)* refinement?
	| refinement;

annotType: simpleType annotation*;

simpleType:
	(path Dot Type | stableId | LParen types RParen) (
		typeArgs
		| Hash identifier
	)*
	| literal /* BooleanLiteral is a type apparently, but I am going to keep it general */;

typeArgs: LBracket types RBracket;

types: type_ (Comma type_)*;

refinement: NL? LBrace refineStat? (semi refineStat?)* RBrace;

refineStat: dcl | Type typeDef;

typePat: type_;

ascription: Colon (infixType | annotation+ | UnderScore Star);

expr: ((bindings | Implicit? identifier | UnderScore) Arrow)* expr1;

expr1:
	If LParen expr RParen NL* expr (semi? Else expr)?								# ifExpr
	| While LParen expr RParen NL* expr												# whileExpr
	| Try expr (Catch expr)? (Finally expr)?										# tryExpr
	| Do expr semi? While LParen expr RParen										# doWhileExpr
	| For (LParen enumerators RParen | LBrace enumerators RBrace) NL* Yield? expr	# forExpr
	| Throw expr																	# throwExpr
	| Return expr?																	# returnExpr
	| (simpleExpr Dot)? identifier Eq expr											# assignmentExpr
	| simpleExpr1 argumentExprs Eq expr												# TODOExpr
	| postfixExpr (ascription | Match LBrace caseClauses RBrace)?					# Todo2Expr;

postfixExpr: infixExpr (identifier NL?)?;

infixExpr: prefixExpr | infixExpr identifier typeParamClause? NL? infixExpr;

prefixDef: Minus | Plus | Tilde | Exclamation;

prefixExpr: prefixDef? simpleExpr;

simpleExpr:
	New (classTemplate | templateBody)
	| blockExpr
	| simpleExpr1 UnderScore?;

simpleExpr1:
	(
		literal
		// operator
		| path
		| UnderScore
		| LParen exprs? RParen
		| (New (classTemplate | templateBody) | blockExpr) (
			Dot identifier
			| typeArgs
		)
		| xmlExpr
	) (argumentExprs | UnderScore? (Dot identifier | typeArgs))*;

//
xmlExpr: xmlContent element*;

element: emptyElemTag | sTag content? eTag;

emptyElemTag: XMLOpenTag Name? attribute* XMLAutoClose;
sTag: XMLOpenTag Name? attribute* XMLCloseTag;
eTag: XMLClosingNodeTag Name? XMLCloseTag;
content: (CharData | content1)+;
content1: xmlContent | reference | scalaExpr;
reference: EntityRef | CharRef;
xmlContent: element | cdSect /* | PI*/;
cdSect: CDataChunk;
attribute: Name EQUALS attValue;
attValue: XMLString | scalaExpr;

scalaExpr: LBraceXML block RBraceXML | LBrace block RBrace;

//charData:;
xmlPattern: elementPattern;

elementPattern: emptyElemTagP | sTagP contentP? eTagP;

emptyElemTagP: XMLOpenTag Name XMLAutoClose;
sTagP: XMLOpenTag Name XMLCloseTag;
eTagP: XMLClosingNodeTag Name XMLCloseTag;

contentP: ( contentP1 | CharData)+;

contentP1:
	elementPattern
	| cdSect
	//| PI
	| scalaPatterns;

scalaPatterns: LBrace patterns RBrace;

//

exprs: expr (Comma expr)* Comma?;

argumentExprs:
	LParen exprs? RParen
	| LParen (exprs Comma)? postfixExpr Colon UnderScore Star RParen
	| NL? blockExpr;

blockExpr: LBrace caseClauses RBrace | LBrace block RBrace;

block: blockStat? ( semi blockStat?)* resultExpr?;

blockStat:
	import_
	| (annotation NL?)* (Implicit? Lazy? def | localModifier* tmplDef)
	| expr1;

resultExpr:
	expr1
	| (
		bindings
		| (Implicit? identifier | UnderScore) (
			Colon compoundType
		)?
	) Arrow block;

enumerators: generator ( semi generator)*;

generator:
	pattern1 Assign expr (semi? guard | semi pattern1 Eq expr)*;

caseClauses: caseClause+;

caseClause: Case pattern guard? Arrow block;

guard: If NL? postfixExpr;

pattern: pattern1 (NL? Or NL? pattern1)*;

pattern1: (BackTick VarId BackTick | VarId | UnderScore) Colon typePat
	| pattern2;

pattern2: identifier (At pattern3)? | pattern3;

pattern3: simplePattern (identifier NL? simplePattern)*;

simplePattern:
	UnderScore
	| VarId
	| literal
	| stableId (
		LParen (
			patterns?
			| (patterns Comma)? (identifier At)? UnderScore Star
		) RParen
	)?
	| LParen patterns? RParen
	| xmlPattern;

patterns: pattern (Comma patterns)? | UnderScore Star;

typeParamClause:
	LBracket variantTypeParam (Comma variantTypeParam)* RBracket;

funTypeParamClause:
	LBracket typeParam (Comma typeParam)* RBracket;

variantTypeParam: annotation* (Plus | Minus)? typeParam;

typeParam: annotation* (identifier | UnderScore) typeParamClause? (
		LowerType type_
	)? (UpperType type_)? (ViewBound type_)* (Colon type_)*;

paramClauses: paramClause* (NL? LParen Implicit params RParen)?;

paramClause: NL? LParen params? RParen;

params: param (Comma param)* Comma?;

param: annotation* identifier (Colon paramType)? (Eq expr)?;

paramType: Arrow type_ | type_ Star?;

classParamClauses:
	classParamClause* (NL? LParen Implicit classParams RParen)?;

classParamClause: NL? LParen classParams? RParen;

classParams: classParam (Comma classParam)*;

classParam:
	annotation* (modifier NL?)* (Val | Var)? identifier
	//(
	Colon paramType
	//)?
	(Eq expr)?;

bindings: LParen (binding (Comma binding)*)? RParen;

binding: (identifier | UnderScore) (Colon type_)?;

modifier: localModifier | accessModifier | Override;

localModifier: Abstract | Final | Sealed | Implicit | Lazy;

accessModifier: (Private | Protected) accessQualifier?;

accessQualifier: LBracket (identifier | This) RBracket;

annotation: At simpleType argumentExprs*;

constrAnnotation: At simpleType argumentExprs;

templateBody:
	NL? LBrace selfType? templateStat? (semi templateStat?)* semi* RBrace;

templateStat:
	import_
	| (annotation NL?)* (modifier NL?)* (def | dcl)
	| expr;

selfType: (identifier (Colon type_)? | This Colon type_) Arrow;

import_: Import importExpr (Comma importExpr)*;

importExpr:
	stableId Dot (identifier | UnderScore | importSelectors);

importSelectors:
	LBrace NL* (importSelector Comma NL*)* (importSelector | UnderScore)? NL* RBrace;

importSelector: identifier (Arrow (identifier | UnderScore))?;

dcl: Val valDcl | Var varDcl | Def funDcl | Type NL* typeDcl;

valDcl: ids Colon type_;

varDcl: ids Colon type_;

funDcl: funSig (Colon type_)?;

funSig: identifier funTypeParamClause? paramClauses;

typeDcl:
	identifier typeParamClause? (LowerType type_)? (
		UpperType type_
	)?;

patVarDef: Val patDef | Var varDef;

def: patVarDef | Def funDef | Type NL* typeDef | tmplDef;

patDef: pattern2 (Comma pattern2)* (Colon type_)? Eq expr;

varDef: patDef | ids Colon type_ Eq UnderScore;

funDef:
	funSig ((Colon type_)? Eq (expr | Macro qualId funTypeParamClause?) | NL? LBrace block RBrace)
	| This paramClause paramClauses (
		Eq constrExpr
		| NL? constrBlock
	);

typeDef: identifier typeParamClause? Eq type_;

tmplDef:
	Case? (Class classDef | Object objectDef)
	| Trait traitDef;

classDef:
	identifier typeParamClause? constrAnnotation* accessModifier? classParamClauses classTemplateOpt
		?;

traitDef: identifier typeParamClause? traitTemplateOpt?;

objectDef: identifier classTemplateOpt?;

classTemplateOpt: (Extends (templateBody | classTemplate))
	| templateBody;

traitTemplateOpt: (Extends (templateBody | traitTemplate))
	| templateBody;

classTemplate: earlyDefs? classParents templateBody?;

traitTemplate: earlyDefs? traitParents templateBody?;

classParents: constr (With annotType)*;

traitParents: annotType (With annotType)*;

constr: annotType (LParen exprs? RParen)*;

earlyDefs: LBrace (earlyDef (semi earlyDef)*)? RBrace With;

earlyDef: (annotation NL?)* (modifier NL?)* patVarDef;

constrExpr: selfInvocation | constrBlock;

constrBlock: LBrace selfInvocation (semi blockStat?)* RBrace;

selfInvocation: This argumentExprs+;

topStatSeq: topStat ( semi topStat)*;

topStatFile: topStat EOF;

topStat: (annotation NL?)* (modifier NL?)* tmplDef
	| import_
	| packageObject
	| packaging;

packaging: Package qualId NL? LBrace topStatSeq RBrace;

packageObject: Package Object objectDef;

compilationUnit: (Package qualId semi)* topStatSeq? (
		SemiColon
		| NL+
	)* EOF;

// Inspired from the logic of CSharp's grammar
interpolatedString:
	interpolatedStringSingleLine
	| interpolatedStringMultiLine;
interpolatedStringSingleLine:
	InterpolatedSingleLineStringStart (
		StringSingle
		| EscapeSingle
		| DoubleDollarSingle
		| escape
	)* DoubleQuoteSingle;

interpolatedStringMultiLine:
	InterpolatedMultiLineStringStart (
		DoubleQuoteMulti? DoubleQuoteMulti? (
			StringMulti
			| EscapeMulti
			| DoubleDollarMulti
			| escape
		)
	)* TripleDoubleQuoteMulti;

escape:
	(DollarInsideSingle | DollarInsideMulti) (
		identifier
		| This
		| blockExpr
	);

identifier: plainId | BackTickId | UnderScore;

symbolLiteral: Quote plainId;

plainId: AlphaId | VarId | operator;
operator:
	(
		Hash
		| Exclamation
		| Plus
		| Minus
		| Colon
		| At
		| Or
		| Tilde
		| Star
		| Eq
		| OpChar
		| LowerType
		| UpperType
		| ViewBound
		| UnderScore
		| Arrow
	)+;

semi: SemiColon | NL+;