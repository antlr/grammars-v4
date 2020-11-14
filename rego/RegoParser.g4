/**
License Dual Licensed under BSD 3-Clause and MIT.  See included `LICENSE` file
for full text.
Copyright Arroyo Networks 2019
Authors: Josh Marshall
*/

parser grammar RegoParser;

options { tokenVocab=RegoLexer; }



/*****************************************************************************/
/* Rules */

root  
  : stmt* EOF
  ;

stmt
  : regoPackage
  | importDirective
  | regoRules
  | regoBody
  ;

/*****************************************************************************/

regoPackage
  : Package ref 
  ;

/*****************************************************************************/

importDirective
  : Import import_target=ref ( As import_target_rename_as=ref )?
  ;

/*****************************************************************************/

regoRules 
  : Default Name EqOper term
  | ruleHead ruleBody*
  ;

ruleHead 
  : Name ( LParan exprTermList? RParan )? ( LSBrace exprTerm RSBrace )? ( EqOper exprTerm )? 
  ;

ruleBody
  : ( Else (EqOper exprTerm)? )? nonEmptyBraceEnclosedBody
  ;

ruleExt 
  : regoElse
  | nonEmptyBraceEnclosedBody
  ;

regoElse 
  : Else EqOper term nonEmptyBraceEnclosedBody
  | Else nonEmptyBraceEnclosedBody
  ;

/*****************************************************************************/

regoBody 
  : query 
  | nonEmptyBraceEnclosedBody
  ;

nonEmptyBraceEnclosedBody 
  : LCBrace query RCBrace
  ;

query
  : literal ( Semicolon? literal )*
  ;

literal 
  : Not? literalExpr withKeyword*
  ;

literalExpr 
  : exprTerm (EqOper exprTerm)*
  ;

withKeyword 
  : With exprTerm As exprTerm
  ;

functionCall
  : ref LParan exprTermList? RParan
  ;

/*****************************************************************************/

exprTermPair 
  : exprTerm Colon exprTerm
  ;

termPair 
  : term Colon term
  ;

exprTermPairList 
  : exprTermPair ( Comma exprTermPair )*
  ;

/*****************************************************************************/

exprTerm 
  : relationExpr ( RelationOperator relationExpr )*
  ;

exprTermList 
  : exprTerm ( Comma exprTerm )*
  ;

relationExpr 
  : bitwiseOrExpr ( Mid bitwiseOrExpr )*
  ;

bitwiseOrExpr 
  : bitwiseAndExpr ( Ampersand bitwiseAndExpr )*
  ;

bitwiseAndExpr 
  : arithExpr ( ArithOperator arithExpr )*
  ;

arithExpr 
  : factorExpr ( FactorOperator factorExpr )*
  ;

factorExpr 
  : LParan exprTerm RParan
  | term 
  ;

term 
  : arrayComprehension 
  | objectComprehension 
  | setComprehension 
  | object 
  | array 
  | set 
  | ArithOperator? scalar 
  | functionCall
  | Not? ref
  ;

/*****************************************************************************/

arrayComprehension 
  : LSBrace term Mid query RSBrace
  ;

setComprehension 
  : LCBrace term Mid query RCBrace
  ;

objectComprehension 
  : LCBrace termPair Mid query RCBrace
  ;

object 
  : LCBrace ( objectItem ( Comma objectItem )* Comma? )? RCBrace
  ;

objectItem
  : ( scalar | ref ) Colon term
  ;

array 
  : LSBrace exprTermList? RSBrace
  ;

set 
  : emptySet 
  | nonEmptySet
  ;

emptySet
  : Set RParan
  ;

nonEmptySet
  : LCBrace exprTermList RCBrace
  ;

ref 
  : Name refOperand*
  ;

refOperand 
  : refOperandDot 
  | refOperandCanonical
  ;

/*Allow an exprTerm here for more dynamic lookups?*/
refOperandDot 
  : Dot Name 
  ;

refOperandCanonical 
  : LSBrace exprTerm RSBrace
  ;

scalar 
  : UnsignedNumber 
  | String 
  | Bool 
  | Null
  ;