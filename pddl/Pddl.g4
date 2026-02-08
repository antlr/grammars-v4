/**
 * PDDL grammar for ANTLR v4
 *
 * Zeyn Saigol
 * School of Computer Science
 * University of Birmingham
 *
 * modified for v4 and made it language agnostic by
 * Hernán M. Foffani
 * Now part of the project
 * https://bitbucket.org/hfoffani/pddl-parser
 *
 *
 * Modified to add Multi-Agent PDDL (MAPDDL),
 * following some of the specification by Kovacs, 2012.
 * https://home.mit.bme.hu/~dkovacs/pubs/d.l.kovacs_2012_ICAPS-WIPC.pdf
 * Agents have the capability (in this implementation) to carry out
 * whichever actions they please, future work sees the addition of
 * agents to the goal condition.
 * Stewart Anderson 2024-04-03
 * (stewart.anderson@abdn.ac.uk)
 *
 * This file carries the same license from the original.
 */

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar Pddl;

tokens {
    DOMAIN,
    DOMAIN_NAME,
    REQUIREMENTS,
    TYPES,
    EITHER_TYPE,
    CONSTANTS,
    FUNCTIONS,
    PREDICATES,
    ACTION,
    DURATIVE_ACTION,
    PROBLEM,
    PROBLEM_NAME,
    PROBLEM_DOMAIN,
    OBJECTS,
    INIT,
    FUNC_HEAD,
    PRECONDITION,
    EFFECT,
    AND_GD,
    OR_GD,
    NOT_GD,
    IMPLY_GD,
    EXISTS_GD,
    FORALL_GD,
    COMPARISON_GD,
    AND_EFFECT,
    FORALL_EFFECT,
    WHEN_EFFECT,
    ASSIGN_EFFECT,
    NOT_EFFECT,
    PRED_HEAD,
    GOAL,
    BINARY_OP,
    UNARY_MINUS,
    INIT_EQ,
    INIT_AT,
    NOT_PRED_INIT,
    PRED_INST,
    PROBLEM_CONSTRAINT,
    PROBLEM_METRIC
}

/************* Start of grammar *******************/

pddlDoc
    : (domain | problem) EOF
    ;

/************* DOMAINS ****************************/

domain
    : '(' 'define' domainName requireDef? typesDef? constantsDef? predicatesDef? functionsDef? constraints? structureDef* ')'
    ;

domainName
    : '(' 'domain' name ')'
    ;

requireDef
    : '(' ':requirements' REQUIRE_KEY+ ')'
    ;

typesDef
    : '(' ':types' typedNameList ')'
    ;

// If have any typed names, they must come FIRST!
typedNameList
    : (name* | singleTypeNameList+ name*)
    ;

singleTypeNameList
    : (name+ '-' t = r_type)
    ;

r_type
    : ('(' 'either' primType+ ')')
    | primType
    ;

primType
    : name
    ;

functionsDef
    : '(' ':functions' functionList ')'
    ;

functionList
    : (atomicFunctionSkeleton+ ('-' functionType)?)*
    ;

atomicFunctionSkeleton
    : '(' functionSymbol typedVariableList ')'
    ;

functionSymbol
    : name
    ;

functionType
    : 'number'
    ; // Currently in PDDL only numeric functions are allowed

constantsDef
    : '(' ':constants' typedNameList ')'
    ;

predicatesDef
    : '(' ':predicates' (atomicFormulaSkeleton | privatePredicates)* ')'
    ;

privatePredicates
    : '(' ':private' typedVariableList (atomicFormulaSkeleton | singleTypedPredicate)* ')'
    ;

singleTypedPredicate
    : '(' predicate typedVariableList ')'
    ;

atomicFormulaSkeleton
    : '(' predicate typedVariableList ')'
    ;

predicate
    : name
    ;

// If have any typed variables, they must come FIRST!
typedVariableList
    : (VARIABLE* | singleTypeVarList+ VARIABLE*)
    ;

singleTypeVarList
    : (VARIABLE+ '-' t = r_type)
    ;

constraints
    : '(' ':constraints' conGD ')'
    ;

structureDef
    : actionDef
    | durativeActionDef
    | derivedDef
    ;

/************* ACTIONS ****************************/

actionDef
    : '(' ':action' actionSymbol parametersDecl* actionDefBody ')'
    ;

parametersDecl
    : ':parameters' '(' typedVariableList ')'
    | ':agent' typedVariableList
    ;

actionSymbol
    : name
    ;

// Should allow preGD instead of goalDesc for preconditions -
// but I can't get the LL(*) parsing to work
// This means 'preference' preconditions cannot be used
actionDefBody
    : (':precondition' (('(' ')') | precondition))? (':effect' (('(' ')') | effect))?
    ;

// to ease Listener implementation
precondition
    : goalDesc
    ;

//preGD
//	: prefGD
//	| '(' 'and' preGD* ')'
//	| '(' 'forall' '(' typedVariableList ')' preGD ')'
//	;
//
//prefGD
//	: '(' 'preference' name? goalDesc ')'
//	| goalDesc
//	;

goalDesc
    : atomicTermFormula
    | '(' 'and' goalDesc* ')'
    | '(' 'or' goalDesc* ')'
    | '(' 'not' goalDesc ')'
    | '(' 'imply' goalDesc goalDesc ')'
    | '(' 'exists' '(' typedVariableList ')' goalDesc ')'
    | '(' 'forall' '(' typedVariableList ')' goalDesc ')'
    | fComp
    ;

fComp
    : '(' binaryComp fExp fExp ')'
    ;

atomicTermFormula
    : '(' predicate term* ')'
    ;

term
    : name
    | VARIABLE
    ;

/************* DURATIVE ACTIONS ****************************/

durativeActionDef
    : '(' ':durative-action' actionSymbol parametersDecl* daDefBody ')'
    ;

daDefBody
    : ':duration' durationConstraint ':condition' (('(' ')') | daGD) ':effect' (
        ('(' ')')
        | daEffect
    )
    ;

daGD
    : prefTimedGD
    | '(' 'and' daGD* ')'
    | '(' 'forall' '(' typedVariableList ')' daGD ')'
    ;

prefTimedGD
    : timedGD
    | '(' 'preference' name? timedGD ')'
    ;

timedGD
    : '(' 'at' timeSpecifier goalDesc ')'
    | '(' 'over' interval goalDesc ')'
    ;

timeSpecifier
    : 'start'
    | 'end'
    ;

interval
    : 'all'
    ;

/************* DERIVED DEFINITIONS ****************************/

derivedDef
    : '(' ':derived' typedVariableList goalDesc ')'
    ;

/************* EXPRESSIONS ****************************/

fExp
    : NUMBER
    | '(' binaryOp fExp fExp2 ')'
    | '(' '-' fExp ')'
    | fHead
    ;

// This is purely a workaround for an ANTLR bug in tree construction
// http://www.antlr.org/wiki/display/ANTLR3/multiple+occurences+of+a+token+mix+up+the+list+management+in+tree+rewrites
fExp2
    : fExp
    ;

fHead
    : '(' functionSymbol term* ')'
    | functionSymbol
    ;

effect
    : '(' 'and' cEffect* ')'
    | cEffect
    ;

cEffect
    : '(' 'forall' '(' typedVariableList ')' effect ')'
    | '(' 'when' goalDesc condEffect ')'
    | pEffect
    ;

pEffect
    : '(' assignOp fHead fExp ')'
    | '(' 'not' atomicTermFormula ')'
    | atomicTermFormula
    ;

// TODO: why is this different from the "and cEffect" above? Does it matter?
condEffect
    : '(' 'and' pEffect* ')'
    | pEffect
    ;

// TODO: should these be uppercase & lexer section?
binaryOp
    : '*'
    | '+'
    | '-'
    | '/'
    ;

binaryComp
    : '>'
    | '<'
    | '='
    | '>='
    | '<='
    ;

assignOp
    : 'assign'
    | 'scale-up'
    | 'scale-down'
    | 'increase'
    | 'decrease'
    ;

/************* DURATIONS  ****************************/

durationConstraint
    : '(' 'and' simpleDurationConstraint+ ')'
    | '(' ')'
    | simpleDurationConstraint
    ;

simpleDurationConstraint
    : '(' durOp '?duration' durValue ')'
    | '(' 'at' timeSpecifier simpleDurationConstraint ')'
    ;

durOp
    : '<='
    | '>='
    | '='
    ;

durValue
    : NUMBER
    | fExp
    ;

daEffect
    : '(' 'and' daEffect* ')'
    | timedEffect
    | '(' 'forall' '(' typedVariableList ')' daEffect ')'
    | '(' 'when' daGD timedEffect ')'
    | '(' assignOp fHead fExpDA ')'
    ;

timedEffect
    : '(' 'at' timeSpecifier cEffect ')'
    | '(' 'at' timeSpecifier fAssignDA ')'
    | '(' assignOpT fHead fExp ')' /* fExpT in BNF but not making sense */
    ;

fAssignDA
    : '(' assignOp fHead fExpDA ')'
    ;

fExpDA
    : '(' ((binaryOp fExpDA fExpDA) | ('-' fExpDA)) ')'
    | '?duration'
    | fExp
    ;

assignOpT
    : 'increase'
    | 'decrease'
    ;

/*
 * We should have fExpT according to:
 * http://www.plg.inf.uc3m.es/ipc2011-deterministic/attachments/OtherContributions/kovacs-pddl-3.1-2011.pdf

fExpT
	: '(*' fExp #t ')'
	| '(*' #t fExp ')'
	| #t
	;
 */

/************* PROBLEMS ****************************/

problem
    : '(' 'define' problemDecl problemDomain requireDef? objectDecl? init goal probConstraints? metricSpec?
    // lengthSpec? This is not defined anywhere in the BNF spec
    ')'
    ;

problemDecl
    : '(' 'problem' name ')'
    ;

problemDomain
    : '(' ':domain' name ')'
    ;

objectDecl
    : '(' ':objects' objectList? ')'
    ;

objectList
    : objectDeclaration+
    ;

objectDeclaration
    : objectName+ '-' objectType
    | '(' ':private' objectName objectList? ')'
    ;

objectType
    : NAME
    ;

objectName
    : NAME
    ;

init
    : '(' ':init' initEl* ')'
    ;

initEl
    : nameLiteral
    | '(' '=' fHead NUMBER ')'
    | '(' 'at' NUMBER nameLiteral ')'
    ;

nameLiteral
    : atomicNameFormula
    | '(' 'not' atomicNameFormula ')'
    ;

atomicNameFormula
    : '(' predicate name* ')'
    ;

// Should allow preGD instead of goalDesc -
// but I can't get the LL(*) parsing to work
// This means 'preference' preconditions cannot be used
//goal : '(' ':goal' preGD ')'
goal
    : '(' ':goal' goalDesc ')'
    ;

probConstraints
    : '(' ':constraints' prefConGD ')'
    ;

prefConGD
    : '(' 'and' prefConGD* ')'
    | '(' 'forall' '(' typedVariableList ')' prefConGD ')'
    | '(' 'preference' name? conGD ')'
    | conGD
    ;

metricSpec
    : '(' ':metric' optimization metricFExp ')'
    ;

optimization
    : 'minimize'
    | 'maximize'
    ;

metricFExp
    : '(' binaryOp metricFExp metricFExp ')'
    | '(' ('*' | '/') metricFExp metricFExp+ ')'
    | '(' '-' metricFExp ')'
    | NUMBER
    | '(' functionSymbol name* ')'
    | functionSymbol
    | 'total-time'
    | '(' 'is-violated' name ')'
    ;

/************* CONSTRAINTS ****************************/

conGD
    : '(' 'and' conGD* ')'
    | '(' 'forall' '(' typedVariableList ')' conGD ')'
    | '(' 'at' 'end' goalDesc ')'
    | '(' 'always' goalDesc ')'
    | '(' 'sometime' goalDesc ')'
    | '(' 'within' NUMBER goalDesc ')'
    | '(' 'at-most-once' goalDesc ')'
    | '(' 'sometime-after' goalDesc goalDesc ')'
    | '(' 'sometime-before' goalDesc goalDesc ')'
    | '(' 'always-within' NUMBER goalDesc goalDesc ')'
    | '(' 'hold-during' NUMBER NUMBER goalDesc ')'
    | '(' 'hold-after' NUMBER goalDesc ')'
    ;

/************* LEXER ****************************/

REQUIRE_KEY
    : ':strips'
    | ':typing'
    | ':negative-preconditions'
    | ':disjunctive-preconditions'
    | ':equality'
    | ':existential-preconditions'
    | ':universal-preconditions'
    | ':quantified-preconditions'
    | ':conditional-effects'
    | ':fluents'
    | ':adl'
    | ':durative-actions'
    | ':derived-predicates'
    | ':timed-initial-literals'
    | ':preferences'
    | ':constraints'
    | ':numeric-fluents'
    | ':multi-agent'
    | ':unfactored-privacy'
    ;

/*
 * allowing keywords as identifier where allowed
 * may need more to specify
 */
name
    : NAME
    | 'at'
    | 'over'
    ;

NAME
    : LETTER ANY_CHAR*
    ;

fragment LETTER
    : 'a' ..'z'
    | 'A' ..'Z'
    ;

fragment ANY_CHAR
    : LETTER
    | '0' ..'9'
    | '-'
    | '_'
    ;

VARIABLE
    : '?' LETTER ANY_CHAR*
    ;

NUMBER
    : DIGIT+ ('.' DIGIT+)?
    ;

fragment DIGIT
    : '0' ..'9'
    ;

LINE_COMMENT
    : ';' ~('\n' | '\r')* '\r'? '\n' -> skip
    ;

WHITESPACE
    : (' ' | '\t' | '\r' | '\n')+ -> skip
    ;
