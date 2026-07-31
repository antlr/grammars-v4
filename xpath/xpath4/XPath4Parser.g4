// XPath v4.0
// Author--Ken Domino
// Based on the XPath 4.0 WG Review Draft at https://qt4cg.org/specifications/xquery-40/xpath-40.html
//
// This is an implementation of the XPath version 4.0 grammar.

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar XPath4Parser;

options {
    tokenVocab = XPath4Lexer;
    superClass = XPath4ParserBase;
}

// Abbreviated steps: "..", "@nodetest", or a simple node test (child axis abbreviation)
abbreviatedstep
    : DD
    | AT nodetest
    | simplenodetest
    ;

absolutepathexpr
    : SLASH relativepathexpr?
    | SS relativepathexpr
    ;

additiveexpr
    : multiplicativeexpr ((PLUS | MINUS) multiplicativeexpr)*
    ;

andexpr
    : comparisonexpr (KW_AND comparisonexpr)*
    ;

anyarraytype
    : KW_ARRAY OP STAR CP
    ;

anyfunctiontype
    : (KW_FUNCTION | KW_FN) OP STAR CP
    ;

anyitemtype
    : KW_ITEM OP CP
    ;

anymaptype
    : KW_MAP OP STAR CP
    ;

anyrecordtype
    : KW_RECORD OP STAR CP
    ;

anyxnodetype
    : KW_NODE OP CP
    ;

argument
    : exprsingle
    | argumentplaceholder
    ;

// Argument list: now supports keyword arguments (new in XPath 4.0)
argumentlist
    : OP (positionalarguments (COMMA keywordarguments)? | keywordarguments)? CP
    ;

argumentplaceholder
    : QM
    ;

arrayconstructor
    : squarearrayconstructor
    | curlyarrayconstructor
    ;

// Array types
arraytype
    : anyarraytype
    | typedarraytype
    ;

// ArrowExpr: supports both "=>" (sequence arrow) and "=!>" (mapping arrow)
arrowexpr
    : unaryexpr (sequencearrowtarget | mappingarrowtarget)*
    ;

// Arrow target: named function call or restricted dynamic call
arrowtarget
    : functioncall
    | restricteddynamiccall
    ;

attributename
    : eqname
    ;

attributenodetype
    : KW_ATTRIBUTE OP (nametestunion (COMMA typename_)?)? CP
    ;

// XPath 4.0 adds four new or-self axes and four new or-self siblings
axis
    : KW_ANCESTOR COLONCOLON
    | KW_ANCESTOR_OR_SELF COLONCOLON
    | KW_ATTRIBUTE COLONCOLON
    | KW_CHILD COLONCOLON
    | KW_DESCENDANT COLONCOLON
    | KW_DESCENDANT_OR_SELF COLONCOLON
    | KW_FOLLOWING COLONCOLON
    | KW_FOLLOWING_OR_SELF COLONCOLON
    | KW_FOLLOWING_SIBLING COLONCOLON
    | KW_FOLLOWING_SIBLING_OR_SELF COLONCOLON
    | KW_NAMESPACE COLONCOLON
    | KW_PARENT COLONCOLON
    | KW_PRECEDING COLONCOLON
    | KW_PRECEDING_OR_SELF COLONCOLON
    | KW_PRECEDING_SIBLING COLONCOLON
    | KW_PRECEDING_SIBLING_OR_SELF COLONCOLON
    | KW_SELF COLONCOLON
    ;

// Axis step: abbreviated or full, followed by zero or more predicates/lookups
axisstep
    : (abbreviatedstep | fullstep) (predicate | lookup)*
    ;

// Block form for if: if (cond) { expr }  (new in XPath 4.0)
bracedaction
    : enclosedexpr
    ;

// Expression operators in precedence order (lowest to highest)

// CastableExpr: now uses CastTarget + OccurrenceIndicator instead of SingleType
castableexpr
    : castexpr (KW_CASTABLE KW_AS casttarget occurrenceindicator?)?
    ;

castexpr
    : pipelineexpr (KW_CAST KW_AS casttarget occurrenceindicator?)?
    ;

// CastTarget: richer than XPath 3.1's SingleType -- can target composite types
casttarget
    : typename_
    | choiceitemtype
    | enumerationtype
    | typedarraytype
    | typedmaptype
    | typedrecordtype
    ;

// ChoiceItemType: parenthesized item type or union of item types, e.g. (T) or (T1 | T2)
choiceitemtype
    : OP itemtype (P itemtype)* CP
    ;

commentnodetype
    : KW_COMMENT OP CP
    ;

comparisonexpr
    : otherwiseexpr ((valuecomp | generalcomp | nodecomp) otherwiseexpr)?
    ;

compAttrconstructor
    : KW_ATTRIBUTE compnodename enclosedexpr
    ;

compCommentconstructor
    : KW_COMMENT enclosedexpr
    ;

compdocconstructor
    : KW_DOCUMENT enclosedexpr
    ;

compElemconstructor
    : KW_ELEMENT compnodename enclosedcontentexpr
    ;

compNSconstructor
    : KW_NAMESPACE compnodencname enclosedexpr
    ;

// Computed node name: a QName literal (#name) or dynamic expression ({expr})
compnodename
    : qnameliteral
    | OC expr CC
    ;

// Computed NCName: a marked NCName (#ncname) or dynamic expression ({expr})
compnodencname
    : markedncname
    | OC expr CC
    ;

compPIconstructor
    : KW_PROCESSING_INSTRUCTION compnodencname enclosedexpr
    ;

comptextconstructor
    : KW_TEXT enclosedexpr
    ;

computedconstructor
    : compdocconstructor
    | compElemconstructor
    | compAttrconstructor
    | compNSconstructor
    | comptextconstructor
    | compCommentconstructor
    | compPIconstructor
    ;

// Constant: literal value, QName literal, or boolean function call
constant
    : StringLiteral
    | MINUS numericliteral
    | qnameliteral
    | eqname OP CP
    ;

// ContextValueRef replaces ContextItemExpr from XPath 3.1 (still just ".")
contextvalueref
    : D
    ;

curlyarrayconstructor
    : KW_ARRAY enclosedexpr
    ;

// Namespace declarations (new in XPath 4.0)
defaultelementnamespacedecl
    : KW_DECLARE KW_DEFAULT KW_ELEMENT KW_NAMESPACE uriliteral
    ;

documentnodetype
    : KW_DOCUMENT_NODE OP (elementnodetype | schemaelementnodetype | nametestunion)? CP
    ;

// Dynamic node test: { expr } -- node type computed at runtime (new in XPath 4.0)
dynamicnodetest
    : enclosedexpr
    ;

elementname
    : eqname
    ;

elementnodetype
    : KW_ELEMENT OP (nametestunion (COMMA typename_ QM?)?)? CP
    ;

enclosedcontentexpr
    : enclosedexpr
    ;

enclosedexpr
    : OC expr? CC
    ;

// Enumeration types (new in XPath 4.0): enum("value1", "value2", ...)
enumerationtype
    : KW_ENUM OP StringLiteral (COMMA StringLiteral)* CP
    ;

// EQName: an expanded QName -- keywords are also valid names in XPath
eqname
    : QName
    | URIQualifiedName
    | KW_ANCESTOR
    | KW_ANCESTOR_OR_SELF
    | KW_AND
    | KW_ARRAY
    | KW_AS
    | KW_AT
    | KW_ATTRIBUTE
    | KW_CAST
    | KW_CASTABLE
    | KW_CHILD
    | KW_COMMENT
    | KW_DECLARE
    | KW_DEFAULT
    | KW_DESCENDANT
    | KW_DESCENDANT_OR_SELF
    | KW_DIV
    | KW_DOCUMENT_NODE
    | KW_DOCUMENT
    | KW_ELEMENT
    | KW_ELSE
    | KW_EMPTY_SEQUENCE
    | KW_ENUM
    | KW_EQ
    | KW_EVERY
    | KW_EXCEPT
    | KW_FN
    | KW_FOLLOWING
    | KW_FOLLOWING_OR_SELF
    | KW_FOLLOWING_SIBLING
    | KW_FOLLOWING_SIBLING_OR_SELF
    | KW_FOLLOWS
    | KW_FOLLOWS_OR_IS
    | KW_FOR
    | KW_FUNCTION
    | KW_GE
    | KW_GNODE
    | KW_GT
    | KW_IDIV
    | KW_IF
    | KW_IN
    | KW_INSTANCE
    | KW_INTERSECT
    | KW_IS
    | KW_IS_NOT
    | KW_ITEM
    | KW_JNODE
    | KW_KEY
    | KW_LE
    | KW_LET
    | KW_LT
    | KW_MAP
    | KW_MEMBER
    | KW_MOD
    | KW_NAMESPACE
    | KW_NAMESPACE_NODE
    | KW_NE
    | KW_NODE
    | KW_OF
    | KW_OR
    | KW_OTHERWISE
    | KW_PARENT
    | KW_PRECEDES
    | KW_PRECEDES_OR_IS
    | KW_PRECEDING
    | KW_PRECEDING_OR_SELF
    | KW_PRECEDING_SIBLING
    | KW_PRECEDING_SIBLING_OR_SELF
    | KW_PROCESSING_INSTRUCTION
    | KW_RECORD
    | KW_RETURN
    | KW_SATISFIES
    | KW_SCHEMA_ATTRIBUTE
    | KW_SCHEMA_ELEMENT
    | KW_SELF
    | KW_SOME
    | KW_TEXT
    | KW_THEN
    | KW_TO
    | KW_TREAT
    | KW_UNION
    | KW_VALUE
    ;

// [1]
expr
    : exprsingle (COMMA exprsingle)*
    ;

exprsingle
    : forexpr
    | letexpr
    | quantifiedexpr
    | ifexpr
    | orexpr
    ;

// FieldDeclaration: "?" marks the field as optional; "as SequenceType" gives its type
fielddeclaration
    : fieldname QM? (KW_AS sequencetype)?
    ;

fieldname
    : QName
    | StringLiteral
    ;

forbinding
    : foritembinding
    | formemberbinding
    | forentrybinding
    ;

forclause
    : KW_FOR forbinding
    ;

// Entry binding (new in XPath 4.0): iterates over map key-value entries
forentrybinding
    : (forentrykeybinding forentryvaluebinding | forentryvaluebinding) positionalvar? KW_IN exprsingle
    ;

forentrykeybinding
    : KW_KEY varnameandtype
    ;

forentryvaluebinding
    : KW_VALUE varnameandtype
    ;

// For expression: single ForClause followed by chained ForLetReturn body.
// Multiple bindings are expressed by chaining: for $x in A for $y in B return C
forexpr
    : forclause forletreturn
    ;

// Standard item binding: for $x [as T] [at $p] in expr
foritembinding
    : varnameandtype positionalvar? KW_IN exprsingle
    ;

// The body of a for/let expression -- can chain another for, let, or terminate with return.
// This avoids the indirect left-recursion that the spec's ForLetReturn would otherwise create.
forletreturn
    : forclause forletreturn
    | letclause forletreturn
    | KW_RETURN exprsingle
    ;

// Member binding (new in XPath 4.0): iterates over array members
formemberbinding
    : KW_MEMBER varnameandtype positionalvar? KW_IN exprsingle
    ;

// Full step: axis "::" nodetest
fullstep
    : axis nodetest
    ;

functionbody
    : enclosedexpr
    ;

functioncall
    : { this.IsFuncCall() }? eqname argumentlist
    ;

// Function item expressions
functionitemexpr
    : namedfunctionref
    | inlinefunctionexpr
    ;

// FunctionSignature separates the parameter list from the return type
functionsignature
    : OP paramlist? CP typedeclaration?
    ;

// Function types: "fn" is now an alias for "function"
functiontype
    : anyfunctiontype
    | typedfunctiontype
    ;

generalcomp
    : EQ
    | NE
    | LT
    | LE
    | GT
    | GE
    ;

// Generic node type (new in XPath 4.0): gnode()
gnodetype
    : KW_GNODE OP CP
    ;

// If expression: now allows both traditional "then/else" and block "{...}" forms
ifexpr
    : KW_IF OP expr CP (unbracedactions | bracedaction)
    ;

// InlineFunctionExpr: "fn" is now an alias for "function" (new in XPath 4.0)
inlinefunctionexpr
    : (KW_FUNCTION | KW_FN) functionsignature functionbody
    ;

instanceofexpr
    : treatexpr (KW_INSTANCE KW_OF sequencetype)?
    ;

intersectexceptexpr
    : recordputexpr ((KW_INTERSECT | KW_EXCEPT) recordputexpr)*
    ;

// ItemType: restructured in XPath 4.0
itemtype
    : regularitemtype
    | functiontype
    | typename_
    | choiceitemtype
    ;

// JSON node type (new in XPath 4.0): jnode(selector [, SequenceType])
jnodetype
    : KW_JNODE OP (STAR | jrootselector | QName | constant) (COMMA sequencetype)? CP
    ;

jrootselector
    : OP CP
    ;

// KeySpecifier: extended in XPath 4.0 to include ContextValueRef, VarRef, and LookupWildcard
keyspecifier
    : QName
    | literal
    | contextvalueref
    | varref
    | parenthesizedexpr
    | lookupwildcard
    ;

keywordargument
    : eqname CEQ argument
    ;

keywordarguments
    : keywordargument (COMMA keywordargument)*
    ;

// let $[ $x [as T] ] [as T] := expr  -- destructuring array binding
letarraybinding
    : DOLLAR OB varnameandtype CB typedeclaration? CEQ exprsingle
    ;

letbinding
    : letvaluebinding
    | letsequencebinding
    | letarraybinding
    | letmapbinding
    ;

letclause
    : KW_LET letbinding
    ;

// Let expression: single LetClause followed by chained ForLetReturn body.
letexpr
    : letclause forletreturn
    ;

// let ${ $x [as T] } [as T] := expr  -- destructuring map binding
letmapbinding
    : DOLLAR OC varnameandtype CC typedeclaration? CEQ exprsingle
    ;

// let $( $x [as T] ) [as T] := expr  -- destructuring sequence binding
letsequencebinding
    : DOLLAR OP varnameandtype CP typedeclaration? CEQ exprsingle
    ;

// let $x [as T] := expr
letvaluebinding
    : varnameandtype CEQ exprsingle
    ;

literal
    : numericliteral
    | StringLiteral
    ;

lookup
    : QM keyspecifier
    ;

lookupwildcard
    : STAR
    ;

mapconstructor
    : KW_MAP OC (mapconstructorentry (COMMA mapconstructorentry)*)? CC
    ;

// MapConstructorEntry: simplified to ExprSingle ":" ExprSingle (no separate key/value rules)
mapconstructorentry
    : exprsingle COLON exprsingle
    ;

// =!> target(...args)  -- maps arrow operator (new in XPath 4.0)
mappingarrowtarget
    : MAPPING_ARROW arrowtarget
    ;

// Map types
maptype
    : anymaptype
    | typedmaptype
    ;

// #NCName -- marks an unqualified name for namespace/PI constructors
markedncname
    : POUND QName
    ;

// XPath 4.0 adds Unicode × (U+00D7) and ÷ (U+00F7) as aliases for * and div
multiplicativeexpr
    : unionexpr ((STAR | TIMES_SIGN | KW_DIV | DIV_SIGN | KW_IDIV | KW_MOD) unionexpr)*
    ;

namedfunctionref
    : eqname POUND IntegerLiteral /* xgc: reserved-function-names */
    ;

namespacedecl
    : KW_DECLARE KW_NAMESPACE QName EQ uriliteral
    ;

namespacenodetype
    : KW_NAMESPACE_NODE OP CP
    ;

nametest
    : eqname
    | wildcard
    ;

// NameTestUnion: used in element/attribute node type tests
nametestunion
    : nametest
    ;

// NodeComp: extended in XPath 4.0 with is-not, precedes, follows, precedes-or-is, follows-or-is
nodecomp
    : KW_IS
    | KW_IS_NOT
    | nodeprecedes
    | nodefollows
    | KW_PRECEDES_OR_IS
    | KW_FOLLOWS_OR_IS
    ;

// Node constructors (computed constructors -- new in XPath 4.0 for XPath; from XQuery)
nodeConstructor
    : computedconstructor
    ;

nodefollows
    : GG
    | KW_FOLLOWS
    ;

nodeprecedes
    : LL
    | KW_PRECEDES
    ;

// Node test: union, simple, or dynamic (new in XPath 4.0)
nodetest
    : unionnodetest
    | simplenodetest
    | dynamicnodetest
    ;

numericliteral
    : IntegerLiteral
    | DecimalLiteral
    | DoubleLiteral
    ;

occurrenceindicator
    : QM
    | STAR
    | PLUS
    ;

orexpr
    : andexpr (KW_OR andexpr)*
    ;

// OtherwiseExpr: new in XPath 4.0 -- sequence coalescing with "otherwise"
otherwiseexpr
    : stringconcatexpr (KW_OTHERWISE stringconcatexpr)*
    ;

// Shared sub-parts
paramlist
    : varnameandtype (COMMA varnameandtype)*
    ;

parenthesizedexpr
    : OP expr? CP
    ;

// Path expressions
pathexpr
    : absolutepathexpr
    | relativepathexpr
    ;

// PipelineExpr: thin wrapper around ArrowExpr (placeholder for future pipeline operators)
pipelineexpr
    : arrowexpr
    ;

positionalargumentlist
    : OP positionalarguments? CP
    ;

positionalarguments
    : argument (COMMA argument)*
    ;

// Positional variable binding: at $pos
positionalvar
    : KW_AT DOLLAR eqname
    ;

// Postfix expressions: primaryexpr followed by zero or more postfix operations.
// PostfixExpr covers: FilterExpr, DynamicFunctionCall, LookupExpr, MethodCall
postfixexpr
    : primaryexpr (
        predicate
        | positionalargumentlist
        | lookup
        | METHOD_ARROW QName positionalargumentlist
    )*
    ;

predicate
    : OB expr CB
    ;

predicatelist
    : predicate*
    ;

// Primary expressions: extended in XPath 4.0 with NodeConstructor and StringTemplate
primaryexpr
    : literal
    | varref
    | parenthesizedexpr
    | contextvalueref
    | functioncall
    | nodeConstructor
    | functionitemexpr
    | mapconstructor
    | arrayconstructor
    | stringtemplate
    | unarylookup
    ;

processinginstructionnodetype
    : KW_PROCESSING_INSTRUCTION OP (QName | StringLiteral)? CP
    ;

// #EQName -- marks a qualified name for element/attribute constructors
qnameliteral
    : POUND eqname
    ;

// Quantified expression: some/every with one or more bindings
quantifiedexpr
    : (KW_SOME | KW_EVERY) quantifierbinding (COMMA quantifierbinding)* KW_SATISFIES exprsingle
    ;

quantifierbinding
    : varnameandtype KW_IN exprsingle
    ;

rangeexpr
    : additiveexpr (KW_TO additiveexpr)?
    ;

// RecordPutExpr: new in XPath 4.0 -- record field update with "+:="
recordputexpr
    : instanceofexpr (PLUS_CEQ instanceofexpr)*
    ;

// Record types (new in XPath 4.0): structural typing for maps
recordtype
    : anyrecordtype
    | typedrecordtype
    ;

regularitemtype
    : anyitemtype
    | xnodetype
    | gnodetype
    | jnodetype
    | maptype
    | arraytype
    | recordtype
    | enumerationtype
    ;

relativepathexpr
    : stepexpr ((SLASH | SS) stepexpr)*
    ;

// Restricted set of targets allowed for dynamic arrow calls
restricteddynamiccall
    : (varref | parenthesizedexpr | functionitemexpr | mapconstructor | arrayconstructor) positionalargumentlist
    ;

schemaattributenodetype
    : KW_SCHEMA_ATTRIBUTE OP attributename CP
    ;

schemaelementnodetype
    : KW_SCHEMA_ELEMENT OP elementname CP
    ;

// Name selector: a qualified name or wildcard (for abbreviated child/attribute axis)
selector
    : eqname
    | wildcard
    ;

// => target(...args)
sequencearrowtarget
    : EG arrowtarget
    ;

sequencetype
    : KW_EMPTY_SEQUENCE OP CP
    | itemtype occurrenceindicator?
    ;

// Simple map expression
simplemapexpr
    : pathexpr (BANG pathexpr)*
    ;

// Simple node test: type test or element/attribute name selector
simplenodetest
    : typetest
    | selector
    ;

squarearrayconstructor
    : OB (exprsingle (COMMA exprsingle)*)? CB
    ;

stepexpr
    : postfixexpr
    | axisstep
    ;

stringconcatexpr
    : rangeexpr (PP rangeexpr)*
    ;

// String template: backtick-delimited interpolated string (new in XPath 4.0).
// The lexer captures the full template as a single token (simplified).
// A production implementation would parse embedded { expr } with lexer modes.
stringtemplate
    : StringTemplate
    ;

textnodetype
    : KW_TEXT OP CP
    ;

treatexpr
    : castableexpr (KW_TREAT KW_AS sequencetype)?
    ;

typedarraytype
    : KW_ARRAY OP sequencetype CP
    ;

typedeclaration
    : KW_AS sequencetype
    ;

typedfunctionparam
    : (DOLLAR eqname KW_AS)? sequencetype
    ;

// TypedFunctionParam: optional named parameter ("$name as") followed by a SequenceType
typedfunctionparamlist
    : typedfunctionparam (COMMA typedfunctionparam)*
    ;

typedfunctiontype
    : (KW_FUNCTION | KW_FN) OP typedfunctionparamlist? CP KW_AS sequencetype
    ;

// TypedMapType now uses ItemType for key (not just AtomicOrUnionType)
typedmaptype
    : KW_MAP OP itemtype COMMA sequencetype CP
    ;

typedrecordtype
    : KW_RECORD OP fielddeclaration (COMMA fielddeclaration)* CP
    ;

// Type name
typename_
    : eqname
    ;

simpletypename
    : typename_
    ;

// Type test: covers gnode, XML node types, and JSON node types
typetest
    : gnodetype
    | xnodetype
    | jnodetype
    ;

unaryexpr
    : (MINUS | PLUS)* valueexpr
    ;

unarylookup
    : QM keyspecifier
    ;

// Type declarations and sequence types

unbracedactions
    : KW_THEN exprsingle KW_ELSE exprsingle
    ;

unionexpr
    : intersectexceptexpr ((KW_UNION | P) intersectexceptexpr)*
    ;

// Union node test: (T1 | T2 | ...) -- selects nodes matching any of the tests
unionnodetest
    : OP simplenodetest (P simplenodetest)+ CP
    ;

uriliteral
    : StringLiteral
    ;

valuecomp
    : KW_EQ
    | KW_NE
    | KW_LT
    | KW_LE
    | KW_GT
    | KW_GE
    ;

valueexpr
    : simplemapexpr
    ;

// Variable name with optional type (replaces old "$ varname typedeclaration?")
varnameandtype
    : DOLLAR eqname typedeclaration?
    ;

varref
    : DOLLAR eqname
    ;

// Wildcard: extended in XPath 4.0 with URIQualifiedStar (BracedURILiteral "*")
wildcard
    : STAR
    | QName CS
    | SC QName
    | BracedURILiteral STAR
    ;

// XML node types (XNodeType) -- replaces and renames the old KindTest rules
xnodetype
    : documentnodetype
    | elementnodetype
    | attributenodetype
    | schemaelementnodetype
    | schemaattributenodetype
    | processinginstructionnodetype
    | commentnodetype
    | textnodetype
    | namespacenodetype
    | anyxnodetype
    ;

// Top-level XPath expression. XPath 4.0 allows optional namespace declarations at the start.
xpath
    : (defaultelementnamespacedecl SEMI)? (namespacedecl SEMI)* expr EOF
    ;

// Entry point for testing: semicolon-separated expressions
auxilary
    : (expr SEMI)+ EOF
    ;