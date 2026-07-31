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
    : '..'
    | '@' nodetest
    | simplenodetest
    ;

absolutepathexpr
    : '/' relativepathexpr?
    | '//' relativepathexpr
    ;

additiveexpr
    : multiplicativeexpr (( '+' | '-') multiplicativeexpr)*
    ;

andexpr
    : comparisonexpr ('and' comparisonexpr)*
    ;

anyarraytype
    : 'array' '(' '*' ')'
    ;

anyfunctiontype
    : ('function' | 'fn') '(' '*' ')'
    ;

anyitemtype
    : 'item' '(' ')'
    ;

anymaptype
    : 'map' '(' '*' ')'
    ;

anyrecordtype
    : 'record' '(' '*' ')'
    ;

anyxnodetype
    : 'node' '(' ')'
    ;

argument
    : exprsingle
    | argumentplaceholder
    ;

// Argument list: now supports keyword arguments (new in XPath 4.0)
argumentlist
    : '(' (positionalarguments ( ',' keywordarguments)? | keywordarguments)? ')'
    ;

argumentplaceholder
    : '?'
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
    : 'attribute' '(' (nametestunion ( ',' typename_)?)? ')'
    ;

// XPath 4.0 adds four new or-self axes and four new or-self siblings
axis
    : 'ancestor' '::'
    | 'ancestor-or-self' '::'
    | 'attribute' '::'
    | 'child' '::'
    | 'descendant' '::'
    | 'descendant-or-self' '::'
    | 'following' '::'
    | 'following-or-self' '::'
    | 'following-sibling' '::'
    | 'following-sibling-or-self' '::'
    | 'namespace' '::'
    | 'parent' '::'
    | 'preceding' '::'
    | 'preceding-or-self' '::'
    | 'preceding-sibling' '::'
    | 'preceding-sibling-or-self' '::'
    | 'self' '::'
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
    : castexpr ('castable' 'as' casttarget occurrenceindicator?)?
    ;

castexpr
    : pipelineexpr ('cast' 'as' casttarget occurrenceindicator?)?
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
    : '(' itemtype ('|' itemtype)* ')'
    ;

commentnodetype
    : 'comment' '(' ')'
    ;

comparisonexpr
    : otherwiseexpr ((valuecomp | generalcomp | nodecomp) otherwiseexpr)?
    ;

compAttrconstructor
    : 'attribute' compnodename enclosedexpr
    ;

compCommentconstructor
    : 'comment' enclosedexpr
    ;

compdocconstructor
    : 'document' enclosedexpr
    ;

compElemconstructor
    : 'element' compnodename enclosedcontentexpr
    ;

compNSconstructor
    : 'namespace' compnodencname enclosedexpr
    ;

// Computed node name: a QName literal (#name) or dynamic expression ({expr})
compnodename
    : qnameliteral
    | '{' expr '}'
    ;

// Computed NCName: a marked NCName (#ncname) or dynamic expression ({expr})
compnodencname
    : markedncname
    | '{' expr '}'
    ;

compPIconstructor
    : 'processing-instruction' compnodencname enclosedexpr
    ;

comptextconstructor
    : 'text' enclosedexpr
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
    | '-' numericliteral
    | qnameliteral
    | eqname '(' ')'
    ;

// ContextValueRef replaces ContextItemExpr from XPath 3.1 (still just ".")
contextvalueref
    : '.'
    ;

curlyarrayconstructor
    : 'array' enclosedexpr
    ;

// Namespace declarations (new in XPath 4.0)
defaultelementnamespacedecl
    : 'declare' 'default' 'element' 'namespace' uriliteral
    ;

documentnodetype
    : 'document-node' '(' (elementnodetype | schemaelementnodetype | nametestunion)? ')'
    ;

// Dynamic node test: { expr } -- node type computed at runtime (new in XPath 4.0)
dynamicnodetest
    : enclosedexpr
    ;

elementname
    : eqname
    ;

elementnodetype
    : 'element' '(' (nametestunion ( ',' typename_ '?'?)?)? ')'
    ;

enclosedcontentexpr
    : enclosedexpr
    ;

enclosedexpr
    : '{' expr? '}'
    ;

// Enumeration types (new in XPath 4.0): enum("value1", "value2", ...)
enumerationtype
    : 'enum' '(' StringLiteral (',' StringLiteral)* ')'
    ;

// EQName: an expanded QName -- keywords are also valid names in XPath
eqname
    : QName
    | URIQualifiedName
    | 'ancestor'
    | 'ancestor-or-self'
    | 'and'
    | 'array'
    | 'as'
    | 'at'
    | 'attribute'
    | 'cast'
    | 'castable'
    | 'child'
    | 'comment'
    | 'declare'
    | 'default'
    | 'descendant'
    | 'descendant-or-self'
    | 'div'
    | 'document-node'
    | 'document'
    | 'element'
    | 'else'
    | 'empty-sequence'
    | 'enum'
    | 'eq'
    | 'every'
    | 'except'
    | 'fn'
    | 'following'
    | 'following-or-self'
    | 'following-sibling'
    | 'following-sibling-or-self'
    | 'follows'
    | 'follows-or-is'
    | 'for'
    | 'function'
    | 'ge'
    | 'gnode'
    | 'gt'
    | 'idiv'
    | 'if'
    | 'in'
    | 'instance'
    | 'intersect'
    | 'is'
    | 'is-not'
    | 'item'
    | 'jnode'
    | 'key'
    | 'le'
    | 'let'
    | 'lt'
    | 'map'
    | 'member'
    | 'mod'
    | 'namespace'
    | 'namespace-node'
    | 'ne'
    | 'node'
    | 'of'
    | 'or'
    | 'otherwise'
    | 'parent'
    | 'precedes'
    | 'precedes-or-is'
    | 'preceding'
    | 'preceding-or-self'
    | 'preceding-sibling'
    | 'preceding-sibling-or-self'
    | 'processing-instruction'
    | 'record'
    | 'return'
    | 'satisfies'
    | 'schema-attribute'
    | 'schema-element'
    | 'self'
    | 'some'
    | 'text'
    | 'then'
    | 'to'
    | 'treat'
    | 'union'
    | 'value'
    ;

// [1]
expr
    : exprsingle (',' exprsingle)*
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
    : fieldname '?'? ('as' sequencetype)?
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
    : 'for' forbinding
    ;

// Entry binding (new in XPath 4.0): iterates over map key-value entries
forentrybinding
    : (forentrykeybinding forentryvaluebinding | forentryvaluebinding) positionalvar? 'in' exprsingle
    ;

forentrykeybinding
    : 'key' varnameandtype
    ;

forentryvaluebinding
    : 'value' varnameandtype
    ;

// For expression: single ForClause followed by chained ForLetReturn body.
// Multiple bindings are expressed by chaining: for $x in A for $y in B return C
forexpr
    : forclause forletreturn
    ;

// Standard item binding: for $x [as T] [at $p] in expr
foritembinding
    : varnameandtype positionalvar? 'in' exprsingle
    ;

// The body of a for/let expression -- can chain another for, let, or terminate with return.
// This avoids the indirect left-recursion that the spec's ForLetReturn would otherwise create.
forletreturn
    : forclause forletreturn
    | letclause forletreturn
    | 'return' exprsingle
    ;

// Member binding (new in XPath 4.0): iterates over array members
formemberbinding
    : 'member' varnameandtype positionalvar? 'in' exprsingle
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
    : '(' paramlist? ')' typedeclaration?
    ;

// Function types: "fn" is now an alias for "function"
functiontype
    : anyfunctiontype
    | typedfunctiontype
    ;

generalcomp
    : '='
    | '!='
    | '<'
    | '<='
    | '>'
    | '>='
    ;

// Generic node type (new in XPath 4.0): gnode()
gnodetype
    : 'gnode' '(' ')'
    ;

// If expression: now allows both traditional "then/else" and block "{...}" forms
ifexpr
    : 'if' '(' expr ')' (unbracedactions | bracedaction)
    ;

// InlineFunctionExpr: "fn" is now an alias for "function" (new in XPath 4.0)
inlinefunctionexpr
    : ('function' | 'fn') functionsignature functionbody
    ;

instanceofexpr
    : treatexpr ('instance' 'of' sequencetype)?
    ;

intersectexceptexpr
    : recordputexpr (( 'intersect' | 'except') recordputexpr)*
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
    : 'jnode' '(' ('*' | jrootselector | QName | constant) (',' sequencetype)? ')'
    ;

jrootselector
    : '(' ')'
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
    : eqname ':=' argument
    ;

keywordarguments
    : keywordargument (',' keywordargument)*
    ;

// let $[ $x [as T] ] [as T] := expr  -- destructuring array binding
letarraybinding
    : '$' '[' varnameandtype ']' typedeclaration? ':=' exprsingle
    ;

letbinding
    : letvaluebinding
    | letsequencebinding
    | letarraybinding
    | letmapbinding
    ;

letclause
    : 'let' letbinding
    ;

// Let expression: single LetClause followed by chained ForLetReturn body.
letexpr
    : letclause forletreturn
    ;

// let ${ $x [as T] } [as T] := expr  -- destructuring map binding
letmapbinding
    : '$' '{' varnameandtype '}' typedeclaration? ':=' exprsingle
    ;

// let $( $x [as T] ) [as T] := expr  -- destructuring sequence binding
letsequencebinding
    : '$' '(' varnameandtype ')' typedeclaration? ':=' exprsingle
    ;

// let $x [as T] := expr
letvaluebinding
    : varnameandtype ':=' exprsingle
    ;

literal
    : numericliteral
    | StringLiteral
    ;

lookup
    : '?' keyspecifier
    ;

lookupwildcard
    : '*'
    ;

mapconstructor
    : 'map' '{' (mapconstructorentry ( ',' mapconstructorentry)*)? '}'
    ;

// MapConstructorEntry: simplified to ExprSingle ":" ExprSingle (no separate key/value rules)
mapconstructorentry
    : exprsingle ':' exprsingle
    ;

// =!> target(...args)  -- maps arrow operator (new in XPath 4.0)
mappingarrowtarget
    : '=!>' arrowtarget
    ;

// Map types
maptype
    : anymaptype
    | typedmaptype
    ;

// #NCName -- marks an unqualified name for namespace/PI constructors
markedncname
    : '#' QName
    ;

// XPath 4.0 adds Unicode × (U+00D7) and ÷ (U+00F7) as aliases for * and div
multiplicativeexpr
    : unionexpr (( '*' | '\u00D7' | 'div' | '\u00F7' | 'idiv' | 'mod') unionexpr)*
    ;

namedfunctionref
    : eqname '#' IntegerLiteral /* xgc: reserved-function-names */
    ;

namespacedecl
    : 'declare' 'namespace' QName '=' uriliteral
    ;

namespacenodetype
    : 'namespace-node' '(' ')'
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
    : 'is'
    | 'is-not'
    | nodeprecedes
    | nodefollows
    | 'precedes-or-is'
    | 'follows-or-is'
    ;

// Node constructors (computed constructors -- new in XPath 4.0 for XPath; from XQuery)
nodeConstructor
    : computedconstructor
    ;

nodefollows
    : '>>'
    | 'follows'
    ;

nodeprecedes
    : '<<'
    | 'precedes'
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
    : '?'
    | '*'
    | '+'
    ;

orexpr
    : andexpr ('or' andexpr)*
    ;

// OtherwiseExpr: new in XPath 4.0 -- sequence coalescing with "otherwise"
otherwiseexpr
    : stringconcatexpr ('otherwise' stringconcatexpr)*
    ;

// Shared sub-parts
paramlist
    : varnameandtype (',' varnameandtype)*
    ;

parenthesizedexpr
    : '(' expr? ')'
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
    : '(' positionalarguments? ')'
    ;

positionalarguments
    : argument (',' argument)*
    ;

// Positional variable binding: at $pos
positionalvar
    : 'at' '$' eqname
    ;

// Postfix expressions: primaryexpr followed by zero or more postfix operations.
// PostfixExpr covers: FilterExpr, DynamicFunctionCall, LookupExpr, MethodCall
postfixexpr
    : primaryexpr (
        predicate
        | positionalargumentlist
        | lookup
        | '=?>' QName positionalargumentlist
    )*
    ;

predicate
    : '[' expr ']'
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
    : 'processing-instruction' '(' (QName | StringLiteral)? ')'
    ;

// #EQName -- marks a qualified name for element/attribute constructors
qnameliteral
    : '#' eqname
    ;

// Quantified expression: some/every with one or more bindings
quantifiedexpr
    : ('some' | 'every') quantifierbinding (',' quantifierbinding)* 'satisfies' exprsingle
    ;

quantifierbinding
    : varnameandtype 'in' exprsingle
    ;

rangeexpr
    : additiveexpr ('to' additiveexpr)?
    ;

// RecordPutExpr: new in XPath 4.0 -- record field update with "+:="
recordputexpr
    : instanceofexpr ('+:=' instanceofexpr)*
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
    : stepexpr (( '/' | '//') stepexpr)*
    ;

// Restricted set of targets allowed for dynamic arrow calls
restricteddynamiccall
    : (varref | parenthesizedexpr | functionitemexpr | mapconstructor | arrayconstructor) positionalargumentlist
    ;

schemaattributenodetype
    : 'schema-attribute' '(' attributename ')'
    ;

schemaelementnodetype
    : 'schema-element' '(' elementname ')'
    ;

// Name selector: a qualified name or wildcard (for abbreviated child/attribute axis)
selector
    : eqname
    | wildcard
    ;

// => target(...args)
sequencearrowtarget
    : '=>' arrowtarget
    ;

sequencetype
    : 'empty-sequence' '(' ')'
    | itemtype occurrenceindicator?
    ;

// Simple map expression
simplemapexpr
    : pathexpr ('!' pathexpr)*
    ;

// Simple node test: type test or element/attribute name selector
simplenodetest
    : typetest
    | selector
    ;

squarearrayconstructor
    : '[' (exprsingle ( ',' exprsingle)*)? ']'
    ;

stepexpr
    : postfixexpr
    | axisstep
    ;

stringconcatexpr
    : rangeexpr ('||' rangeexpr)*
    ;

// String template: backtick-delimited interpolated string (new in XPath 4.0).
// The lexer captures the full template as a single token (simplified).
// A production implementation would parse embedded { expr } with lexer modes.
stringtemplate
    : StringTemplate
    ;

textnodetype
    : 'text' '(' ')'
    ;

treatexpr
    : castableexpr ('treat' 'as' sequencetype)?
    ;

typedarraytype
    : 'array' '(' sequencetype ')'
    ;

typedeclaration
    : 'as' sequencetype
    ;

typedfunctionparam
    : ('$' eqname 'as')? sequencetype
    ;

// TypedFunctionParam: optional named parameter ("$name as") followed by a SequenceType
typedfunctionparamlist
    : typedfunctionparam (',' typedfunctionparam)*
    ;

typedfunctiontype
    : ('function' | 'fn') '(' typedfunctionparamlist? ')' 'as' sequencetype
    ;

// TypedMapType now uses ItemType for key (not just AtomicOrUnionType)
typedmaptype
    : 'map' '(' itemtype ',' sequencetype ')'
    ;

typedrecordtype
    : 'record' '(' fielddeclaration (',' fielddeclaration)* ')'
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
    : ('-' | '+')* valueexpr
    ;

unarylookup
    : '?' keyspecifier
    ;

// Type declarations and sequence types

unbracedactions
    : 'then' exprsingle 'else' exprsingle
    ;

unionexpr
    : intersectexceptexpr (( 'union' | '|') intersectexceptexpr)*
    ;

// Union node test: (T1 | T2 | ...) -- selects nodes matching any of the tests
unionnodetest
    : '(' simplenodetest ('|' simplenodetest)+ ')'
    ;

uriliteral
    : StringLiteral
    ;

valuecomp
    : 'eq'
    | 'ne'
    | 'lt'
    | 'le'
    | 'gt'
    | 'ge'
    ;

valueexpr
    : simplemapexpr
    ;

// Variable name with optional type (replaces old "$ varname typedeclaration?")
varnameandtype
    : '$' eqname typedeclaration?
    ;

varref
    : '$' eqname
    ;

// Wildcard: extended in XPath 4.0 with URIQualifiedStar (BracedURILiteral "*")
wildcard
    : '*'
    | QName ':*'
    | '*:' QName
    | BracedURILiteral '*'
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
    : (defaultelementnamespacedecl ';')? (namespacedecl ';')* expr EOF
    ;

// Entry point for testing: semicolon-separated expressions
auxilary
    : (expr ';')+ EOF
    ;