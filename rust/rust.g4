grammar rust
   ;

// lexer

// https://doc.rust-lang.org/reference/keywords.html strict
KW_AS: 'as';
KW_BREAK: 'break';
KW_CONST: 'const';
KW_CONTINUE: 'continue';
KW_CRATE: 'crate';
KW_ELSE: 'else';
KW_ENUM: 'enum';
KW_EXTERN: 'extern';
KW_FALSE: 'false';
KW_FN: 'fn';
KW_FOR: 'for';
KW_IF: 'if';
KW_IMPL: 'impl';
KW_IN: 'in';
KW_LET: 'let';
KW_LOOP: 'loop';
KW_MATCH: 'match';
KW_MOD: 'mod';
KW_MOVE: 'move';
KW_MUT: 'mut';
KW_PUB: 'pub';
KW_REF: 'ref';
KW_RETURN: 'return';
KW_SELFVALUE: 'self';
KW_SELFTYPE: 'Self';
KW_STATIC: 'static';
KW_STRUCT: 'struct';
KW_SUPER: 'super';
KW_TRAIT: 'trait';
KW_TRUE: 'true';
KW_TYPE: 'type';
KW_UNSAFE: 'unsafe';
KW_USE: 'use';
KW_WHERE: 'where';
KW_WHILE: 'while';

// 2018+
KW_ASYNC: 'async';
KW_AWAIT: 'await';
KW_DYN: 'dyn';

// reserved
KW_ABSTRACT: 'abstract';
KW_BECOME: 'become';
KW_BOX: 'box';
KW_DO: 'do';
KW_FINAL: 'final';
KW_MACRO: 'macro';
KW_OVERRIDE: 'override';
KW_PRIV: 'priv';
KW_TYPEOF: 'typeof';
KW_UNSIZED: 'unsized';
KW_VIRTUAL: 'virtual';
KW_YIELD: 'yield';

// reserved 2018+
KW_TRY: 'try';

// weak
KW_UNION: 'union';
KW_STATICLIFETIME: '\'static';

NON_KEYWORD_IDENTIFIER
   : [a-zA-Z][a-zA-Z0-9_]*
   | '_' [a-zA-Z0-9_]+
   ;

// comments https://doc.rust-lang.org/reference/comments.html TODO: remove xxx_DOC?
LINE_COMMENT: '//' (~[/!] | '//') ~[\n]* | '//';

BLOCK_COMMENT
   : '/*' (~[*!] | '**' | BLOCK_COMMENT_OR_DOC)
   (
      BLOCK_COMMENT_OR_DOC
      | ~[*]
   )*? '*/'
   | '/**/'
   | '/***/'
   ;

INNER_LINE_DOC: '//!' ~[\n\r]*; // isolated cr

INNER_BLOCK_DOC
   : '/*!' (BLOCK_COMMENT_OR_DOC | ~[*])*? '*/'
   ;

OUTER_LINE_DOC
   : '///' (~[/] ~[\n\r]*)?
   ; // isolated cr

OUTER_BLOCK_DOC
   : '/**' (~[*] | BLOCK_COMMENT_OR_DOC)
   (
      BLOCK_COMMENT_OR_DOC
      | ~[*]
   )*? '*/'
   ;

BLOCK_COMMENT_OR_DOC
   : BLOCK_COMMENT
   | INNER_BLOCK_DOC
   | OUTER_BLOCK_DOC
   ;

//ISOLATED_CR
// : '\r' // not followed with \n ;

// whitespace https://doc.rust-lang.org/reference/whitespace.html
WHITESPACE: [\p{Zs}] -> channel(HIDDEN);

// tokens char and string TODO: needn't so much detail, it's just token at all
CHAR_LITERAL
   : '\''
   (
      ~['\\\n\r\t]
      | QUOTE_ESCAPE
      | ASCII_ESCAPE
      | UNICODE_ESCAPE
   ) '\''
   ;

STRING_LITERAL
   : '"'
   (
      ~["]
      | QUOTE_ESCAPE
      | ASCII_ESCAPE
      | UNICODE_ESCAPE
      | ESC_NEWLINE
   )* '"'
   ;

RAW_STRING_LITERAL: 'r' RAW_STRING_CONTENT;

fragment RAW_STRING_CONTENT
   : '#' RAW_STRING_CONTENT '#'
   | '#"' .*? '"#'
   ;

BYTE_LITERAL
   : 'b\'' (. | QUOTE_ESCAPE | BYTE_ESCAPE) '\''
   ;

BYTE_STRING_LITERAL
   : 'b"' (. | QUOTE_ESCAPE | BYTE_ESCAPE)*? '"'
   ;

RAW_BYTE_STRING_LITERAL: 'br' RAW_STRING_CONTENT;

fragment ASCII_ESCAPE
   : '\\x' OCT_DIGIT HEX_DIGIT
   | COMMON_ESCAPE
   ;

fragment BYTE_ESCAPE
   : '\\x' HEX_DIGIT HEX_DIGIT
   | COMMON_ESCAPE
   ;

fragment COMMON_ESCAPE: '\\' [nrt\\0];

fragment UNICODE_ESCAPE
   : '\\u{' HEX_DIGIT HEX_DIGIT? HEX_DIGIT? HEX_DIGIT? HEX_DIGIT? HEX_DIGIT? '}'
   ;

fragment QUOTE_ESCAPE: '\\' ['"];

fragment ESC_NEWLINE: '\\' '\n';

// number

INTEGER_LITERAL
   :
   (
      DEC_LITERAL
      | BIN_LITERAL
      | OCT_LITERAL
      | HEX_LITERAL
   ) INTEGER_SUFFIX?
   ;

DEC_LITERAL: DEC_DIGIT (DEC_DIGIT | '_')*;

HEX_LITERAL
   : [+-]? '0x' '_'* HEX_DIGIT (HEX_DIGIT | '_')*
   ;

OCT_LITERAL
   : [+-]? '0o' '_'* OCT_DIGIT (OCT_DIGIT | '_')*
   ;

BIN_LITERAL: [+-]? '0b' '_'* [01] [01_]*;

fragment INTEGER_SUFFIX
   : 'u8'
   | 'u16'
   | 'u32'
   | 'u64'
   | 'u128'
   | 'usize'
   | 'i8'
   | 'i16'
   | 'i32'
   | 'i64'
   | 'i128'
   | 'isize'
   ;

FLOAT_LITERAL
   : DEC_LITERAL '.'
   | DEC_LITERAL ('.' DEC_LITERAL)? FLOAT_EXPONENT? FLOAT_SUFFIX?
   ;

fragment FLOAT_SUFFIX: 'f32' | 'f64';

fragment FLOAT_EXPONENT
   : [eE] [+-]? '_'* DEC_LITERAL
   ;

fragment OCT_DIGIT: [0-7];

fragment DEC_DIGIT: [0-9];

fragment HEX_DIGIT: [0-9a-fA-F];

// bool
BOOLEAN_LITERAL: 'true' | 'false';

SHEBANG
   : '#!' ~'\n' // TODO
   ;

LIFETIME_OR_LABEL: '\'' NON_KEYWORD_IDENTIFIER;

emmmm: EOF;

identifierOrKeyword
   : // only ascii
   NON_KEYWORD_IDENTIFIER
   | keyword
   ;

keyword
   : KW_AS
   | KW_BREAK
   | KW_CONST
   | KW_CONTINUE
   | KW_CRATE
   | KW_ELSE
   | KW_ENUM
   | KW_EXTERN
   | KW_FALSE
   | KW_FN
   | KW_FOR
   | KW_IF
   | KW_IMPL
   | KW_IN
   | KW_LET
   | KW_LOOP
   | KW_MATCH
   | KW_MOD
   | KW_MOVE
   | KW_MUT
   | KW_PUB
   | KW_REF
   | KW_RETURN
   | KW_SELFVALUE
   | KW_SELFTYPE
   | KW_STATIC
   | KW_STRUCT
   | KW_SUPER
   | KW_TRAIT
   | KW_TRUE
   | KW_TYPE
   | KW_UNSAFE
   | KW_USE
   | KW_WHERE
   | KW_WHILE

   // 2018+
   | KW_ASYNC
   | KW_AWAIT
   | KW_DYN
   // reserved
   | KW_ABSTRACT
   | KW_BECOME
   | KW_BOX
   | KW_DO
   | KW_FINAL
   | KW_MACRO
   | KW_OVERRIDE
   | KW_PRIV
   | KW_TYPEOF
   | KW_UNSIZED
   | KW_VIRTUAL
   | KW_YIELD
   | KW_TRY
   | KW_UNION
   | KW_STATICLIFETIME
   ;

identifier: NON_KEYWORD_IDENTIFIER | rawIdentifier;

rawIdentifier
   : 'r#' identifierOrKeyword
   ; // except crate self super Self

simplePath
   : '::'? simplePathSegment
   (
      '::' simplePathSegment
   )*
   ;

simplePathSegment
   : identifier
   | 'super'
   | 'self'
   | 'crate'
   | '$crate'
   ;

pathInExpression
   : '::'? pathExprSegment ('::' pathExprSegment)
   ;

pathExprSegment
   : pathIdentSegment ('::' genericArgs)?
   ;

pathIdentSegment
   : identifier
   | 'super'
   | 'self'
   | 'Self'
   | 'crate'
   | '$crate'
   ;

genericArgs
   : '<' '>'
   | '<' genericArgsLifetimes (',' genericArgsTypes)?
   (
      ',' genericArgsBindings
   )? ','? '>'
   | '<' genericArgsTypes (',' genericArgsBindings)? ','? '>'
   | '<' genericArgsBindings ','? '>'
   ;

genericArgsLifetimes: lifetime (',' lifetime)*;

genericArgsTypes: type (',' type)*;
genericArgsBindings
   : genericArgsBinding (',' genericArgsBinding)*
   ;
genericArgsBinding: identifier '=' type;

qualifiedPathInExpression
   : qualifiedPathType ('::' pathExprSegment)+
   ;
qualifiedPathType: '<' type ('as' typePath)? '>';
qualifiedPathInType
   : qualifiedPathType ('::' typePathSegment)+
   ;

typePath
   : '::'? typePathSegment ('::' typePathSegment)*
   ;
typePathSegment
   : pathIdentSegment '::'?
   (
      genericArgs
      | typePathFn
   )?
   ;

typePathFn: '(' typePathInputs? ')' ( '->' type)?;
typePathInputs: type (',' type)* ','?;

macroInvocation: simplePath '!' delimTokenTree;

delimTokenTree
   : '(' tokenTree* ')'
   | '[' tokenTree* ']'
   | '{' tokenTree* '}'
   ;

tokenTree
   : '::' // TODO: test, put all token except delim here...
   | delimTokenTree
   ;

macroInvocationSemi
   : simplePath '!' '(' tokenTree* ')' ';'
   | simplePath '!' '[' tokenTree* ']' ';'
   | simplePath '!' '{' tokenTree* '}'
   ;

macroRulesDefinition
   : 'macro_rules' '!' identifier macroRulesDef
   ;

macroRulesDef
   : '(' macroRules ')' ';'
   | '[' macroRules ']' ';'
   | '{' macroRules '}'
   ;

macroRules: macroRule (';' macroRule)* ';'?;
macroRule: macroMatcher '=>' macroTranscriber;

macroMatcher
   : '(' macroMatch* ')'
   | '[' macroMatch* ']'
   | '{' macroMatch* '}'
   ;

macroMatch
   : '::' // TODO: token except $ and delim
   | macroMatcher
   | '$' identifier ':' macroFragSpec
   | '$' '(' macroMatch+ ')' macroRepSep? macroRepOp
   ;

macroFragSpec
   : 'block'
   | 'expr'
   | 'ident'
   | 'item'
   | 'lifetime'
   | 'literal'
   | 'meta'
   | 'pat'
   | 'path'
   | 'stmt'
   | 'tt'
   | 'ty'
   | 'vis'
   ;

macroRepSep
   : '::' //TODO: Tokenexcept delimiters and repetition operators
   ;

macroRepOp: '*' | '+' | '?';

macroTranscriber: delimTokenTree;

crate: innerAttribute* item*;

configurationPredicate
   : configurationOption
   | configurationAll
   | configurationAny
   | configurationNot
   ;
configurationOption
   : identifier
   (
      '=' (STRING_LITERAL | RAW_STRING_LITERAL)
   )?
   ;
configurationAll
   : 'all' '(' configurationPredicateList? ')'
   ;
configurationAny
   : 'any' '(' configurationPredicateList? ')'
   ;
configurationNot
   : 'not' '(' configurationPredicate ')'
   ;

configurationPredicateList
   : configurationPredicate
   (
      ',' configurationPredicate
   )* ','?
   ;
cfgAttribute: 'cfg' '(' configurationPredicate ')';
cfgAttrAttribute
   : 'cfg_attr' '(' configurationPredicate ',' cfgAttrs? ')'
   ;
cfgAttrs: attr (',' attr)* ','?;

item: outerAttribute* visItem | macroItem;
visItem
   : visibility?
   (
      module
      | externCrate
      | useDeclaration
      | function
      | typeAlias
      | struct
      | enumeration
      | union
      | constantItem
      | staticItem
      | trait
      | implementation
      | externBlock
   )
   ;
macroItem
   : macroInvocationSemi
   | macroRulesDefinition
   ;

module
   : 'mod' identifier ';'
   | 'mod' identifier '{' innerAttribute* item* '}'
   ;

externCrate
   : 'extern' 'crate' crateRef asClause? ';'
   ;
crateRef: identifier | 'self';
asClause: 'as' (identifier | '_');

useDeclaration: 'use' useTree;
useTree
   : (simplePath? '::')? '*'
   | (simplePath? '::')? '{'
   (
      useTree (',' useTree)* ','?
   )? '}'
   | simplePath ('as' (identifier | '_'))?
   ;

function
   : functionQualifiers 'fn' identifier generics? '(' functionParameters? ')'
      functionReturnType? whereClause '?' blockExpression
   ;

functionQualifiers
   : asyncConstQualifiers? 'unsafe'? ('extern' abi?)?
   ;

asyncConstQualifiers: 'async' | 'const';

abi: STRING_LITERAL | RAW_STRING_LITERAL;

functionParameters
   : functionParam (',' functionParam)* ','?
   ;
functionParam: outerAttribute* pattern ':' type;
functionReturnType: '->' type;

typeAlias
   : 'type' identifier generics? whereClause? '=' type ';'
   ;

struct: structStruct | tupleStruct;
structStruct
   : 'struct' identifier generics? whereClause?
   (
      '{' structFields? '}'
      | ';'
   )
   ;
tupleStruct
   : 'struct' identifier generics? '(' tupleFields? ')' whereClause? ';'
   ;
structFields: structField (',' structField)* ','?;
structField
   : outerAttribute* visibility? identifier ':' type
   ;
tupleFields: tupleField (',' tupleField)* ','?;
tupleField: outerAttribute* visibility? type;

enumeration
   : 'enum' identifier generics? whereClause? '{' enumItems? '}'
   ;
enumItems: enumItem (',' enumItem)* ','?;
enumItem
   : outerAttribute* visibility? identifier
   (
      enumItemTuple
      | enumItemStruct
      | enumItemDiscriminant
   )?
   ;
enumItemTuple: '(' tupleFields? ')';
enumItemStruct: '{' structFields? '}';
enumItemDiscriminant: '=' expression;

union
   : 'union' identifier generics? whereClause? '{' structFields '}'
   ;

constantItem
   : 'const' (identifier | '_') ':' type '=' expression
   ;
staticItem
   : 'static' 'mut'? identifier ':' type '=' expression
   ;

trait
   : 'unsafe'? 'trait' identifier generics?
   (
      ':' typeParamBounds?
   )? whereClause? '{' innerAttribute* traitItem* '}'
   ;

traitItem
   : outerAttribute* visibility?
   (
      traitFunc
      | traitMethod
      | traitConst
      | traitType
      | macroInvocationSemi
   )
   ;
traitFunc
   : traitFunctionDecl (';' | blockExpression)
   ;
traitMethod
   : traitMethodDecl (';' | blockExpression)
   ;
traitFunctionDecl
   : functionQualifiers 'fn' identifier generics? '(' traitFunctionParameters?
      ')' functionReturnType? whereClause?
   ;
traitMethodDecl
   : functionQualifiers 'fn' identifier generics? '(' selfParam
   (
      ',' traitFunctionParam
   )* ',' ')' functionReturnType? whereClause?
   ;
traitFunctionParameters
   : traitFunctionParam (',' traitFunctionParam)* ','?
   ;
traitFunctionParam
   : outerAttribute* (pattern ':')? type
   ;

traitConst
   : 'const' identifier ':' type ('=' expression)? ';'
   ;
traitType
   : 'type' identifier (':' typeParamBounds?)? ';'
   ;

implementation: inherentImpl | traitImpl;
inherentImpl
   : 'impl' generics? type whereClause? '{' innerAttribute* inherentImplItem*
      '}'
   ;
inherentImplItem
   : outerAttribute*
   (
      macroInvocationSemi
      |
      (
         visibility?
         (
            constantItem
            | function
            | method
         )
      )
   )
   ;

traitImpl
   : 'unsafe'? 'impl' generics? '!'? typePath 'for' type whereClause? '{'
      innerAttribute* traitImplItem* '}'
   ;

traitImplItem
   : outerAttribute*
   (
      macroInvocationSemi
      |
      (
         visibility?
         (
            typeAlias
            | constantItem
            | function
            | method
         )
      )
   )
   ;

externBlock
   : 'extern' abi? '{' innerAttribute* externalItem* '}'
   ;

externalItem
   : outerAttribute*
   (
      macroInvocationSemi
      |
      (
         visibility?
         (
            externalStaticItem
            | externalFunctionItem
         )
      )
   )
   ;

externalStaticItem
   : 'static' 'mut'? identifier ':' type ';'
   ;

externalFunctionItem
   : 'fn' identifier generics? '('
   (
      namedFunctionParameters
      | namedFunctionParametersWithVariadics
   )? ')' functionReturnType? whereClause? ';'
   ;

namedFunctionParameters
   : namedFunctionParam (',' namedFunctionParam)* ','?
   ;

namedFunctionParam
   : outerAttribute* (identifier | '_') ':' type
   ;

namedFunctionParametersWithVariadics
   : (namedFunctionParam ',')* namedFunctionParam ',' outerAttribute* '...'
   ;

generics: '<' genericParams '>';

genericParams
   : lifetimeParams
   | ( lifetimeParam ',')* typeParams
   ;

lifetimeParams
   : (lifetimeParam ',')* lifetimeParam?
   ;

lifetimeParam
   : outerAttribute? LIFETIME_OR_LABEL
   (
      ':' lifetimeBounds
   )?
   ;

typeParams: ( typeParam ',')* typeParam?;

typeParam
   : outerAttribute? identifier
   (
      ':' typeParamBounds?
   )? ('=' type)?
   ;

whereClause
   : 'where' (whereClauseItem ',')* whereClauseItem?
   ;

whereClauseItem
   : lifetimeWhereClauseItem
   | typeBoundWhereClauseItem
   ;

lifetimeWhereClauseItem
   : lifetime ':' lifetimeBounds
   ;

typeBoundWhereClauseItem
   : forLifetimes? type ':' typeParamBounds?
   ;

forLifetimes: 'for' '<' lifetimeParams '>';

method
   : functionQualifiers 'fn' identifier generics?
   (
      selfParam (',' functionParam)* ','?
   ) functionReturnType? whereClause? blockExpression
   ;

selfParam
   : outerAttribute* (shorthandSelf | typedSelf)
   ;
shorthandSelf: ('&' | '&' lifetime)? 'mut'? 'self';
typedSelf: 'mut'? 'self' ':' type;

visibility
   : 'pub'
   (
      '('
      (
         'crate'
         | 'self'
         | 'super'
         | 'in' simplePath
      ) ')'
   )?
   ;
innerAttribute: '#' '!' '[' attr ']';

outerAttribute: '#' '[' attr ']';
attr: simplePath attrInput?;
attrInput
   : delimTokenTree
   | '=' literalExpression
   ; // w/o suffix

metaItem
   : simplePath
   (
      '=' literalExpression //w
      | '(' metaSeq ')'
   )?
   ;
metaSeq: metaItemInner (',' metaItemInner)* ','?;
metaItemInner: metaItem | literalExpression; // w

metaWord: identifier;
metaNameValueStr
   : identifier '='
   (
      STRING_LITERAL
      | RAW_STRING_LITERAL
   )
   ;
metaListPaths
   : identifier '('
   (
      simplePath (',' simplePath)* ','?
   )? ')'
   ;
metaListIdents
   : identifier '('
   (
      identifier (',' identifier)* ','?
   )? ')'
   ;
metaListNameValueStr
   : identifier '('
   (
      metaNameValueStr (',' metaNameValueStr)* ','?
   )? ')'
   ;

statement
   : ';'
   | item
   | letStatement
   | expressionStatement
   | macroInvocationSemi
   ;

letStatement
   : outerAttribute* 'let' pattern (':' type)?
   (
      '=' expression
   )? ';'
   ;
expressionStatement
   : expressionWithoutBlock ';'
   | expressionWithBlock ';'?
   ;
expression
   : expressionWithoutBlock
   | expressionWithBlock
   ;

expressionWithoutBlock
   : outerAttribute*
   (
      literalExpression
      | pathExpression
      | operatorExpression
      | groupedExpression
      | arrayExpression
      | awaitExpression
      | indexExpression
      | tupleExpression
      | tupleIndexingExpression
      | structExpression
      | enumerationVariantExpression
      | callExpression
      | methodCallExpression
      | fieldExpression
      | closureExpression
      | continueExpression
      | breakExpression
      | rangeExpression
      | returnExpression
      | macroInvocation
   )
   ;
expressionWithBlock
   : outerAttribute*
   (
      blockExpression
      | asyncBlockExpression
      | unsafeBlockExpression
      | loopExpression
      | ifExpression
      | ifLetExpression
      | matchExpression
   )
   ;

literalExpression
   : CHAR_LITERAL
   | STRING_LITERAL
   | RAW_STRING_LITERAL
   | BYTE_LITERAL
   | BYTE_STRING_LITERAL
   | RAW_BYTE_STRING_LITERAL
   | INTEGER_LITERAL
   | FLOAT_LITERAL
   | BOOLEAN_LITERAL
   ;

pathExpression
   : pathInExpression
   | qualifiedPathInExpression
   ;

blockExpression
   : '{' innerAttribute* statements? '}'
   ;

statements
   : statement+ expressionWithoutBlock?
   | expressionWithoutBlock
   ;

asyncBlockExpression
   : 'async' 'move'? blockExpression
   ;
unsafeBlockExpression: 'unsafe' blockExpression;

operatorExpression
   : borrowExpression
   | dereferenceExpression
   | errorPropagationExpression
   | negationExpression
   | arithmeticOrLogicalExpression
   | comparisonExpression
   | lazyBooleanExpression
   | typeCastExpression
   | assignmentExpression
   | compoundAssignmentExpression
   ;
borrowExpression: ('&' | '&&') 'mut'? expression;
dereferenceExpression: '*' expression;
errorPropagationExpression: expression '?';
negationExpression: ('-' | '!') expression;
arithmeticOrLogicalExpression
   : expression
   (
      '+'
      | '-'
      | '*'
      | '/'
      | '%'
      | '&'
      | '|'
      | '^'
      | '<<'
      | '>>'
   ) expression
   ;
//lifetimeToken: '\'' identifierOrKeyword | '\'_';
comparisonExpression
   : expression
   (
      '=='
      | '!='
      | '>'
      | '<'
      | '>='
      | '<='
   ) expression
   ;
lazyBooleanExpression
   : expression ('||' | '&&') expression
   ;

typeCastExpression: expression 'as' expression;
assignmentExpression: expression '=' expression;
compoundAssignmentExpression
   : expression
   (
      '+='
      | '-='
      | '*='
      | '/='
      | '%='
      | '&='
      | '|='
      | '^='
      | '<<='
      | '>>='
   ) expression
   ;

groupedExpression
   : '(' innerAttribute* expression ')'
   ;
arrayExpression
   : '[' innerAttribute* arrayElements? ']'
   ;
arrayElements
   : expression (',' expression)* ','?
   | expression ';' expression
   ;
indexExpression: expression '[' expression ']';
tupleExpression
   : '(' innerAttribute* tupleElements? ')'
   ;
tupleElements: (expression ',')+ expression?;
tupleIndexingExpression: expression '.' tupleIndex;
tupleIndex: INTEGER_LITERAL;

structExpression
   : structExprStruct
   | structExprTuple
   | structExprUnit
   ;
structExprStruct
   : pathInExpression '{' innerAttribute*
   (
      structExprFields
      | structBase
   )? '}'
   ;
structExprFields
   : structExprField (',' structExprField)*
   (
      ',' structBase
      | ','?
   )
   ;
structExprField
   : identifier
   | (identifier | tupleIndex) ':' expression
   ;
structBase: '..' expression;
structExprTuple
   : pathInExpression '(' innerAttribute*
   (
      expression (',' expression)* ','?
   )? ')'
   ;

structExprUnit: pathInExpression;

enumerationVariantExpression
   : enumExprStruct
   | enumExprTuple
   | enumExprFieldless
   ;
enumExprStruct
   : pathInExpression '{' enumExprFields? '}'
   ;

enumExprFields
   : enumExprField (',' enumExprField)* ','?
   ;
enumExprField
   : identifier
   | (identifier | tupleIndex) ':' expression
   ;
enumExprTuple
   : pathInExpression '('
   (
      expression (',' expression)* ','?
   )? ')'
   ;
enumExprFieldless: pathInExpression;

callExpression: expression '(' callParams? ')';
callParams: expression (',' expression)* ','?;
methodCallExpression
   : expression '.' pathExprSegment '(' callParams? ')'
   ;
fieldExpression: expression '.' identifier;

closureExpression
   : 'move'? ('||' | '|' closureParameters? '|')
   (
      expression
      | '->' typeNoBounds blockExpression
   )
   ;
closureParameters
   : closureParam (',' closureParam)* ','?
   ;
closureParam: outerAttribute* pattern (':' type)?;

loopExpression
   : loopLabel?
   (
      infiniteLoopExpression
      | predicateLoopExpression
      | predicatePatternLoopExpression
      | iteratorLoopExpression
   )
   ;

infiniteLoopExpression: 'loop' blockExpression;
predicateLoopExpression
   : 'while' expression /*except structExpression*/ blockExpression
   ;
predicatePatternLoopExpression
   : 'while' 'let' matchArmPatterns '=' expression blockExpression
   ;
iteratorLoopExpression
   : 'for' pattern 'in' expression blockExpression
   ;

loopLabel: LIFETIME_OR_LABEL ':';
breakExpression
   : 'break' LIFETIME_OR_LABEL? expression?
   ;
continueExpression
   : 'continue' LIFETIME_OR_LABEL? expression?
   ;

rangeExpression
   : rangeExpr
   | rangeFromExpr
   | rangeToExpr
   | rangeFullExpr
   | rangeInclusiveExpr
   | rangeToInclusiveExpr
   ;

rangeExpr: expression '..' expression;
rangeFromExpr: expression '..';
rangeToExpr: '..' expression;
rangeFullExpr: '..';
rangeInclusiveExpr: expression '..=' expression;
rangeToInclusiveExpr: '..=' expression;

ifExpression
   : 'if' expression blockExpression
   (
      'else'
      (
         blockExpression
         | ifExpression
         | ifLetExpression
      )
   )?
   ;

ifLetExpression
   : 'if' 'let' matchArmPatterns '=' expression blockExpression
   (
      'else'
      (
         blockExpression
         | ifExpression
         | ifLetExpression
      )
   )?
   ;

matchExpression
   : 'match' expression '{' innerAttribute* matchArms? '}'
   ;
matchArms
   :
   (
      matchArm '=>'
      (
         expressionWithoutBlock ','
         | expressionWithBlock ','?
      )
   )* matchArm '=>' expression ','?
   ;

matchArm
   : outerAttribute* matchArmPatterns matchArmGuard?
   ;
matchArmPatterns: '|'? pattern ('|' pattern)*;
matchArmGuard: 'if' expression;

returnExpression: 'return' expression?;
awaitExpression: expression '.' 'await';

pattern: patternWithoutRange | rangePattern;
patternWithoutRange
   : literalPattern
   | identifierPattern
   | wildcardPattern
   | restPattern
   | obsoleteRangePattern
   | referencePattern
   | structPattern
   | tupleStructPattern
   | groupedPattern
   | slicePattern
   | pathPattern
   | macroInvocation
   ;

literalPattern
   : BOOLEAN_LITERAL
   | CHAR_LITERAL
   | BYTE_LITERAL
   | STRING_LITERAL
   | RAW_STRING_LITERAL
   | BYTE_STRING_LITERAL
   | RAW_BYTE_STRING_LITERAL
   | '-'? INTEGER_LITERAL
   | '-'? FLOAT_LITERAL
   ;

identifierPattern
   : 'ref'? 'mut'? identifier ('@' pattern)?
   ;
wildcardPattern: '_';
restPattern: '..';
rangePattern
   : rangePatternBound '..=' rangePatternBound
   ;
obsoleteRangePattern
   : rangePatternBound '...' rangePatternBound
   ;
rangePatternBound
   : CHAR_LITERAL
   | BYTE_LITERAL
   | '-'? INTEGER_LITERAL
   | '-'? FLOAT_LITERAL
   | pathInExpression
   | qualifiedPathInExpression
   ;
referencePattern
   : ('&' | '&&') 'mut'? patternWithoutRange
   ;
structPattern
   : pathInExpression '{' structPatternElements? '}'
   ;
structPatternElements
   : structPatternFields
   (
      ',' structPatternEtCetera?
   )?
   | structPatternEtCetera
   ;
structPatternFields
   : structPatternField (',' structPatternField)*
   ;
structPatternField
   : outerAttribute*
   (
      tupleIndex ':' pattern
      | identifier ':' pattern
      | 'ref'? 'mut'? identifier
   )
   ;
structPatternEtCetera: outerAttribute* '..';
tupleStructPattern
   : pathInExpression '(' tupleStructItems? ')'
   ;
tupleStructItems: pattern (',' pattern)* ','?;
tuplePattern: '(' tuplePatternItems? ')';
tuplePatternItems
   : pattern ','
   | restPattern
   | pattern (',' pattern)+ ','?
   ;
groupedPattern: '(' pattern ')';
slicePattern: '[' slicePatternItems ']';
slicePatternItems: pattern (',' pattern)* ','?;
pathPattern
   : pathInExpression
   | qualifiedPathInExpression
   ;

type
   : typeNoBounds
   | implTraitType
   | traitObjectType
   ;
typeNoBounds
   : parenthesizedType
   | implTraitTypeOneBound
   | traitObjectTypeOneBound
   | typePath
   | tupleType
   | neverType
   | rawPointerType
   | referenceType
   | arrayType
   | sliceType
   | inferredType
   | qualifiedPathInType
   | bareFunctionType
   | macroInvocation
   ;
parenthesizedType: '(' type ')';
neverType: '!';
tupleType: '(' ( (type ',')+ type?) ')';
arrayType: '[' type ';' expression ']';
sliceType: '[' type ']';
referenceType: '&' lifetime? 'mut'? typeNoBounds;
rawPointerType: '*' ('mut' | 'const') typeNoBounds;

bareFunctionType
   : forLifetimes? functionQualifiers 'fn' '('
      functionParametersMaybeNamedVariadic? ')' bareFunctionReturnType?
   ;
bareFunctionReturnType: '->' typeNoBounds;
functionParametersMaybeNamedVariadic
   : maybeNamedFunctionParameters
   | maybeNamedFunctionParametersVariadic
   ;
maybeNamedFunctionParameters
   : maybeNamedParam (',' maybeNamedParam)* ','?
   ;
maybeNamedParam
   : outerAttribute* ((identifier | '_') ':')? type
   ;
maybeNamedFunctionParametersVariadic
   : (maybeNamedParam ',')* maybeNamedParam ',' outerAttribute* '...'
   ;
traitObjectType: 'dyn'? typeParamBounds;
traitObjectTypeOneBound: 'dyn'? traitBound;
implTraitType: 'impl' typeParamBounds;
implTraitTypeOneBound: 'impl' traitBound;
inferredType: '_';

typeParamBounds
   : typeParamBound ('+' typeParamBound)* '+'?
   ;
typeParamBound: lifetime | traitBound;
traitBound
   : '?'? forLifetimes? typePath
   | '(' '?'? forLifetimes? typePath ')'
   ;
lifetimeBounds: (lifetime '+')* lifetime?;
lifetime: LIFETIME_OR_LABEL | '\'static' | '\'_';