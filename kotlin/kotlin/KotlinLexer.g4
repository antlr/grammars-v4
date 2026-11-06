/**
 * Kotlin Grammar for ANTLR v4
 *
 * Based on:
 * jetbrains.github.io/kotlin-spec/#_grammars_and_parsing
 * and
 * kotlinlang.org/docs/reference/grammar.html
 *
 * Tested on
 * github.com/JetBrains/kotlin/tree/master/compiler/testData/psi\
 * (stale link)
 */

// $antlr-format alignTrailingComments true, columnLimit 150, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine true, allowShortBlocksOnASingleLine true, minEmptyLines 0, alignSemicolons ownLine
// $antlr-format alignColons trailing, singleLineOverrulesHangingColon true, alignLexerCommands true, alignLabels true, alignTrailers true

lexer grammar KotlinLexer;

import UnicodeClasses;

ShebangLine: '#!' ~[\u000A\u000D]*;
InvalidShebangLine: OperatorTrivia+ '#!' ~[\u000A\u000D]*;

DelimitedComment: DelimitedCommentText -> channel(HIDDEN);

LineComment: '//' ~[\u000A\u000D]* -> channel(HIDDEN);

WS: [\u0020\u0009\u000C] -> skip;

NL: '\u000D' '\u000A' | '\u000D' | '\u000A';

fragment DelimitedCommentText: '/*' (DelimitedCommentText | .)*? '*/';

fragment OperatorTrivia:
    [\u0020\u0009\u000C]
    | NL
    | '//' ~[\u000A\u000D]*
    | DelimitedCommentText
;

fragment SeparatedExclamation: '!' OperatorTrivia+;

//SEPARATORS & OPERATIONS

RESERVED         : '...';
DOT              : '.';
COMMA            : ',';
LPAREN           : '(' -> pushMode(Inside);
RPAREN           : ')';
LSQUARE          : '[' -> pushMode(Inside);
RSQUARE          : ']';
LCURL            : '{' -> pushMode(DEFAULT_MODE);
RCURL            : '}' -> popMode;
MULT             : '*';
MOD              : '%';
DIV              : '/';
ADD              : '+';
SUB              : '-';
INCR             : '++';
DECR             : '--';
CONJ             : '&&';
AMP              : '&';
DISJ             : '||';
EXCL_EXCL        : '!!';
EXCL_WS          : SeparatedExclamation;
EXCL             : '!';
COLON            : ':';
SEMICOLON        : ';';
ASSIGNMENT       : '=';
ADD_ASSIGNMENT   : '+=';
SUB_ASSIGNMENT   : '-=';
MULT_ASSIGNMENT  : '*=';
DIV_ASSIGNMENT   : '/=';
MOD_ASSIGNMENT   : '%=';
ARROW            : '->';
DOUBLE_ARROW     : '=>';
RANGE_UNTIL      : '..<';
RANGE            : '..';
COLONCOLON       : '::';
Q_COLONCOLON     : '?::';
DOUBLE_SEMICOLON : ';;';
HASH             : '#';
AT               : '@';
SAFE_NAV_WS      : '?' OperatorTrivia+ '.';
QUEST            : '?';
ELVIS            : '?:';
LANGLE           : '<';
RANGLE           : '>';
LE               : '<=';
GE               : '>=';
EXCL_EQ          : '!=';
EXCL_EQEQ        : '!==';
AS_SAFE          : 'as?';
EQEQ             : '==';
EQEQEQ           : '===';
SINGLE_QUOTE     : '\'';

//KEYWORDS

RETURN_AT       : 'return@' Identifier;
CONTINUE_AT     : 'continue@' Identifier;
BREAK_AT        : 'break@' Identifier;

FILE_SITE       : '@file';
ALL_SITE        : '@all';
PACKAGE         : 'package';
IMPORT          : 'import';
CLASS           : 'class';
INTERFACE       : 'interface';
CONTEXT         : 'context';
FUN             : 'fun';
OBJECT          : 'object';
VAL             : 'val';
VAR             : 'var';
TYPE_ALIAS      : 'typealias';
CONSTRUCTOR     : 'constructor';
BY              : 'by';
COMPANION       : 'companion';
INIT            : 'init';
THIS            : 'this';
SUPER           : 'super';
TYPEOF          : 'typeof';
WHERE           : 'where';
IF              : 'if';
ELSE            : 'else';
WHEN            : 'when';
TRY             : 'try';
CATCH           : 'catch';
FINALLY         : 'finally';
FOR             : 'for';
DO              : 'do';
WHILE           : 'while';
THROW           : 'throw';
RETURN          : 'return';
CONTINUE        : 'continue';
BREAK           : 'break';
AS              : 'as';
IS              : 'is';
IN              : 'in';
NOT_IS          : '!is' OperatorTrivia+;
NOT_IN          : '!in' OperatorTrivia+;
OUT             : 'out';
FIELD_SITE      : '@field';
FIELD           : 'field';
PROPERTY_SITE   : '@property';
GET_SITE        : '@get';
SET_SITE        : '@set';
GETTER          : 'get';
SETTER          : 'set';
RECEIVER_SITE   : '@receiver';
PARAM_SITE      : '@param';
SETPARAM_SITE   : '@setparam';
DELEGATE_SITE   : '@delegate';
DYNAMIC         : 'dynamic';

//MODIFIERS

PUBLIC      : 'public';
PRIVATE     : 'private';
PROTECTED   : 'protected';
INTERNAL    : 'internal';
ENUM        : 'enum';
SEALED      : 'sealed';
ANNOTATION  : 'annotation';
DATA        : 'data';
INNER       : 'inner';
TAILREC     : 'tailrec';
OPERATOR    : 'operator';
INLINE      : 'inline';
INFIX       : 'infix';
EXTERNAL    : 'external';
SUSPEND     : 'suspend';
OVERRIDE    : 'override';
ABSTRACT    : 'abstract';
FINAL       : 'final';
OPEN        : 'open';
CONST       : 'const';
LATEINIT    : 'lateinit';
VARARG      : 'vararg';
NOINLINE    : 'noinline';
CROSSINLINE : 'crossinline';
REIFIED     : 'reified';
EXPECT      : 'expect';
ACTUAL      : 'actual';
VALUE       : 'value';

//

QUOTE_OPEN        : '"'   -> pushMode(LineString);
TRIPLE_QUOTE_OPEN : '"""' -> pushMode(MultiLineString);

RealLiteral: FloatLiteral | DoubleLiteral;

FloatLiteral: DoubleLiteral [fF] | DecDigits [fF];

fragment DecDigitOrSeparator: DecDigit | '_';

fragment DecDigits: DecDigit DecDigitOrSeparator* DecDigit | DecDigit;

fragment DoubleExponent: [eE] ('+' | '-')? DecDigits;

DoubleLiteral: DecDigits? '.' DecDigits DoubleExponent? | DecDigits DoubleExponent;

UnsignedLiteral: (IntegerLiteral | HexLiteral | BinLiteral) [uU] 'L'?;

LongLiteral: (IntegerLiteral | HexLiteral | BinLiteral) 'L';

IntegerLiteral: DecDigitNoZero DecDigitOrSeparator* DecDigit | DecDigit;

fragment DecDigit: [0-9];

fragment DecDigitNoZero: [1-9];

fragment UnicodeDigit: UNICODE_CLASS_ND;

HexLiteral: '0' [xX] HexDigit HexDigitOrSeparator* HexDigit | '0' [xX] HexDigit;

fragment HexDigit: [0-9a-fA-F];

fragment HexDigitOrSeparator: HexDigit | '_';

BinLiteral: '0' [bB] BinDigit BinDigitOrSeparator* BinDigit | '0' [bB] BinDigit;

fragment BinDigit: [01];

fragment BinDigitOrSeparator: BinDigit | '_';

BooleanLiteral: 'true' | 'false';

NullLiteral: 'null';

Identifier: (Letter | '_') (Letter | '_' | UnicodeDigit)* | '`' ~('`' | '\u000A' | '\u000D')+ '`';

LabelReference: '@' Identifier;

LabelDefinition: Identifier '@';

FieldIdentifier: '$' Identifier;

CharacterLiteral: '\'' (EscapeSeq | ~('\'' | '\\' | '\u000A' | '\u000D')) '\'';

fragment EscapeSeq: UniCharacterLiteral | EscapedIdentifier;

fragment UniCharacterLiteral: '\\' 'u' HexDigit HexDigit HexDigit HexDigit;

fragment EscapedIdentifier: '\\' ('t' | 'b' | 'r' | 'n' | '\'' | '"' | '\\' | '$');

// Frozen Unicode 12.1.0 categories are generated in UnicodeClasses.g4.
fragment Letter:
    UNICODE_CLASS_LL
    | UNICODE_CLASS_LM
    | UNICODE_CLASS_LO
    | UNICODE_CLASS_LT
    | UNICODE_CLASS_LU
    | UNICODE_CLASS_NL
;

mode Inside;

Inside_RPAREN  : ')' -> popMode, type(RPAREN);
Inside_RSQUARE : ']' -> popMode, type(RSQUARE);

Inside_LPAREN  : LPAREN  -> pushMode(Inside), type(LPAREN);
Inside_LSQUARE : LSQUARE -> pushMode(Inside), type(LSQUARE);

Inside_LCURL            : LCURL            -> pushMode(DEFAULT_MODE), type(LCURL);
Inside_RCURL            : RCURL            -> popMode, type(RCURL);
Inside_DOT              : DOT              -> type(DOT);
Inside_COMMA            : COMMA            -> type(COMMA);
Inside_MULT             : MULT             -> type(MULT);
Inside_MOD              : MOD              -> type(MOD);
Inside_DIV              : DIV              -> type(DIV);
Inside_ADD              : ADD              -> type(ADD);
Inside_SUB              : SUB              -> type(SUB);
Inside_INCR             : INCR             -> type(INCR);
Inside_DECR             : DECR             -> type(DECR);
Inside_EXCL_EXCL        : EXCL_EXCL -> type(EXCL_EXCL);
Inside_EXCL_WS          : SeparatedExclamation -> type(EXCL_WS);
Inside_CONJ             : CONJ             -> type(CONJ);
Inside_AMP              : AMP              -> type(AMP);
Inside_DISJ             : DISJ             -> type(DISJ);
Inside_EXCL             : EXCL             -> type(EXCL);
Inside_COLON            : COLON            -> type(COLON);
Inside_SEMICOLON        : SEMICOLON        -> type(SEMICOLON);
Inside_ASSIGNMENT       : ASSIGNMENT       -> type(ASSIGNMENT);
Inside_ADD_ASSIGNMENT   : ADD_ASSIGNMENT   -> type(ADD_ASSIGNMENT);
Inside_SUB_ASSIGNMENT   : SUB_ASSIGNMENT   -> type(SUB_ASSIGNMENT);
Inside_MULT_ASSIGNMENT  : MULT_ASSIGNMENT  -> type(MULT_ASSIGNMENT);
Inside_DIV_ASSIGNMENT   : DIV_ASSIGNMENT   -> type(DIV_ASSIGNMENT);
Inside_MOD_ASSIGNMENT   : MOD_ASSIGNMENT   -> type(MOD_ASSIGNMENT);
Inside_ARROW            : ARROW            -> type(ARROW);
Inside_DOUBLE_ARROW     : DOUBLE_ARROW     -> type(DOUBLE_ARROW);
Inside_RANGE_UNTIL      : RANGE_UNTIL      -> type(RANGE_UNTIL);
Inside_RANGE            : RANGE            -> type(RANGE);
Inside_RESERVED         : RESERVED         -> type(RESERVED);
Inside_COLONCOLON       : COLONCOLON       -> type(COLONCOLON);
Inside_Q_COLONCOLON     : Q_COLONCOLON     -> type(Q_COLONCOLON);
Inside_DOUBLE_SEMICOLON : DOUBLE_SEMICOLON -> type(DOUBLE_SEMICOLON);
Inside_HASH             : HASH             -> type(HASH);
Inside_AT               : AT               -> type(AT);
Inside_SAFE_NAV_WS      : SAFE_NAV_WS      -> type(SAFE_NAV_WS);
Inside_QUEST            : QUEST            -> type(QUEST);
Inside_ELVIS            : ELVIS            -> type(ELVIS);
Inside_LANGLE           : LANGLE           -> type(LANGLE);
Inside_RANGLE           : RANGLE           -> type(RANGLE);
Inside_LE               : LE               -> type(LE);
Inside_GE               : GE               -> type(GE);
Inside_EXCL_EQ          : EXCL_EQ          -> type(EXCL_EQ);
Inside_EXCL_EQEQ        : EXCL_EQEQ        -> type(EXCL_EQEQ);
Inside_NOT_IS           : NOT_IS           -> type(NOT_IS);
Inside_NOT_IN           : NOT_IN           -> type(NOT_IN);
Inside_AS_SAFE          : AS_SAFE          -> type(AS_SAFE);
Inside_EQEQ             : EQEQ             -> type(EQEQ);
Inside_EQEQEQ           : EQEQEQ           -> type(EQEQEQ);
Inside_SINGLE_QUOTE     : SINGLE_QUOTE     -> type(SINGLE_QUOTE);
Inside_QUOTE_OPEN       : QUOTE_OPEN       -> pushMode(LineString), type(QUOTE_OPEN);
Inside_TRIPLE_QUOTE_OPEN:
    TRIPLE_QUOTE_OPEN -> pushMode(MultiLineString), type(TRIPLE_QUOTE_OPEN)
;

Inside_RETURN_AT     : RETURN_AT     -> type(RETURN_AT);
Inside_CONTINUE_AT   : CONTINUE_AT   -> type(CONTINUE_AT);
Inside_BREAK_AT      : BREAK_AT      -> type(BREAK_AT);
Inside_FILE          : FILE_SITE     -> type(FILE_SITE);
Inside_ALL_SITE      : ALL_SITE      -> type(ALL_SITE);
Inside_PACKAGE       : PACKAGE       -> type(PACKAGE);
Inside_IMPORT        : IMPORT        -> type(IMPORT);
Inside_CLASS         : CLASS         -> type(CLASS);
Inside_INTERFACE     : INTERFACE     -> type(INTERFACE);
Inside_CONTEXT       : CONTEXT       -> type(CONTEXT);
Inside_FUN           : FUN           -> type(FUN);
Inside_OBJECT        : OBJECT        -> type(OBJECT);
Inside_VAL           : VAL           -> type(VAL);
Inside_VAR           : VAR           -> type(VAR);
Inside_TYPE_ALIAS    : TYPE_ALIAS    -> type(TYPE_ALIAS);
Inside_CONSTRUCTOR   : CONSTRUCTOR   -> type(CONSTRUCTOR);
Inside_BY            : BY            -> type(BY);
Inside_COMPANION     : COMPANION     -> type(COMPANION);
Inside_INIT          : INIT          -> type(INIT);
Inside_THIS          : THIS          -> type(THIS);
Inside_SUPER         : SUPER         -> type(SUPER);
Inside_TYPEOF        : TYPEOF        -> type(TYPEOF);
Inside_WHERE         : WHERE         -> type(WHERE);
Inside_IF            : IF            -> type(IF);
Inside_ELSE          : ELSE          -> type(ELSE);
Inside_WHEN          : WHEN          -> type(WHEN);
Inside_TRY           : TRY           -> type(TRY);
Inside_CATCH         : CATCH         -> type(CATCH);
Inside_FINALLY       : FINALLY       -> type(FINALLY);
Inside_FOR           : FOR           -> type(FOR);
Inside_DO            : DO            -> type(DO);
Inside_WHILE         : WHILE         -> type(WHILE);
Inside_THROW         : THROW         -> type(THROW);
Inside_RETURN        : RETURN        -> type(RETURN);
Inside_CONTINUE      : CONTINUE      -> type(CONTINUE);
Inside_BREAK         : BREAK         -> type(BREAK);
Inside_AS            : AS            -> type(AS);
Inside_IS            : IS            -> type(IS);
Inside_IN            : IN            -> type(IN);
Inside_OUT           : OUT           -> type(OUT);
Inside_FIELD_SITE    : FIELD_SITE    -> type(FIELD_SITE);
Inside_FIELD         : FIELD         -> type(FIELD);
Inside_PROPERTY      : PROPERTY_SITE -> type(PROPERTY_SITE);
Inside_GET           : GET_SITE      -> type(GET_SITE);
Inside_SET           : SET_SITE      -> type(SET_SITE);
Inside_GETTER        : GETTER        -> type(GETTER);
Inside_SETTER        : SETTER        -> type(SETTER);
Inside_RECEIVER      : RECEIVER_SITE -> type(RECEIVER_SITE);
Inside_PARAM         : PARAM_SITE    -> type(PARAM_SITE);
Inside_SETPARAM      : SETPARAM_SITE -> type(SETPARAM_SITE);
Inside_DELEGATE      : DELEGATE_SITE -> type(DELEGATE_SITE);
Inside_DYNAMIC       : DYNAMIC       -> type(DYNAMIC);

Inside_PUBLIC      : PUBLIC      -> type(PUBLIC);
Inside_PRIVATE     : PRIVATE     -> type(PRIVATE);
Inside_PROTECTED   : PROTECTED   -> type(PROTECTED);
Inside_INTERNAL    : INTERNAL    -> type(INTERNAL);
Inside_ENUM        : ENUM        -> type(ENUM);
Inside_SEALED      : SEALED      -> type(SEALED);
Inside_ANNOTATION  : ANNOTATION  -> type(ANNOTATION);
Inside_DATA        : DATA        -> type(DATA);
Inside_INNER       : INNER       -> type(INNER);
Inside_TAILREC     : TAILREC     -> type(TAILREC);
Inside_OPERATOR    : OPERATOR    -> type(OPERATOR);
Inside_INLINE      : INLINE      -> type(INLINE);
Inside_INFIX       : INFIX       -> type(INFIX);
Inside_EXTERNAL    : EXTERNAL    -> type(EXTERNAL);
Inside_SUSPEND     : SUSPEND     -> type(SUSPEND);
Inside_OVERRIDE    : OVERRIDE    -> type(OVERRIDE);
Inside_ABSTRACT    : ABSTRACT    -> type(ABSTRACT);
Inside_FINAL       : FINAL       -> type(FINAL);
Inside_OPEN        : OPEN        -> type(OPEN);
Inside_CONST       : CONST       -> type(CONST);
Inside_LATEINIT    : LATEINIT    -> type(LATEINIT);
Inside_VARARG      : VARARG      -> type(VARARG);
Inside_NOINLINE    : NOINLINE    -> type(NOINLINE);
Inside_CROSSINLINE : CROSSINLINE -> type(CROSSINLINE);
Inside_REIFIED     : REIFIED     -> type(REIFIED);
Inside_EXPECT      : EXPECT      -> type(EXPECT);
Inside_ACTUAL      : ACTUAL      -> type(ACTUAL);
Inside_VALUE       : VALUE       -> type(VALUE);

Inside_BooleanLiteral   : BooleanLiteral   -> type(BooleanLiteral);
Inside_IntegerLiteral   : IntegerLiteral   -> type(IntegerLiteral);
Inside_HexLiteral       : HexLiteral       -> type(HexLiteral);
Inside_BinLiteral       : BinLiteral       -> type(BinLiteral);
Inside_CharacterLiteral : CharacterLiteral -> type(CharacterLiteral);
Inside_RealLiteral      : RealLiteral      -> type(RealLiteral);
Inside_NullLiteral      : NullLiteral      -> type(NullLiteral);

Inside_UnsignedLiteral: UnsignedLiteral -> type(UnsignedLiteral);
Inside_LongLiteral: LongLiteral -> type(LongLiteral);

Inside_Identifier      : Identifier                       -> type(Identifier);
Inside_LabelReference  : LabelReference                   -> type(LabelReference);
Inside_LabelDefinition : LabelDefinition                  -> type(LabelDefinition);
Inside_Comment         : (LineComment | DelimitedComment) -> channel(HIDDEN);
Inside_WS              : WS                               -> skip;
Inside_NL              : NL                               -> skip;

mode LineString;

QUOTE_CLOSE: '"' -> popMode;

LineStrRef: FieldIdentifier;

LineStrText: ~('\\' | '"' | '$' | '\u000A' | '\u000D')+ | '$';

LineStrEscapedChar: EscapeSeq;

LineStrExprStart: '${' -> pushMode(StringExpression);

mode MultiLineString;

TRIPLE_QUOTE_CLOSE: MultiLineStringQuote? '"""' -> popMode;

MultiLineStringQuote: '"'+;

MultiLineStrRef: FieldIdentifier;

MultiLineStrText: ~('"' | '$')+ | '$';

MultiLineStrExprStart: '${' -> pushMode(StringExpression);

MultiLineNL: NL -> skip;

mode StringExpression;

StrExpr_RCURL: RCURL -> popMode, type(RCURL);

StrExpr_LPAREN  : LPAREN  -> pushMode(Inside), type(LPAREN);
StrExpr_LSQUARE : LSQUARE -> pushMode(Inside), type(LSQUARE);

StrExpr_RPAREN           : ')'              -> type(RPAREN);
StrExpr_RSQUARE          : ']'              -> type(RSQUARE);
StrExpr_LCURL            : LCURL            -> pushMode(DEFAULT_MODE), type(LCURL);
StrExpr_DOT              : DOT              -> type(DOT);
StrExpr_COMMA            : COMMA            -> type(COMMA);
StrExpr_MULT             : MULT             -> type(MULT);
StrExpr_MOD              : MOD              -> type(MOD);
StrExpr_DIV              : DIV              -> type(DIV);
StrExpr_ADD              : ADD              -> type(ADD);
StrExpr_SUB              : SUB              -> type(SUB);
StrExpr_INCR             : INCR             -> type(INCR);
StrExpr_DECR             : DECR             -> type(DECR);
StrExpr_EXCL_EXCL        : EXCL_EXCL -> type(EXCL_EXCL);
StrExpr_EXCL_WS          : SeparatedExclamation -> type(EXCL_WS);
StrExpr_CONJ             : CONJ             -> type(CONJ);
StrExpr_AMP              : AMP              -> type(AMP);
StrExpr_DISJ             : DISJ             -> type(DISJ);
StrExpr_EXCL             : EXCL             -> type(EXCL);
StrExpr_COLON            : COLON            -> type(COLON);
StrExpr_SEMICOLON        : SEMICOLON        -> type(SEMICOLON);
StrExpr_ASSIGNMENT       : ASSIGNMENT       -> type(ASSIGNMENT);
StrExpr_ADD_ASSIGNMENT   : ADD_ASSIGNMENT   -> type(ADD_ASSIGNMENT);
StrExpr_SUB_ASSIGNMENT   : SUB_ASSIGNMENT   -> type(SUB_ASSIGNMENT);
StrExpr_MULT_ASSIGNMENT  : MULT_ASSIGNMENT  -> type(MULT_ASSIGNMENT);
StrExpr_DIV_ASSIGNMENT   : DIV_ASSIGNMENT   -> type(DIV_ASSIGNMENT);
StrExpr_MOD_ASSIGNMENT   : MOD_ASSIGNMENT   -> type(MOD_ASSIGNMENT);
StrExpr_ARROW            : ARROW            -> type(ARROW);
StrExpr_DOUBLE_ARROW     : DOUBLE_ARROW     -> type(DOUBLE_ARROW);
StrExpr_RANGE_UNTIL      : RANGE_UNTIL      -> type(RANGE_UNTIL);
StrExpr_RANGE            : RANGE            -> type(RANGE);
StrExpr_COLONCOLON       : COLONCOLON       -> type(COLONCOLON);
StrExpr_Q_COLONCOLON     : Q_COLONCOLON     -> type(Q_COLONCOLON);
StrExpr_DOUBLE_SEMICOLON : DOUBLE_SEMICOLON -> type(DOUBLE_SEMICOLON);
StrExpr_HASH             : HASH             -> type(HASH);
StrExpr_AT               : AT               -> type(AT);
StrExpr_SAFE_NAV_WS      : SAFE_NAV_WS      -> type(SAFE_NAV_WS);
StrExpr_QUEST            : QUEST            -> type(QUEST);
StrExpr_ELVIS            : ELVIS            -> type(ELVIS);
StrExpr_LANGLE           : LANGLE           -> type(LANGLE);
StrExpr_RANGLE           : RANGLE           -> type(RANGLE);
StrExpr_LE               : LE               -> type(LE);
StrExpr_GE               : GE               -> type(GE);
StrExpr_EXCL_EQ          : EXCL_EQ          -> type(EXCL_EQ);
StrExpr_EXCL_EQEQ        : EXCL_EQEQ        -> type(EXCL_EQEQ);
StrExpr_AS               : AS               -> type(AS);
StrExpr_IS               : IS               -> type(IS);
StrExpr_IN               : IN               -> type(IN);
StrExpr_NOT_IS           : NOT_IS           -> type(NOT_IS);
StrExpr_NOT_IN           : NOT_IN           -> type(NOT_IN);
StrExpr_AS_SAFE          : AS_SAFE          -> type(AS_SAFE);
StrExpr_EQEQ             : EQEQ             -> type(EQEQ);
StrExpr_EQEQEQ           : EQEQEQ           -> type(EQEQEQ);
StrExpr_SINGLE_QUOTE     : SINGLE_QUOTE     -> type(SINGLE_QUOTE);
StrExpr_QUOTE_OPEN       : QUOTE_OPEN   -> pushMode(LineString), type(QUOTE_OPEN);
StrExpr_TRIPLE_QUOTE_OPEN:
    TRIPLE_QUOTE_OPEN -> pushMode(MultiLineString), type(TRIPLE_QUOTE_OPEN)
;

StrExpr_RETURN_AT     : RETURN_AT     -> type(RETURN_AT);
StrExpr_CONTINUE_AT   : CONTINUE_AT   -> type(CONTINUE_AT);
StrExpr_BREAK_AT      : BREAK_AT      -> type(BREAK_AT);
StrExpr_FILE          : FILE_SITE     -> type(FILE_SITE);
StrExpr_ALL_SITE      : ALL_SITE      -> type(ALL_SITE);
StrExpr_PACKAGE       : PACKAGE       -> type(PACKAGE);
StrExpr_IMPORT        : IMPORT        -> type(IMPORT);
StrExpr_CLASS         : CLASS         -> type(CLASS);
StrExpr_INTERFACE     : INTERFACE     -> type(INTERFACE);
StrExpr_CONTEXT       : CONTEXT       -> type(CONTEXT);
StrExpr_FUN           : FUN           -> type(FUN);
StrExpr_OBJECT        : OBJECT        -> type(OBJECT);
StrExpr_VAL           : VAL           -> type(VAL);
StrExpr_VAR           : VAR           -> type(VAR);
StrExpr_TYPE_ALIAS    : TYPE_ALIAS    -> type(TYPE_ALIAS);
StrExpr_CONSTRUCTOR   : CONSTRUCTOR   -> type(CONSTRUCTOR);
StrExpr_BY            : BY            -> type(BY);
StrExpr_COMPANION     : COMPANION     -> type(COMPANION);
StrExpr_INIT          : INIT          -> type(INIT);
StrExpr_THIS          : THIS          -> type(THIS);
StrExpr_SUPER         : SUPER         -> type(SUPER);
StrExpr_TYPEOF        : TYPEOF        -> type(TYPEOF);
StrExpr_WHERE         : WHERE         -> type(WHERE);
StrExpr_IF            : IF            -> type(IF);
StrExpr_ELSE          : ELSE          -> type(ELSE);
StrExpr_WHEN          : WHEN          -> type(WHEN);
StrExpr_TRY           : TRY           -> type(TRY);
StrExpr_CATCH         : CATCH         -> type(CATCH);
StrExpr_FINALLY       : FINALLY       -> type(FINALLY);
StrExpr_FOR           : FOR           -> type(FOR);
StrExpr_DO            : DO            -> type(DO);
StrExpr_WHILE         : WHILE         -> type(WHILE);
StrExpr_THROW         : THROW         -> type(THROW);
StrExpr_RETURN        : RETURN        -> type(RETURN);
StrExpr_CONTINUE      : CONTINUE      -> type(CONTINUE);
StrExpr_BREAK         : BREAK         -> type(BREAK);
StrExpr_OUT           : OUT           -> type(OUT);
StrExpr_FIELD_SITE    : FIELD_SITE    -> type(FIELD_SITE);
StrExpr_FIELD         : FIELD         -> type(FIELD);
StrExpr_PROPERTY      : PROPERTY_SITE -> type(PROPERTY_SITE);
StrExpr_GET           : GET_SITE      -> type(GET_SITE);
StrExpr_SET           : SET_SITE      -> type(SET_SITE);
StrExpr_GETTER        : GETTER        -> type(GETTER);
StrExpr_SETTER        : SETTER        -> type(SETTER);
StrExpr_RECEIVER      : RECEIVER_SITE -> type(RECEIVER_SITE);
StrExpr_PARAM         : PARAM_SITE    -> type(PARAM_SITE);
StrExpr_SETPARAM      : SETPARAM_SITE -> type(SETPARAM_SITE);
StrExpr_DELEGATE      : DELEGATE_SITE -> type(DELEGATE_SITE);
StrExpr_DYNAMIC       : DYNAMIC       -> type(DYNAMIC);
StrExpr_PUBLIC        : PUBLIC        -> type(PUBLIC);
StrExpr_PRIVATE       : PRIVATE       -> type(PRIVATE);
StrExpr_PROTECTED     : PROTECTED     -> type(PROTECTED);
StrExpr_INTERNAL      : INTERNAL      -> type(INTERNAL);
StrExpr_ENUM          : ENUM          -> type(ENUM);
StrExpr_SEALED        : SEALED        -> type(SEALED);
StrExpr_ANNOTATION    : ANNOTATION    -> type(ANNOTATION);
StrExpr_DATA          : DATA          -> type(DATA);
StrExpr_INNER         : INNER         -> type(INNER);
StrExpr_TAILREC       : TAILREC       -> type(TAILREC);
StrExpr_OPERATOR      : OPERATOR      -> type(OPERATOR);
StrExpr_INLINE        : INLINE        -> type(INLINE);
StrExpr_INFIX         : INFIX         -> type(INFIX);
StrExpr_EXTERNAL      : EXTERNAL      -> type(EXTERNAL);
StrExpr_SUSPEND       : SUSPEND       -> type(SUSPEND);
StrExpr_OVERRIDE      : OVERRIDE      -> type(OVERRIDE);
StrExpr_ABSTRACT      : ABSTRACT      -> type(ABSTRACT);
StrExpr_FINAL         : FINAL         -> type(FINAL);
StrExpr_OPEN          : OPEN          -> type(OPEN);
StrExpr_CONST         : CONST         -> type(CONST);
StrExpr_LATEINIT      : LATEINIT      -> type(LATEINIT);
StrExpr_VARARG        : VARARG        -> type(VARARG);
StrExpr_NOINLINE      : NOINLINE      -> type(NOINLINE);
StrExpr_CROSSINLINE   : CROSSINLINE   -> type(CROSSINLINE);
StrExpr_REIFIED       : REIFIED       -> type(REIFIED);
StrExpr_EXPECT        : EXPECT        -> type(EXPECT);
StrExpr_ACTUAL        : ACTUAL        -> type(ACTUAL);
StrExpr_VALUE         : VALUE         -> type(VALUE);

StrExpr_BooleanLiteral   : BooleanLiteral   -> type(BooleanLiteral);
StrExpr_IntegerLiteral   : IntegerLiteral   -> type(IntegerLiteral);
StrExpr_HexLiteral       : HexLiteral       -> type(HexLiteral);
StrExpr_BinLiteral       : BinLiteral       -> type(BinLiteral);
StrExpr_CharacterLiteral : CharacterLiteral -> type(CharacterLiteral);
StrExpr_RealLiteral      : RealLiteral      -> type(RealLiteral);
StrExpr_NullLiteral      : NullLiteral      -> type(NullLiteral);
StrExpr_UnsignedLiteral  : UnsignedLiteral  -> type(UnsignedLiteral);
StrExpr_LongLiteral      : LongLiteral      -> type(LongLiteral);

StrExpr_Identifier      : Identifier                       -> type(Identifier);
StrExpr_LabelReference  : LabelReference                   -> type(LabelReference);
StrExpr_LabelDefinition : LabelDefinition                  -> type(LabelDefinition);
StrExpr_Comment         : (LineComment | DelimitedComment) -> channel(HIDDEN);
StrExpr_WS              : WS                               -> skip;
StrExpr_NL              : NL                               -> skip;