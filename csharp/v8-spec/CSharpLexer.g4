lexer grammar CSharpLexer;

options { superClass=CSharpLexerBase; }

channels { DIRECTIVE }

// Insert here @header for lexer.

// Source: §6.3.0 Extracted literal tokens
TK : '_' ;
TK_MINUS : '-' ;
TK_MINUS_MINUS : '--' ;
TK_MINUS_EQ : '-=' ;
TK_MINUS_GT : '->' ;
TK_COMMA : ',' ;
TK_SEMI : ';' ;
TK_COLON : ':' ;
TK_COLON_COLON : '::' ;
TK_NOT : '!' ;
TK_NOT_EQ : '!=' ;
TK_QMARK : '?' ;
TK_QMARK_QMARK : '??' ;
TK_QMARK_QMARK_EQ : '??=' ;
TK_DOT : '.' ;
TK_DOT_DOT : '..' ;
// ║ Previous:
// ║ 
// ║ TK_LPAREN : '(' ;
TK_LPAREN : '(' -> pushMode(DEFAULT_MODE) ;

// ║ Previous:
// ║ 
// ║ TK_RPAREN : ')' ;
TK_RPAREN : ')' -> popMode ;

// ║ Previous:
// ║ 
// ║ TK_LBRACK : '[' ;
TK_LBRACK : '[' -> pushMode(DEFAULT_MODE) ;

// ║ Previous:
// ║ 
// ║ TK_RBRACK : ']' ;
TK_RBRACK : ']' -> popMode ;

// ║ Previous:
// ║ 
// ║ TK_LBRACE : '{' ;
TK_LBRACE : '{' -> pushMode(DEFAULT_MODE) ;

// ║ Previous:
// ║ 
// ║ TK_RBRACE : '}' ;
TK_RBRACE : '}' -> popMode ;

TK_MUL_EQ : '*=' ;
TK_DIV_EQ : '/=' ;
TK_AND : '&' ;
TK_AND_AND : '&&' ;
TK_AND_EQ : '&=' ;
TK_PCT : '%' ;
TK_PCT_EQ : '%=' ;
TK_XOR : '^' ;
TK_XOR_EQ : '^=' ;
TK_PLUS : '+' ;
TK_PLUS_PLUS : '++' ;
TK_PLUS_EQ : '+=' ;
TK_LT : '<' ;
TK_LT_LT : '<<' ;
TK_LT_LT_EQ : '<<=' ;
TK_LT_EQ : '<=' ;
TK_EQ : '=' ;
TK_EQ_EQ : '==' ;
TK_EQ_GT : '=>' ;
TK_GT : '>' ;
TK_GT_EQ : '>=' ;
TK_OR : '|' ;
TK_OR_EQ : '|=' ;
TK_OR_OR : '||' ;
TK_INV : '~' ;
KW_ABSTRACT : 'abstract' ;
KW_ADD : 'add' ;
KW_ALIAS : 'alias' ;
KW_ARGLIST : '__arglist' ;
KW_AS : 'as' ;
KW_ASCENDING : 'ascending' ;
KW_ASYNC : 'async' ;
KW_AWAIT : 'await' ;
KW_BASE : 'base' ;
KW_BOOL : 'bool' ;
KW_BREAK : 'break' ;
KW_BY : 'by' ;
KW_BYTE : 'byte' ;
KW_CASE : 'case' ;
KW_CATCH : 'catch' ;
KW_CHAR : 'char' ;
KW_CHECKED : 'checked' ;
KW_CLASS : 'class' ;
KW_CONST : 'const' ;
KW_CONTINUE : 'continue' ;
KW_DECIMAL : 'decimal' ;
KW_DELEGATE : 'delegate' ;
KW_DESCENDING : 'descending' ;
KW_DO : 'do' ;
KW_DOUBLE : 'double' ;
KW_DYNAMIC : 'dynamic' ;
KW_ELSE : 'else' ;
KW_ENUM : 'enum' ;
KW_EQUALS : 'equals' ;
KW_EVENT : 'event' ;
KW_EXPLICIT : 'explicit' ;
KW_EXTERN : 'extern' ;
KW_FINALLY : 'finally' ;
KW_FIXED : 'fixed' ;
KW_FLOAT : 'float' ;
KW_FOR : 'for' ;
KW_FOREACH : 'foreach' ;
KW_FROM : 'from' ;
KW_GET : 'get' ;
KW_GLOBAL : 'global' ;
KW_GOTO : 'goto' ;
KW_GROUP : 'group' ;
KW_IF : 'if' ;
KW_IMPLICIT : 'implicit' ;
KW_IN : 'in' ;
KW_INT : 'int' ;
KW_INTERFACE : 'interface' ;
KW_INTERNAL : 'internal' ;
KW_INTO : 'into' ;
KW_IS : 'is' ;
KW_JOIN : 'join' ;
KW_LET : 'let' ;
KW_LOCK : 'lock' ;
KW_LONG : 'long' ;
KW_MAKEREF : '__makeref' ; // MS extension
KW_NAMEOF : 'nameof' ;
KW_NAMESPACE : 'namespace' ;
KW_NEW : 'new' ;
KW_NOTNULL : 'notnull' ;
KW_OBJECT : 'object' ;
KW_ON : 'on' ;
KW_OPERATOR : 'operator' ;
KW_ORDERBY : 'orderby' ;
KW_OUT : 'out' ;
KW_OVERRIDE : 'override' ;
KW_PARAMS : 'params' ;
KW_PARTIAL : 'partial' ;
KW_PRIVATE : 'private' ;
KW_PROTECTED : 'protected' ;
KW_PUBLIC : 'public' ;
KW_READONLY : 'readonly' ;
KW_REF : 'ref' ;
KW_REFTYPE  : '__reftype'  ; // MS extension
KW_REFVALUE : '__refvalue' ; // MS extension
KW_REMOVE : 'remove' ;
KW_RETURN : 'return' ;
KW_SBYTE : 'sbyte' ;
KW_SEALED : 'sealed' ;
KW_SELECT : 'select' ;
KW_SET : 'set' ;
KW_SHORT : 'short' ;
KW_SIZEOF : 'sizeof' ;
KW_STACKALLOC : 'stackalloc' ;
KW_STATIC : 'static' ;
KW_STRING : 'string' ;
KW_STRUCT : 'struct' ;
KW_SWITCH : 'switch' ;
KW_THIS : 'this' ;
KW_THROW : 'throw' ;
KW_TRY : 'try' ;
KW_TYPEOF : 'typeof' ;
KW_UINT : 'uint' ;
KW_ULONG : 'ulong' ;
KW_UNCHECKED : 'unchecked' ;
KW_UNMANAGED : 'unmanaged' ;
KW_UNSAFE : 'unsafe' ;
KW_USHORT : 'ushort' ;
KW_USING : 'using' ;
KW_VALUE : 'value' ;
KW_VAR : 'var' ;
KW_VIRTUAL : 'virtual' ;
KW_VOID : 'void' ;
KW_VOLATILE : 'volatile' ;
KW_WHEN : 'when' ;
KW_WHERE : 'where' ;
KW_WHILE : 'while' ;
KW_YIELD : 'yield' ;
// Source: §6.3.1 General
DEFAULT  : 'default' ;
NULL     : 'null' ;
TRUE     : 'true' ;
FALSE    : 'false' ;
ASTERISK : '*' ;
SLASH    : '/' ;
// Source: §6.3.2 Line terminators
// ║ Previous:
// ║ 
// ║ New_Line
// ║     : New_Line_Character
// ║     | '\u000D\u000A'    // carriage return, line feed
// ║     ;
// ║ 
// [SKIP]
New_Line
    : ( New_Line_Character
      | '\u000D\u000A'    // carriage return, line feed
      ) -> skip
    ;

// Source: §6.3.3 Comments
// ║ Previous:
// ║ 
// ║ Comment
// ║     : Single_Line_Comment
// ║     | Delimited_Comment
// ║     ;
// ║ 
// [SKIP]
Comment
    : ( Single_Line_Comment
      | Delimited_Comment
      ) -> skip
    ;

fragment Single_Line_Comment
    : '//' Input_Character*
    ;

fragment Input_Character
    // anything but New_Line_Character
    : ~('\u000D' | '\u000A'   | '\u0085' | '\u2028' | '\u2029')
    ;

fragment New_Line_Character
    : '\u000D'  // carriage return
    | '\u000A'  // line feed
    | '\u0085'  // next line
    | '\u2028'  // line separator
    | '\u2029'  // paragraph separator
    ;

fragment Delimited_Comment
    : '/*' Delimited_Comment_Section* ASTERISK+ '/'
    ;

fragment Delimited_Comment_Section
    : SLASH
    | ASTERISK* Not_Slash_Or_Asterisk
    ;

fragment Not_Slash_Or_Asterisk
    : ~('/' | '*')    // Any except SLASH or ASTERISK
    ;

// Source: §6.3.4 White space
// ║ Previous:
// ║ 
// ║ Whitespace
// ║     : [\p{Zs}]  // any character with Unicode class Zs
// ║     | '\u0009'  // horizontal tab
// ║     | '\u000B'  // vertical tab
// ║     | '\u000C'  // form feed
// ║     ;
// ║ 
// [SKIP]
Whitespace
    : ( [\p{Zs}]  // any character with Unicode class Zs
      | '\u0009'  // horizontal tab
      | '\u000B'  // vertical tab
      | '\u000C'  // form feed
      ) -> skip
    ;

// Source: §6.4.2 Unicode character escape sequences
fragment Unicode_Escape_Sequence
    : '\\u' Hex_Digit Hex_Digit Hex_Digit Hex_Digit
    | '\\U' Hex_Digit Hex_Digit Hex_Digit Hex_Digit
            Hex_Digit Hex_Digit Hex_Digit Hex_Digit
    ;

// Source: §6.4.3 Identifiers
Simple_Identifier
    : Available_Identifier
    | Escaped_Identifier
    ;

fragment Available_Identifier
    // excluding keywords or contextual keywords, see note below
    : Basic_Identifier
    ;

fragment Escaped_Identifier
    // Includes keywords and contextual keywords prefixed by '@'.
    // See note below.
    : '@' Basic_Identifier
    ;

fragment Basic_Identifier
    : Identifier_Start_Character Identifier_Part_Character*
    ;

fragment Identifier_Start_Character
    : Letter_Character
    | Underscore_Character
    ;

fragment Underscore_Character
    : '_'               // underscore
    | '\\u005' [fF]     // Unicode_Escape_Sequence for underscore
    | '\\U0000005' [fF] // Unicode_Escape_Sequence for underscore
    ;

fragment Identifier_Part_Character
    : Letter_Character
    | Decimal_Digit_Character
    | Connecting_Character
    | Combining_Character
    | Formatting_Character
    ;

fragment Letter_Character
    // Category Letter, all subcategories; category Number, subcategory letter.
    : [\p{L}\p{Nl}]
    // Only escapes for categories L & Nl allowed. See note below.
    | Unicode_Escape_Sequence
    ;

fragment Combining_Character
    // Category Mark, subcategories non-spacing and spacing combining.
    : [\p{Mn}\p{Mc}]
    // Only escapes for categories Mn & Mc allowed. See note below.
    | Unicode_Escape_Sequence
    ;

fragment Decimal_Digit_Character
    // Category Number, subcategory decimal digit.
    : [\p{Nd}]
    // Only escapes for category Nd allowed. See note below.
    | Unicode_Escape_Sequence
    ;

fragment Connecting_Character
    // Category Punctuation, subcategory connector.
    : [\p{Pc}]
    // Only escapes for category Pc allowed. See note below.
    | Unicode_Escape_Sequence
    ;

fragment Formatting_Character
    // Category Other, subcategory format.
    : [\p{Cf}]
    // Only escapes for category Cf allowed, see note below.
    | Unicode_Escape_Sequence
    ;

// Source: §6.4.5.3 Integer literals
Integer_Literal
    : Decimal_Integer_Literal
    | Hexadecimal_Integer_Literal
    | Binary_Integer_Literal
    ;

fragment Decimal_Integer_Literal
    : Decimal_Digit Decorated_Decimal_Digit* Integer_Type_Suffix?
    ;

fragment Decorated_Decimal_Digit
    : '_'* Decimal_Digit
    ;

fragment Decimal_Digit
    : '0'..'9'
    ;

fragment Integer_Type_Suffix
    : 'U' | 'u' | 'L' | 'l' |
      'UL' | 'Ul' | 'uL' | 'ul' | 'LU' | 'Lu' | 'lU' | 'lu'
    ;

fragment Hexadecimal_Integer_Literal
    : ('0x' | '0X') Decorated_Hex_Digit+ Integer_Type_Suffix?
    ;

fragment Decorated_Hex_Digit
    : '_'* Hex_Digit
    ;

fragment Hex_Digit
    : '0'..'9' | 'A'..'F' | 'a'..'f'
    ;

fragment Binary_Integer_Literal
    : ('0b' | '0B') Decorated_Binary_Digit+ Integer_Type_Suffix?
    ;

fragment Decorated_Binary_Digit
    : '_'* Binary_Digit
    ;

fragment Binary_Digit
    : '0' | '1'
    ;

// Source: §6.4.5.4 Real literals
Real_Literal
    : Decimal_Digit Decorated_Decimal_Digit* '.'
      Decimal_Digit Decorated_Decimal_Digit* Exponent_Part? Real_Type_Suffix?
    | '.' Decimal_Digit Decorated_Decimal_Digit* Exponent_Part? Real_Type_Suffix?
    | Decimal_Digit Decorated_Decimal_Digit* Exponent_Part Real_Type_Suffix?
    | Decimal_Digit Decorated_Decimal_Digit* Real_Type_Suffix
    ;

fragment Exponent_Part
    : ('e' | 'E') Sign? Decimal_Digit Decorated_Decimal_Digit*
    ;

fragment Sign
    : '+' | '-'
    ;

fragment Real_Type_Suffix
    : 'F' | 'f' | 'D' | 'd' | 'M' | 'm'
    ;

// Source: §6.4.5.5 Character literals
Character_Literal
    : '\'' Character '\''
    ;

fragment Character
    : Single_Character
    | Simple_Escape_Sequence
    | Hexadecimal_Escape_Sequence
    | Unicode_Escape_Sequence
    ;

fragment Single_Character
    // anything but ', \, and New_Line_Character
    : ~['\\\u000D\u000A\u0085\u2028\u2029]
    ;

fragment Simple_Escape_Sequence
    : '\\\'' | '\\"' | '\\\\' | '\\0' | '\\a' | '\\b' |
      '\\f' | '\\n' | '\\r' | '\\t' | '\\v'
    ;

fragment Hexadecimal_Escape_Sequence
    : '\\x' Hex_Digit Hex_Digit? Hex_Digit? Hex_Digit?
    ;

// Source: §6.4.5.6 String literals
String_Literal
    : Regular_String_Literal
    | Verbatim_String_Literal
    ;

fragment Regular_String_Literal
    : '"' Regular_String_Literal_Character* '"'
    ;

fragment Regular_String_Literal_Character
    : Single_Regular_String_Literal_Character
    | Simple_Escape_Sequence
    | Hexadecimal_Escape_Sequence
    | Unicode_Escape_Sequence
    ;

fragment Single_Regular_String_Literal_Character
    // anything but ", \, and New_Line_Character
    : ~["\\\u000D\u000A\u0085\u2028\u2029]
    ;

fragment Verbatim_String_Literal
    : '@"' Verbatim_String_Literal_Character* '"'
    ;

fragment Verbatim_String_Literal_Character
    : Single_Verbatim_String_Literal_Character
    | Quote_Escape_Sequence
    ;

fragment Single_Verbatim_String_Literal_Character
    : ~["]     // anything but quotation mark (U+0022)
    ;

fragment Quote_Escape_Sequence
    : '""'
    ;

// Source: §6.5.1 General
// Preprocessor directives: '#' switches to DIRECTIVE_MODE where each token is
// placed on the DIRECTIVE channel and consumed by CSharpLexerBase.NextToken().
SHARP : '#' -> mode(DIRECTIVE_MODE), skip ;

// Synthetic token emitted by CSharpLexerBase.SkipFalseBlock() for false #if
// blocks; never matched from input.
SKIPPED_SECTION : '\u0000' -> channel(HIDDEN) ;

// Source: §12.8.3 Interpolated string expressions
// ║ Previous:
// ║ 
// ║ Interpolated_Regular_String_Start
// ║     : '$"'
// ║     ;
// ║ 
Interpolated_Regular_String_Start
    : '$"' { this.WrapToken(); } -> pushMode(IRS_CONT)
    ;

mode IRS_CONT;

// ║ Previous:
// ║ 
// ║ // the following three lexical rules are context sensitive, see details below
// ║ 
// ║ 
// ║ Interpolated_Regular_String_Mid
// ║     : Interpolated_Regular_String_Element+
// ║     ;
// ║ 
Interpolated_Regular_String_Mid
    : Interpolated_Regular_String_Element+ { this.WrapToken(); }
    ;

mode DEFAULT_MODE;

// ║ Previous:
// ║ 
// ║ Regular_Interpolation_Format
// ║     : ':' Interpolated_Regular_String_Element+
// ║     ;
// ║ 
Regular_Interpolation_Format
    : ':' Interpolated_Regular_String_Element+ { this.LookAheadIs(1, '}') }? { this.PeekModeIs(IRS_CONT) }? { this.WrapToken(); }
    ;

mode IRS_CONT;

// ║ Previous:
// ║ 
// ║ Interpolated_Regular_String_End
// ║     : '"'
// ║     ;
// ║ 
Interpolated_Regular_String_End
    : '"' { this.WrapToken(); } -> popMode
    ;

mode DEFAULT_MODE;

fragment Interpolated_Regular_String_Element
    : Interpolated_Regular_String_Character
    | Simple_Escape_Sequence
    | Hexadecimal_Escape_Sequence
    | Unicode_Escape_Sequence
    | Open_Brace_Escape_Sequence
    | Close_Brace_Escape_Sequence
    ;

fragment Interpolated_Regular_String_Character
    // Any character except " (U+0022), \\ (U+005C),
    // { (U+007B), } (U+007D), and New_Line_Character.
    : ~["\\{}\u000D\u000A\u0085\u2028\u2029]
    ;

// ║ Previous:
// ║ 
// ║ Interpolated_Verbatim_String_Start
// ║     : '$@"'
// ║     | '@$"'
// ║     ;
// ║ 
/********************************************************************************************/

Interpolated_Verbatim_String_Start
    : ( '$@"'
      | '@$"'
      ) { this.WrapToken(); } -> pushMode(IVS_CONT)
    ;

mode IVS_CONT;

// ║ Previous:
// ║ 
// ║ // the following three lexical rules are context sensitive, see details below
// ║ 
// ║ 
// ║ Interpolated_Verbatim_String_Mid
// ║     : Interpolated_Verbatim_String_Element+
// ║     ;
// ║ 
Interpolated_Verbatim_String_Mid
    : Interpolated_Verbatim_String_Element+ { this.WrapToken(); }
    ;

mode DEFAULT_MODE;

// ║ Previous:
// ║ 
// ║ Verbatim_Interpolation_Format
// ║     : ':' Interpolated_Verbatim_String_Element+
// ║     ;
// ║ 
Verbatim_Interpolation_Format
    : ':' Interpolated_Verbatim_String_Element+ { this.LookAheadIs(1, '}') }? { this.PeekModeIs(IVS_CONT) }? { this.WrapToken(); }
    ;

mode IVS_CONT;

// ║ Previous:
// ║ 
// ║ Interpolated_Verbatim_String_End
// ║     : '"'
// ║     ;
// ║ 
Interpolated_Verbatim_String_End
    : '"' { this.WrapToken(); } -> popMode
    ;

mode DEFAULT_MODE;

fragment Interpolated_Verbatim_String_Element
    : Interpolated_Verbatim_String_Character
    | Quote_Escape_Sequence
    | Open_Brace_Escape_Sequence
    | Close_Brace_Escape_Sequence
    ;

fragment Interpolated_Verbatim_String_Character
    : ~["{}]    // Any character except " (U+0022), { (U+007B) and } (U+007D)
    ;

// lexical fragments used by both regular and verbatim interpolated strings

fragment Open_Brace_Escape_Sequence
    : '{{'
    ;

fragment Close_Brace_Escape_Sequence
    : '}}'
    ;


// If the lexer grammar switches mode and there has been no non-fragment rule
// since the last mode change, which can happen, then ANTLR will erroneously
// decide the complete mode has no non-fragment rules at all and complain...
// So in this case we give it a non-fragment rule to workaround the ANTLR bug.
// Bug last seen in ANTLR 4.9.2.

ANTLR_BUG_BEATER_1 : '⫸⫸⫸[1] This is to stop ANTLR barfing or otherwise being annoying⫷⫷⫷' ;

mode IRS_CONT;

// ║ Added ║
Interpolated_Regular_Solitary_LBrace
    : { this.LookAheadIsNot(2, '{') }? '{' -> type(TK_LBRACE), pushMode(DEFAULT_MODE)
    ;

mode IVS_CONT;

// ║ Added ║
Interpolated_Verbatim_Solitary_LBrace
    : { this.LookAheadIsNot(2, '{') }? '{' -> type(TK_LBRACE), pushMode(DEFAULT_MODE)
    ;

mode DEFAULT_MODE;


// If the lexer grammar switches mode and there has been no non-fragment rule
// since the last mode change, which can happen, then ANTLR will erroneously
// decide the complete mode has no non-fragment rules at all and complain...
// So in this case we give it a non-fragment rule to workaround the ANTLR bug.
// Bug last seen in ANTLR 4.9.2.

ANTLR_BUG_BEATER_2 : '⫸⫸⫸[2] This is to stop ANTLR barfing or otherwise being annoying⫷⫷⫷' ;

// Source: §6.5 Pre-processing directives — directive token mode

mode DIRECTIVE_MODE;

DIRECTIVE_WHITESPACES  : ( [\p{Zs}] | '\u0009' | '\u000B' | '\u000C' )+
                           -> channel(HIDDEN) ;
DIGITS                 : [0-9]+                  -> channel(DIRECTIVE) ;
DIRECTIVE_TRUE         : 'true'                  -> channel(DIRECTIVE), type(TRUE) ;
DIRECTIVE_FALSE        : 'false'                 -> channel(DIRECTIVE), type(FALSE) ;
DEFINE                 : 'define'                -> channel(DIRECTIVE) ;
UNDEF                  : 'undef'                 -> channel(DIRECTIVE) ;
DIRECTIVE_IF           : 'if'                    -> channel(DIRECTIVE), type(KW_IF) ;
ELIF                   : 'elif'                  -> channel(DIRECTIVE) ;
DIRECTIVE_ELSE         : 'else'                  -> channel(DIRECTIVE), type(KW_ELSE) ;
ENDIF                  : 'endif'                 -> channel(DIRECTIVE) ;
LINE                   : 'line'                  -> channel(DIRECTIVE) ;
ERROR                  : 'error'   [ \t]+        -> channel(DIRECTIVE), mode(DIRECTIVE_TEXT) ;
WARNING                : 'warning' [ \t]+        -> channel(DIRECTIVE), mode(DIRECTIVE_TEXT) ;
REGION                 : 'region'  [ \t]*        -> channel(DIRECTIVE), mode(DIRECTIVE_TEXT) ;
ENDREGION              : 'endregion' [ \t]*      -> channel(DIRECTIVE), mode(DIRECTIVE_TEXT) ;
PRAGMA                 : 'pragma'  [ \t]+        -> channel(DIRECTIVE), mode(DIRECTIVE_TEXT) ;
NULLABLE               : 'nullable' [ \t]+       -> channel(DIRECTIVE), mode(DIRECTIVE_TEXT) ;
DIRECTIVE_DEFAULT      : 'default'               -> channel(DIRECTIVE), type(DEFAULT) ;
DIRECTIVE_HIDDEN       : 'hidden'                -> channel(DIRECTIVE) ;
DIRECTIVE_OPEN_PARENS  : '('                     -> channel(DIRECTIVE), type(TK_LPAREN) ;
DIRECTIVE_CLOSE_PARENS : ')'                     -> channel(DIRECTIVE), type(TK_RPAREN) ;
DIRECTIVE_BANG         : '!'                     -> channel(DIRECTIVE), type(TK_NOT) ;
DIRECTIVE_OP_EQ        : '=='                    -> channel(DIRECTIVE), type(TK_EQ_EQ) ;
DIRECTIVE_OP_NE        : '!='                    -> channel(DIRECTIVE), type(TK_NOT_EQ) ;
DIRECTIVE_OP_AND       : '&&'                    -> channel(DIRECTIVE), type(TK_AND_AND) ;
DIRECTIVE_OP_OR        : '||'                    -> channel(DIRECTIVE), type(TK_OR_OR) ;
DIRECTIVE_STRING       : '"' ~["\r\n\u0085\u2028\u2029]* '"'
                           -> channel(DIRECTIVE) ;
CONDITIONAL_SYMBOL     : Basic_Identifier        -> channel(DIRECTIVE) ;
DIRECTIVE_SINGLE_LINE_COMMENT
                       : '//' Input_Character*   -> channel(HIDDEN) ;
DIRECTIVE_NEW_LINE     : ( New_Line_Character | '\u000D\u000A' )
                           -> channel(DIRECTIVE), mode(DEFAULT_MODE) ;

mode DIRECTIVE_TEXT;

TEXT          : Input_Character+                 -> channel(DIRECTIVE) ;
TEXT_NEW_LINE : ( New_Line_Character | '\u000D\u000A' )
                  -> channel(DIRECTIVE), type(DIRECTIVE_NEW_LINE), mode(DEFAULT_MODE) ;

