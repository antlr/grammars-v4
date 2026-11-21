/*
 * Lexer grammar for Invisible XML (iXML) 1.0
 * https://invisiblexml.org/1.0/
 *
 * See ixmlParser.g4 for the parser grammar and design notes.
 */

// $antlr-format alignColons trailing, alignLabels true, alignLexerCommands true, alignSemicolons ownLine, alignTrailers true
// $antlr-format alignTrailingComments true, allowShortBlocksOnASingleLine true, allowShortRulesOnASingleLine true, columnLimit 150
// $antlr-format maxEmptyLinesToKeep 1, minEmptyLines 0, reflowComments false, singleLineOverrulesHangingColon true, useTab false

lexer grammar ixmlLexer;

// ── Default mode ─────────────────────────────────────────────────────────────

// Keywords — must precede NAME so that 'ixml' and 'version' are recognised
// in prolog context; the 'name' parser rule also accepts these tokens so
// they may still be used as rule names in user grammars.
IXML_KW    : 'ixml';
VERSION_KW : 'version';

// Unicode category codes: one capital letter, optional lowercase letter
// (e.g. L, Zs, Nd, Mn, Lu, Ll, Lt, …).
// Listed before NAME so that short identifiers like 'L' or 'Lu' are
// tokenised as CODE when they appear as class members inside sets.
CODE: [A-Z] [a-z]?;

// Name segments: dots remain separate tokens so the parser can distinguish
// a name's dots (including trailing dots) from a rule-terminating dot.
// Visible whitespace prevents the parser from joining separated segments.
// '\u002D' = '-', '\u00B7' = '·', '\u203F' = '‿', '\u2040' = '⁀'.
NAME: [_\p{L}] [_\p{L}\p{Nd}\p{Mn}\u002D\u00B7\u203F\u2040]*;
NAME_FOLLOWER: [\p{Nd}\p{Mn}\u00B7\u203F\u2040]+;

// Mark / tmark single characters
AT    : '@';
CARET : '^';
MINUS : '-';

// Assignment operator (= or :)
ASSIGN: [=:];

// Punctuation — DSTAR/DPLUS must precede STAR/PLUS (longest-match rule)
DOT      : '.';
COMMA    : ',';
ALT_SEP  : [;|];
DSTAR    : '**';
STAR     : '*';
DPLUS    : '++';
PLUS     : '+';
QMARK    : '?';
LPAREN   : '(';
RPAREN   : ')';
LBRACKET : '[';
RBRACKET : ']';
TILDE    : '~';

// '#' switches to HEX_MODE so that the following hex digits are never
// confused with NAME tokens, even when they start with a letter (a-f, A-F)
// or contain the '-' name-follower character.
HASH: '#' -> pushMode(HEX_MODE);

// String literals
// dchar: any non-", non-CR, non-LF character, or escaped ""
// schar: any non-', non-CR, non-LF character, or escaped ''
// Single decoded characters have distinct token types for range endpoints.
// These rules precede STRING so equal-length matches prefer CHAR.
DQUOTE_CHAR   : '"' (~["\r\n] | '""') '"';
SQUOTE_CHAR   : '\'' (~['\r\n] | '\'\'') '\'';
DQUOTE_STRING : '"' (~["\r\n] | '""')+ '"';
SQUOTE_STRING : '\'' (~['\r\n] | '\'\'')+ '\'';

// Keep separators on the default channel for the parser's s and rs rules.
WS: [\p{Zs}\t\r\n]+;

// Comments with support for nesting: '{' (COMMENT | non-brace-char)* '}'
COMMENT: '{' (COMMENT | ~[{}])* '}';

// ── HEX_MODE ─────────────────────────────────────────────────────────────────
// Entered after '#'; consumes one or more hex digits then returns to the
// default mode.  This prevents hex letters (a-f, A-F) from being merged into
// surrounding NAME tokens, and prevents '-' from being swallowed into a NAME
// when patterns like '#A0-#FF' appear in character-class ranges.
mode HEX_MODE;
HEX_DIGITS: [0-9a-fA-F]+ -> popMode;
