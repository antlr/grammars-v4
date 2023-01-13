/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2013-2014 by Bart Kiers
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * Project      : A Portable Game Notation (PGN) ANTLR 4 grammar
 *                and parser.
 * Developed by : Bart Kiers, bart@big-o.nl
 * Also see     : https://github.com/bkiers/PGN-parser
 */

//
// A Portable Game Notation (PGN) grammar based on:
// http://www.thechessdrum.net/PGN_Reference.txt
//
// The inline comments starting with "///" in this grammar are direct 
// copy-pastes from the PGN reference linked above.
//
grammar PGN;

// The entry point of the grammar.
parse
 : pgn_database EOF
 ;

/// <PGN-database> ::= <PGN-game> <PGN-database>
///                    <empty>
pgn_database
 : pgn_game*
 ;

/// <PGN-game> ::= <tag-section> <movetext-section>
pgn_game
 : tag_section movetext_section
 ;

/// <tag-section> ::= <tag-pair> <tag-section>
///                   <empty>
tag_section
 : tag_pair*
 ;

/// <tag-pair> ::= [ <tag-name> <tag-value> ]
tag_pair
 : LEFT_BRACKET tag_name tag_value RIGHT_BRACKET
 ;

/// <tag-name> ::= <identifier>
tag_name
 : SYMBOL
 ;

/// <tag-value> ::= <string>
tag_value
 : STRING
 ;
 
/// <movetext-section> ::= <element-sequence> <game-termination>
movetext_section
 : element_sequence game_termination
 ;

/// <element-sequence> ::= <element> <element-sequence>
///                        <recursive-variation> <element-sequence>
///                        <empty>
element_sequence
 : (element | recursive_variation)*
 ;

/// <element> ::= <move-number-indication>
///               <SAN-move>
///               <numeric-annotation-glyph>
element
 : move_number_indication
 | san_move
 | NUMERIC_ANNOTATION_GLYPH
 ;

move_number_indication
 : INTEGER PERIOD?
 ;

san_move
 : SYMBOL
 ;

/// <recursive-variation> ::= ( <element-sequence> )
recursive_variation
 : LEFT_PARENTHESIS element_sequence RIGHT_PARENTHESIS
 ;

/// <game-termination> ::= 1-0
///                        0-1
///                        1/2-1/2
///                        *
game_termination
 : WHITE_WINS
 | BLACK_WINS
 | DRAWN_GAME
 | ASTERISK
 ;

WHITE_WINS
 : '1-0'
 ;

BLACK_WINS
 : '0-1'
 ;

DRAWN_GAME
 : '1/2-1/2'
 ;

/// Comment text may appear in PGN data.  There are two kinds of comments.  The
/// first kind is the "rest of line" comment; this comment type starts with a
/// semicolon character and continues to the end of the line.  The second kind
/// starts with a left brace character and continues to the next right brace
/// character.  Comments cannot appear inside any token.
REST_OF_LINE_COMMENT
 : ';' ~[\r\n]* -> skip
 ;

/// Brace comments do not nest; a left brace character appearing in a brace comment
/// loses its special meaning and is ignored.  A semicolon appearing inside of a
/// brace comment loses its special meaning and is ignored.  Braces appearing
/// inside of a semicolon comments lose their special meaning and are ignored.
BRACE_COMMENT
 : '{' ~'}'* '}' -> skip
 ;

/// There is a special escape mechanism for PGN data.  This mechanism is triggered
/// by a percent sign character ("%") appearing in the first column of a line; the
/// data on the rest of the line is ignored by publicly available PGN scanning
/// software.  This escape convention is intended for the private use of software
/// developers and researchers to embed non-PGN commands and data in PGN streams.
///
/// A percent sign appearing in any other place other than the first position in a
/// line does not trigger the escape mechanism.
ESCAPE
 : {getCharPositionInLine() == 0}? '%' ~[\r\n]* -> skip
 ;

SPACES
 : [ \t\r\n]+ -> skip
 ;

/// A string token is a sequence of zero or more printing characters delimited by a
/// pair of quote characters (ASCII decimal value 34, hexadecimal value 0x22).  An
/// empty string is represented by two adjacent quotes.  (Note: an apostrophe is
/// not a quote.)  A quote inside a string is represented by the backslash
/// immediately followed by a quote.  A backslash inside a string is represented by
/// two adjacent backslashes.  Strings are commonly used as tag pair values (see
/// below).  Non-printing characters like newline and tab are not permitted inside
/// of strings.  A string token is terminated by its closing quote.  Currently, a
/// string is limited to a maximum of 255 characters of data.
STRING
 : '"' ('\\\\' | '\\"' | ~[\\"])* '"'
 ;

/// An integer token is a sequence of one or more decimal digit characters.  It is
/// a special case of the more general "symbol" token class described below.
/// Integer tokens are used to help represent move number indications (see below).
/// An integer token is terminated just prior to the first non-symbol character
/// following the integer digit sequence.
INTEGER
 : [0-9]+
 ;

/// A period character (".") is a token by itself.  It is used for move number
/// indications (see below).  It is self terminating.
PERIOD
 : '.'
 ;

/// An asterisk character ("*") is a token by itself.  It is used as one of the
/// possible game termination markers (see below); it indicates an incomplete game
/// or a game with an unknown or otherwise unavailable result.  It is self
/// terminating.
ASTERISK
 : '*'
 ;

/// The left and right bracket characters ("[" and "]") are tokens.  They are used
/// to delimit tag pairs (see below).  Both are self terminating.
LEFT_BRACKET
 : '['
 ;

RIGHT_BRACKET
 : ']'
 ;

/// The left and right parenthesis characters ("(" and ")") are tokens.  They are
/// used to delimit Recursive Annotation Variations (see below).  Both are self
/// terminating.
LEFT_PARENTHESIS
 : '('
 ;

RIGHT_PARENTHESIS
 : ')'
 ;

/// The left and right angle bracket characters ("<" and ">") are tokens.  They are
/// reserved for future expansion.  Both are self terminating.
LEFT_ANGLE_BRACKET
 : '<'
 ;

RIGHT_ANGLE_BRACKET
 : '>'
 ;

/// A Numeric Annotation Glyph ("NAG", see below) is a token; it is composed of a
/// dollar sign character ("$") immediately followed by one or more digit
/// characters.  It is terminated just prior to the first non-digit character
/// following the digit sequence.
NUMERIC_ANNOTATION_GLYPH
 : '$' [0-9]+
 ;

/// A symbol token starts with a letter or digit character and is immediately
/// followed by a sequence of zero or more symbol continuation characters.  These
/// continuation characters are letter characters ("A-Za-z"), digit characters
/// ("0-9"), the underscore ("_"), the plus sign ("+"), the octothorpe sign ("#"),
/// the equal sign ("="), the colon (":"),  and the hyphen ("-").  Symbols are used
/// for a variety of purposes.  All characters in a symbol are significant.  A
/// symbol token is terminated just prior to the first non-symbol character
/// following the symbol character sequence.  Currently, a symbol is limited to a
/// maximum of 255 characters in length.
SYMBOL
 : [a-zA-Z0-9] [a-zA-Z0-9_+#=:-]*
 ;

/// Import format PGN allows for the use of traditional suffix annotations for
/// moves.  There are exactly six such annotations available: "!", "?", "!!", "!?",
/// "?!", and "??".  At most one such suffix annotation may appear per move, and if
/// present, it is always the last part of the move symbol.
SUFFIX_ANNOTATION
 : [?!] [?!]?
 ;

// A fall through rule that will catch any character not matched by any of the
// previous lexer rules.
UNEXPECTED_CHAR
 : .
 ;
