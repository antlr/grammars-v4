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
 * Project      : An IRI ANTLR 4 grammar and parser.
 * Developed by : Bart Kiers, bart@big-o.nl
 * Also see     : https://github.com/bkiers/iri-parser
 */
//
// An IRI grammar based on: http://www.ietf.org/rfc/rfc3987.txt
//
// The inline comments starting with "///" in this grammar are direct 
// copy-pastes from the ABNF syntax from the reference linked above.
//

grammar IRI;

/*
 * Parser rules
 */
parse
   : iri EOF
   ;

/// IRI            = scheme ":" ihier-part [ "?" iquery ] [ "#" ifragment ]
iri
   : scheme ':' ihier_part ('?' iquery)? ('#' ifragment)?
   ;

/// ihier-part     = "//" iauthority ipath-abempty///                / ipath-absolute///                / ipath-rootless///                / ipath-empty
ihier_part
   : '//' iauthority ipath_abempty
   | ipath_absolute
   | ipath_rootless
   | ipath_empty
   ;

/// IRI-reference  = IRI / irelative-ref
iri_reference
   : iri
   | irelative_ref
   ;

/// absolute-IRI   = scheme ":" ihier-part [ "?" iquery ]
absolute_iri
   : scheme ':' ihier_part ('?' iquery)?
   ;

/// irelative-ref  = irelative-part [ "?" iquery ] [ "#" ifragment ]
irelative_ref
   : irelative_part ('?' iquery)? ('#' ifragment)?
   ;

/// irelative-part = "//" iauthority ipath-abempty///                     / ipath-absolute///                     / ipath-noscheme///                     / ipath-empty
irelative_part
   : '//' iauthority ipath_abempty
   | ipath_absolute
   | ipath_noscheme
   | ipath_empty
   ;

/// iauthority     = [ iuserinfo "@" ] ihost [ ":" port ]
iauthority
   : (iuserinfo '@')? ihost (':' port)?
   ;

/// iuserinfo      = *( iunreserved / pct-encoded / sub-delims / ":" )
iuserinfo
   : (iunreserved | pct_encoded | sub_delims | ':')*
   ;

/// ihost          = IP-literal / IPv4address / ireg-name
ihost
   : ip_literal
   | ip_v4_address
   | ireg_name
   ;

/// ireg-name      = *( iunreserved / pct-encoded / sub-delims )
ireg_name
   : (iunreserved | pct_encoded | sub_delims)*
   ;

/// ipath          = ipath-abempty   ; begins with "/" or is empty///                / ipath-absolute  ; begins with "/" but not "//"///                / ipath-noscheme  ; begins with a non-colon segment///                / ipath-rootless  ; begins with a segment///                / ipath-empty     ; zero characters
ipath
   : ipath_abempty
   | ipath_absolute
   | ipath_noscheme
   | ipath_rootless
   | ipath_empty
   ;

/// ipath-abempty  = *( "/" isegment )
ipath_abempty
   : ('/' isegment)*
   ;

/// ipath-absolute = "/" [ isegment-nz *( "/" isegment ) ]
ipath_absolute
   : '/' (isegment_nz ('/' isegment)*)?
   ;

/// ipath-noscheme = isegment-nz-nc *( "/" isegment )
ipath_noscheme
   : isegment_nz_nc ('/' isegment)*
   ;

/// ipath-rootless = isegment-nz *( "/" isegment )
ipath_rootless
   : isegment_nz ('/' isegment)*
   ;

/// ipath-empty    = 0<ipchar>
ipath_empty
   :/* nothing */
   ;

/// isegment       = *ipchar
isegment
   : ipchar*
   ;

/// isegment-nz    = 1*ipchar
isegment_nz
   : ipchar +
   ;

/// isegment-nz-nc = 1*( iunreserved / pct-encoded / sub-delims / "@" )///                ; non-zero-length segment without any colon ":"
isegment_nz_nc
   : (iunreserved | pct_encoded | sub_delims | '@') +
   ;

/// ipchar         = iunreserved / pct-encoded / sub-delims / ":" / "@"
ipchar
   : iunreserved
   | pct_encoded
   | sub_delims
   | (':' | '@')
   ;

/// iquery         = *( ipchar / iprivate / "/" / "?" )
iquery
   : (ipchar | (IPRIVATE | '/' | '?'))*
   ;

/// ifragment      = *( ipchar / "/" / "?" )
ifragment
   : (ipchar | ('/' | '?'))*
   ;

/// iunreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar
iunreserved
   : alpha
   | digit
   | ('-' | '.' | '_' | '~' | UCSCHAR)
   ;

/// scheme         = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
scheme
   : alpha (alpha | digit | ('+' | '-' | '.'))*
   ;

/// port           = *DIGIT
port
   : digit*
   ;

/// IP-literal     = "[" ( IPv6address / IPvFuture  ) "]"
ip_literal
   : '[' (ip_v6_address | ip_v_future) ']'
   ;

/// IPvFuture      = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
ip_v_future
   : V hexdig + '.' (unreserved | sub_delims | ':') +
   ;

/// IPv6address    =                            6( h16 ":" ) ls32///                /                       "::" 5( h16 ":" ) ls32///                / [               h16 ] "::" 4( h16 ":" ) ls32///                / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32///                / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32///                / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32///                / [ *4( h16 ":" ) h16 ] "::"              ls32///                / [ *5( h16 ":" ) h16 ] "::"              h16///                / [ *6( h16 ":" ) h16 ] "::"
ip_v6_address
   : h16 ':' h16 ':' h16 ':' h16 ':' h16 ':' h16 ':' ls32
   | '::' h16 ':' h16 ':' h16 ':' h16 ':' h16 ':' ls32
   | h16? '::' h16 ':' h16 ':' h16 ':' h16 ':' ls32
   | ((h16 ':')? h16)? '::' h16 ':' h16 ':' h16 ':' ls32
   | (((h16 ':')? h16 ':')? h16)? '::' h16 ':' h16 ':' ls32
   | ((((h16 ':')? h16 ':')? h16 ':')? h16)? '::' h16 ':' ls32
   | (((((h16 ':')? h16 ':')? h16 ':')? h16 ':')? h16)? '::' ls32
   | ((((((h16 ':')? h16 ':')? h16 ':')? h16 ':')? h16 ':')? h16)? '::' h16
   | (((((((h16 ':')? h16 ':')? h16 ':')? h16 ':')? h16 ':')? h16 ':')? h16)? '::'
   ;

/// h16            = 1*4HEXDIG
h16
   : hexdig hexdig hexdig hexdig
   | hexdig hexdig hexdig
   | hexdig hexdig
   | hexdig
   ;

/// ls32           = ( h16 ":" h16 ) / IPv4address
ls32
   : h16 ':' h16
   | ip_v4_address
   ;

/// IPv4address    = dec-octet "." dec-octet "." dec-octet "." dec-octet
ip_v4_address
   : dec_octet '.' dec_octet '.' dec_octet '.' dec_octet
   ;

/// dec-octet      = DIGIT                 ; 0-9///                / %x31-39 DIGIT         ; 10-99///                / "1" 2DIGIT            ; 100-199///                / "2" %x30-34 DIGIT     ; 200-249///                / "25" %x30-35          ; 250-255
dec_octet
   : digit
   | non_zero_digit digit
   | D1 digit digit
   | D2 (D0 | D1 | D2 | D3 | D4) digit
   | D2 D5 (D0 | D1 | D2 | D3 | D4 | D5)
   ;

/// pct-encoded    = "%" HEXDIG HEXDIG
pct_encoded
   : '%' hexdig hexdig
   ;

/// unreserved     = ALPHA / DIGIT / "-" / "." / "_" / "~"
unreserved
   : alpha
   | digit
   | ('-' | '.' | '_' | '~')
   ;

/// reserved       = gen-delims / sub-delims
reserved
   : gen_delims
   | sub_delims
   ;

/// gen-delims     = ":" / "/" / "?" / "#" / "[" / "]" / "@"
gen_delims
   : ':'
   | '/'
   | '?'
   | '#'
   | '['
   | ']'
   | '@'
   ;

/// sub-delims     = "!" / "$" / "&" / "'" / "(" / ")"///                / "*" / "+" / "," / ";" / "="
sub_delims
   : '!'
   | '$'
   | '&'
   | '\''
   | '('
   | ')'
   | '*'
   | '+'
   | ','
   | ';'
   | '='
   ;

alpha
   : A
   | B
   | C
   | D
   | E
   | F
   | G
   | H
   | I
   | J
   | K
   | L
   | M
   | N
   | O
   | P
   | Q
   | R
   | S
   | T
   | U
   | V
   | W
   | X
   | Y
   | Z
   ;

hexdig
   : digit
   | (A | B | C | D | E | F)
   ;

digit
   : D0
   | non_zero_digit
   ;

non_zero_digit
   : D1
   | D2
   | D3
   | D4
   | D5
   | D6
   | D7
   | D8
   | D9
   ;

/*
 * Lexer rules
 *//// ucschar        = %xA0-D7FF / %xF900-FDCF / %xFDF0-FFEF///                / %x10000-1FFFD / %x20000-2FFFD / %x30000-3FFFD///                / %x40000-4FFFD / %x50000-5FFFD / %x60000-6FFFD///                / %x70000-7FFFD / %x80000-8FFFD / %x90000-9FFFD///                / %xA0000-AFFFD / %xB0000-BFFFD / %xC0000-CFFFD///                / %xD0000-DFFFD / %xE1000-EFFFD

UCSCHAR
   : '\u00A0' .. '\uD7FF'
   | '\uF900' .. '\uFDCF'
   | '\uFDF0' .. '\uFFEF'
   | '\u{10000}' .. '\u{1FFFD}'
   | '\u{20000}' .. '\u{2FFFD}'
   | '\u{30000}' .. '\u{3FFFD}'
   | '\u{40000}' .. '\u{4FFFD}'
   | '\u{50000}' .. '\u{5FFFD}'
   | '\u{60000}' .. '\u{6FFFD}'
   | '\u{70000}' .. '\u{7FFFD}'
   | '\u{80000}' .. '\u{8FFFD}'
   | '\u{90000}' .. '\u{9FFFD}'
   | '\u{A0000}' .. '\u{AFFFD}'
   | '\u{B0000}' .. '\u{BFFFD}'
   | '\u{C0000}' .. '\u{CFFFD}'
   | '\u{D0000}' .. '\u{DFFFD}'
   | '\u{E1000}' .. '\u{EFFFD}'
   ;

/// iprivate       = %xE000-F8FF / %xF0000-FFFFD / %x100000-10FFFD

IPRIVATE
   : '\uE000' .. '\uF8FF'
   | '\u{F0000}' .. '\u{FFFFD}'
   | '\u{100000}' .. '\u{10FFFD}'
   ;


D0
   : '0'
   ;


D1
   : '1'
   ;


D2
   : '2'
   ;


D3
   : '3'
   ;


D4
   : '4'
   ;


D5
   : '5'
   ;


D6
   : '6'
   ;


D7
   : '7'
   ;


D8
   : '8'
   ;


D9
   : '9'
   ;


A
   : [aA]
   ;


B
   : [bB]
   ;


C
   : [cC]
   ;


D
   : [dD]
   ;


E
   : [eE]
   ;


F
   : [fF]
   ;


G
   : [gG]
   ;


H
   : [hH]
   ;


I
   : [iI]
   ;


J
   : [jJ]
   ;


K
   : [kK]
   ;


L
   : [lL]
   ;


M
   : [mM]
   ;


N
   : [nN]
   ;


O
   : [oO]
   ;


P
   : [pP]
   ;


Q
   : [qQ]
   ;


R
   : [rR]
   ;


S
   : [sS]
   ;


T
   : [tT]
   ;


U
   : [uU]
   ;


V
   : [vV]
   ;


W
   : [wW]
   ;


X
   : [xX]
   ;


Y
   : [yY]
   ;


Z
   : [zZ]
   ;


COL2
   : '::'
   ;


COL
   : ':'
   ;


DOT
   : '.'
   ;


PERCENT
   : '%'
   ;


HYPHEN
   : '-'
   ;


TILDE
   : '~'
   ;


USCORE
   : '_'
   ;


EXCL
   : '!'
   ;


DOLLAR
   : '$'
   ;


AMP
   : '&'
   ;


SQUOTE
   : '\''
   ;


OPAREN
   : '('
   ;


CPAREN
   : ')'
   ;


STAR
   : '*'
   ;


PLUS
   : '+'
   ;


COMMA
   : ','
   ;


SCOL
   : ';'
   ;


EQUALS
   : '='
   ;


FSLASH2
   : '//'
   ;


FSLASH
   : '/'
   ;


QMARK
   : '?'
   ;


HASH
   : '#'
   ;


OBRACK
   : '['
   ;


CBRACK
   : ']'
   ;


AT
   : '@'
   ;
