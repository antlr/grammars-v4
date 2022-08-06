grammar http;

/*
 HTTP-message = start‑line ( header‑field  CRLF ) CRLF [ message‑body ]
 */
http_message: start_line (header_field CRLF)* CRLF EOF //message_body
            ;

/*
 start-line = request‑line / status‑line
 */
start_line: request_line;

/*
 request-line = method  SP  request‑target  SP  HTTP‑version  CRLF
 */
request_line: method SP request_target SP http_version CRLF;

/*
 method = token ; "GET" ; → RFC 7231 – Section 4.3.1 ; "HEAD" ; → RFC 7231 – Section 4.3.2 ; "POST"
 ; → RFC 7231 – Section 4.3.3 ; "PUT" ; → RFC 7231 – Section 4.3.4 ; "DELETE" ; → RFC 7231 – Section
 4.3.5 ; "CONNECT" ; → RFC 7231 – Section 4.3.6 ; "OPTIONS" ; → RFC 7231 – Section 4.3.7 ; "TRACE"
 ; → RFC 7231 – Section 4.3.8
 */
method:
	'GET'
	| 'HEAD'
	| 'POST'
	| 'PUT'
	| 'DELETE'
	| 'CONNECT'
	| 'OPTIONS'
	| 'TRACE';

/*
 request-target = origin-form / absolute-form / authority-form / asterisk-form
 */
request_target: origin_form;

/*
 origin-form = absolute-path  [ "?"  query ]
 */
origin_form: absolute_path (QuestionMark query)?;

/*
 absolute-path = 1*( "/"  segment )
 */
absolute_path: (Slash segment)+;

/*
 segment = pchar
 */
segment: pchar*;

/*
 query = ( pchar /  "/" /  "?" )
 */
query: (pchar | Slash | QuestionMark)*;

/*
 HTTP-version = HTTP-name '/' DIGIT  "."  DIGIT
 */
http_version: http_name DIGIT Dot DIGIT;

/*
 HTTP-name = %x48.54.54.50 ; "HTTP", case-sensitive
 */
http_name: 'HTTP/';


/*
 header-field = field-name  ":"  OWS  field-value  OWS 
 */
header_field: field_name Colon OWS* field_value OWS*;

/*
 field-name = token
 */
field_name: token;

/*
 token
 */
token: tchar+;
/*
 field-value = ( field-content / obs-fold )
 */
field_value: (field_content | obs_fold)+;

/*
 field-content = field-vchar [ 1*( SP / HTAB )  field-vchar ]
 */
field_content: field_vchar ((SP | HTAB)+ field_vchar)?;

/*
 field-vchar = VCHAR / obs-text
 */
field_vchar: vCHAR | obs_text;
/*
 obs-text = %x80-FF
 */
obs_text: OBS_TEXT;
/*
 obs-fold = CRLF  1*( SP / HTAB ) ; see RFC 7230 – Section 3.2.4
 */
obs_fold: CRLF (SP | HTAB)+;

/*
 message-body = OCTET
 */
//message_body: OCTET*;


/*
 SP = %x20 ; space
 */
SP: ' ';
/*
 pchar = unreserved / pct‑encoded / sub‑delims / ":" / "@"
 */
pchar: unreserved | Pct_encoded | sub_delims | Colon | At;

/*
 unreserved = ALPHA /  DIGIT /  "-" /  "." /  "_" /  "~"
 */
unreserved: ALPHA | DIGIT | Minus | Dot | Underscore | Tilde;

/*
 ALPHA = %x41‑5A /  %x61‑7A ; A‑Z / a‑z
 */
ALPHA: [A-Za-z];

/*
 DIGIT = %x30‑39 ; 0-9
 */
DIGIT: [0-9];

/*
 pct-encoded = "%"  HEXDIG  HEXDIG
 */
Pct_encoded: Percent HEXDIG HEXDIG;

/*
 HEXDIG = DIGIT /  "A" /  "B" /  "C" /  "D" /  "E" /  "F"
 */
HEXDIG: DIGIT | 'A' | 'B' | 'C' | 'D' | 'E' | 'F';

/*
 sub-delims = "!" /  "$" /  "&" /  "'" /  "(" /  ")" /  "*" /  "+" /  "," /  ";" /  "="
 */
sub_delims:
	ExclamationMark
	| DollarSign
	| Ampersand
	| SQuote
	| LColumn
	| RColumn
	| Star
	| Plus
	| SemiColon
	| Period
	| Equals;

LColumn:'(';
RColumn:')';
SemiColon:';';
Equals:'=';
Period:',';

/*
 CRLF = CR  LF ; Internet standard newline
 */
CRLF: '\n';

/*
 tchar = "!" /  "#" /  "$" /  "%" /  "&" /  "'" /  "*" /  "+" /  "-" /  "." /  "^" /  "_" /  "`" / 
 "|" /  "~" /  DIGIT /  ALPHA
 */
tchar:
	  ExclamationMark
	| DollarSign
	| Hashtag
	| Percent
	| Ampersand
	| SQuote
	| Star
	| Plus
    | Minus
	| Dot
	| Caret
    | Underscore
	| BackQuote
	| VBar
	| Tilde
	| DIGIT
	| ALPHA;

Minus :'-';
Dot   : '.';
Underscore: '_';
Tilde : '~';
QuestionMark :'?';
Slash :'/';
ExclamationMark: '!';
Colon:':';
At: '@';
DollarSign:'$';
Hashtag:'#';
Ampersand:'&';
Percent:'%';
SQuote:'\'';
Star:'*';
Plus:'+';
Caret:'^';
BackQuote:'`';
VBar:'|';

/*
 OWS = ( SP / HTAB ) ; optional whitespace
 */
OWS: SP | HTAB;

/*
 HTAB = %x09 ; horizontal tab
 */
HTAB: '\t';


/*
 VCHAR = %x21-7E ; visible (printing) characters
 */
vCHAR: ALPHA | DIGIT | VCHAR;

VCHAR:
	ExclamationMark
	| '"'
	| Hashtag
	| DollarSign
	| Percent
	| Ampersand
	| SQuote
	| LColumn
	| RColumn
	| RColumn
	| Star
	| Plus
	| Period
	| Minus
	| Dot
	| Slash
	| Colon
	| SemiColon
	| '<'
	| Equals
	| '>'
	| QuestionMark
	| At
	| '['
	| '\\'
	| Caret
	| Underscore
	| ']'
	| BackQuote
	| '{'
	| '}'
	| VBar
	| Tilde;

OBS_TEXT: '\u0080' ..'\u00ff';

/*
 OCTET = %x00-FF ; 8 bits of data
 */
//OCTET: '\u0000' .. '\u001f' | VCHAR | '\u007f' .. '\u00ff' ;