/* Derived from http://www.w3.org/TR/turtle/#sec-grammar-grammar */


[1]	turtleDoc	::=	statement*
[2]	statement	::=	directive | triples '.'
[3]	directive	::=	prefixID | base | sparqlPrefix | sparqlBase
[4]	prefixID	::=	'@prefix' PNAME_NS IRIREF '.'
[5]	base	::=	'@base' IRIREF '.'
[5s]	sparqlBase	::=	"BASE" IRIREF
[6s]	sparqlPrefix	::=	"PREFIX" PNAME_NS IRIREF
[6]	triples	::=	subject predicateObjectList | blankNodePropertyList predicateObjectList?
[7]	predicateObjectList	::=	verb objectList (';' (verb objectList)?)*
[8]	objectList	::=	object (',' object)*
[9]	verb	::=	predicate | 'a'
[10]	subject	::=	iri | BlankNode | collection
[11]	predicate	::=	iri
[12]	object	::=	iri | BlankNode | collection | blankNodePropertyList | literal
[13]	literal	::=	RDFLiteral | NumericLiteral | BooleanLiteral
[14]	blankNodePropertyList	::=	'[' predicateObjectList ']'
[15]	collection	::=	'(' object* ')'
[16]	NumericLiteral	::=	INTEGER | DECIMAL | DOUBLE
[128s]	RDFLiteral	::=	String (LANGTAG | '^^' iri)?
[133s]	BooleanLiteral	::=	'true' | 'false'
[17]	String	::=	STRING_LITERAL_QUOTE | STRING_LITERAL_SINGLE_QUOTE | STRING_LITERAL_LONG_SINGLE_QUOTE | STRING_LITERAL_LONG_QUOTE
[135s]	iri	::=	IRIREF | PrefixedName
[136s]	PrefixedName	::=	PNAME_LN | PNAME_NS
[137s]	BlankNode	::=	BLANK_NODE_LABEL | ANON
Productions for terminals

[18]	IRIREF	::=	'<' ([^#x00-#x20<>"{}|^`\] | UCHAR)* '>' /* #x00=NULL #01-#x1F=control codes #x20=space */
[139s]	PNAME_NS	::=	PN_PREFIX? ':'
[140s]	PNAME_LN	::=	PNAME_NS PN_LOCAL
[141s]	BLANK_NODE_LABEL	::=	'_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
[144s]	LANGTAG	::=	'@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
[19]	INTEGER	::=	[+-]? [0-9]+
[20]	DECIMAL	::=	[+-]? [0-9]* '.' [0-9]+
[21]	DOUBLE	::=	[+-]? ([0-9]+ '.' [0-9]* EXPONENT | '.' [0-9]+ EXPONENT | [0-9]+ EXPONENT)
[154s]	EXPONENT	::=	[eE] [+-]? [0-9]+
[22]	STRING_LITERAL_QUOTE	::=	'"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"' /* #x22=" #x5C=\ #xA=new line #xD=carriage return */
[23]	STRING_LITERAL_SINGLE_QUOTE	::=	"'" ([^#x27#x5C#xA#xD] | ECHAR | UCHAR)* "'" /* #x27=' #x5C=\ #xA=new line #xD=carriage return */
[24]	STRING_LITERAL_LONG_SINGLE_QUOTE	::=	"'''" (("'" | "''")? ([^'\] | ECHAR | UCHAR))* "'''"
[25]	STRING_LITERAL_LONG_QUOTE	::=	'"""' (('"' | '""')? ([^"\] | ECHAR | UCHAR))* '"""'
[26]	UCHAR	::=	'\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
[159s]	ECHAR	::=	'\' [tbnrf"'\]
[161s]	WS	::=	#x20 | #x9 | #xD | #xA /* #x20=space #x9=character tabulation #xD=carriage return #xA=new line */
[162s]	ANON	::=	'[' WS* ']'
[163s]	PN_CHARS_BASE	::=	[A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
[164s]	PN_CHARS_U	::=	PN_CHARS_BASE | '_'
[166s]	PN_CHARS	::=	PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
[167s]	PN_PREFIX	::=	PN_CHARS_BASE ((PN_CHARS | '.')* PN_CHARS)?
[168s]	PN_LOCAL	::=	(PN_CHARS_U | ':' | [0-9] | PLX) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))?
[169s]	PLX	::=	PERCENT | PN_LOCAL_ESC
[170s]	PERCENT	::=	'%' HEX HEX
[171s]	HEX	::=	[0-9] | [A-F] | [a-f]
[172s]	PN_LOCAL_ESC	::=	'\' ('_' | '~' | '.' | '-' | '!' | '$' | '&' | "'" | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '/' | '?' | '#' | '@' | '%')