/*
 * XPath 2.0 Grammar
 * Paul Bober and Stephen Tu
 * Intuit, Inc.
 * v1.0, August 2009
 *
 * An ANTLR implementation of the XPath 2.0 Standard.  See: http://www.w3.org/TR/xpath20/
 *
 */
 
 
grammar XPath2;

options {
	language=Java;
	}
 
GENERAL_COMP
	: '=' | '!=' | '<' | '<=' | '>' | '>='
	;
	
VALUE_COMP
	:'eq' | 'ne' | 'lt' | 'le' | 'gt' | 'ge'
	;
SLASHSLASH
	:	'//';
SLASH	:	'/';
 
AT	:	'@';
DOLLAR	:	'$';
COMMA	:	',';
LPAREN	:	'(';
RPAREN	:	')';
LBRACKET:	'[';
RBRACKET:	']';
QM	:	'?';
DOTDOT	:	'..';
DOT	:	'.';

STAR	:	'*';
FOR	:	'for';
IN	:	'in';
RETURN	:	'return';
SOME	:	'some';
EVERY	:	'every';
SATISFIES
	:	'satisfies';
IF	:	'if';
THEN	:	'then';
ELSE	:	'else';
AND	:	'and';
OR	:	'or';
PLUS	:	'+';
MINUS	:	'-';
TO	:	'to';
UNION	:	'union' | '|';
INTERSECT
	:	'intersect';
EXCEPT	:	'except';
DIV	:	'div';
IDIV	:	'idiv';
MOD	:	'mod';
INSTANCE
	:	'instance'; 
OF	:	'of';
AS	:	'as';
IS	:	'is';
TREAT	:	'treat';
CAST	:	'cast';
CASTABLE:	'castable';

/* double colons added to tokens for the forward/backwards
   axis in order to fix lexer bug where expressions such as
   //self::a/b were considered "invalid" */

// forward axis specifiers
CHILD	:	'child::';
DESCENDANT
	:	'descendant::';
ATTRIBUTE
	:	'attribute::';


SELF	:	'self::';

DESCENDANT_OR_SELF
	:	'descendant-or-self::'; 


FOLLOWING_SIBLING
	:	'following-sibling::';
FOLLOWING
	:	'following::';
NAMESPACE
	:	'namespace::';

// reverse axis specifiers
PARENT	:	'parent::';
ANCESTOR:	'ancestor::';
PRECEDING_SIBLING
	:	'preceding-sibling::';
PRECEDING
	:	'preceding::';
ANCESTOR_OR_SELF
	:	'ancestor-or-self';

	

EMPTY_SEQUENCE	
	: 	'empty-sequence';
ITEM	:	'item';

NODE	:	'node';
DOCNODE	:	'document-node';
TEXT	:	'text';
COMMENT :	'comment';
PI	:	'processing-instruction';
SCHEMA_ATTR
	:	'schema-attribute';
SCHEMA_ELEM
	:	'schema-element';

ELEMENT	:	'element';
	
	
LCHEVRON:	'<<';
RCHEVRON:	'>>';



	
	
QNAME	
	:	NCNAME (':' NCNAME)?
	;
			

fragment CHAR
	: ('\u0009' | '\u000A' | '\u000D' | '\u0020'..'\uD7FF' | '\uE000'..'\uFFFD' )
	;

fragment DIGITS 	   
	: ('0'..'9')+
	;

fragment NCNAMESTARTCHAR
	: ('A'..'Z') | '_' | ('a'..'z') | ('\u00C0'..'\u00D6') | ('\u00D8'..'\u00F6') | ('\u00F8'..'\u02FF') | ('\u0370'..'\u037D') | ('\u037F'..'\u1FFF') | ('\u200C'..'\u200D') | ('\u2070'..'\u218F') | ('\u2C00'..'\u2FEF') | ('\u3001'..'\uD7FF') | ('\uF900'..'\uFDCF') | ('\uFDF0'..'\uFFFD')
	;
	
fragment NCNAMECHAR
	:   	NCNAMESTARTCHAR | '-' | '.' | '0'..'9' | '\u00B7' | '\u0300'..'\u036F' | '\u203F'..'\u2040'
	;
	
fragment NAMECHAR	   
	:   ':' 
	| NCNAMECHAR
	;
	
fragment NAMESTARTCHAR
	:  ':' 
	| NCNAMESTARTCHAR
	;
	
	
fragment NCNAME	           
	:  NCNAMESTARTCHAR NCNAMECHAR*
	;	

NCNAME_COLON_STAR
	: NCNAME ':' '*'
	;
STAR_COLON_NCNAME
	: '*' ':' NCNAME;

NUMERICLITERAL 	   
	: ( ('.' DIGITS) |(DIGITS ('.' ('0'..'9')*)?)) (('e'|'E') ('+'|'-')? DIGITS)?
	;
		

fragment QUOTE	           
	: '"'
	;
	
fragment APOS		   
	: '\''
	;
	
fragment ESCAPEQUOTE 	   
	: QUOTE QUOTE
	;
	
	
fragment ESCAPEAPOS 	   
	: APOS APOS
	;
	
fragment CHARNOQUOTE	   
	: ~(~CHAR | QUOTE)
	;
	
	
fragment CHARNOAPOS	   
	: ~(~CHAR | APOS)
	;


STRINGLITERAL		   
	: (QUOTE (ESCAPEQUOTE | CHARNOQUOTE)* QUOTE) 
	| (APOS  (ESCAPEAPOS | CHARNOAPOS)* APOS)
	;
			 

/* 
This lexer rule for comments handles multiline, nested comments
*/
COMMENT_CONTENTS
        :       '(:'
                {
                        $channel=98;
                }
                (       ~('('|':')
                        |       ('(' ~':') => '('
                        |       (':' ~')') => ':'
                        |       COMMENT_CONTENTS
                )*
                ':)'
        ;


WS		
	: (' '|'\r'|'\t'|'\u000C'|'\n')+ {$channel = HIDDEN;}
	;


xPath 	   
	: expr EOF
	;

expr
	: exprSingle (COMMA exprSingle)* 
	;

exprSingle 	
	: forExpr 
	| quantifiedExpr
	| ifExpr 
	| orExpr
	| SLASH
	;

					

forExpr	   
	: FOR varRef IN exprSingle (COMMA varRef IN exprSingle)* RETURN exprSingle
	;
	
quantifiedExpr 	   
	: (SOME|EVERY) varRef IN exprSingle (COMMA varRef IN exprSingle)* SATISFIES exprSingle
	;

ifExpr 	   
	: 
	IF LPAREN expr RPAREN THEN exprSingle ELSE exprSingle
	;


orExpr 	
	: andExpr (OR andExpr)*
	;

andExpr 
	: comparisonExpr (AND comparisonExpr)* 
	;
	
comparisonExpr 	   
	:  rangeExpr 
	( (	  valueComp
		| generalComp
		| nodeComp)
	rangeExpr )?
	;
	
	
rangeExpr 	   
	: additiveExpr (TO additiveExpr )?
	;

additiveExpr 	   
	: multiplicativeExpr ( (PLUS|MINUS) multiplicativeExpr )*
	;

multiplicativeExpr 	   
	: unionExpr ( (STAR|DIV|IDIV|MOD) unionExpr )*
	;

unionExpr 	   
	: intersectExceptExpr ( UNION intersectExceptExpr )*
	;
	
intersectExceptExpr 	   
	: instanceofExpr ( (INTERSECT|EXCEPT) instanceofExpr )*
	;
instanceofExpr 	   
	: treatExpr ( INSTANCE OF sequenceType )?
	;
treatExpr 	   
	: castableExpr ( TREAT AS sequenceType )?
	;
	
castableExpr 	   
	: castExpr (CASTABLE AS singleType )?
	;
castExpr 	   
	: unaryExpr (CAST AS singleType )?
	;

unaryExpr 	   
	:    	((PLUS|MINUS)?) valueExpr
	;
	
valueExpr 	
	: pathExpr
	;
	
generalComp
	: GENERAL_COMP
	;

valueComp 	   
	: VALUE_COMP
	;
	
nodeComp
	: IS | LCHEVRON | RCHEVRON
	;

				
pathExpr
	: (SLASH relativePathExpr)
	| (SLASHSLASH relativePathExpr)
	| relativePathExpr 
	;
							
				
relativePathExpr
	: stepExpr ((SLASH|SLASHSLASH) stepExpr)* 
	;
	
stepExpr
	:  filterExpr | axisStep 
	;
	
axisStep 	   
	: (reverseStep | forwardStep) predicateList
	;

forwardStep 	   
	: (forwardAxis nodeTest) 
	| abbrevForwardStep
	;

forwardAxis 
	: (CHILD)
	| (DESCENDANT)
	| (ATTRIBUTE)
	| (SELF)
	| (DESCENDANT_OR_SELF)
	| (FOLLOWING_SIBLING)
	| (FOLLOWING)
	| (NAMESPACE)
	;
	
	
abbrevForwardStep 	   
	: AT? nodeTest
	;


reverseStep 	   
	: (reverseAxis nodeTest) 
	| abbrevReverseStep
	;
	
reverseAxis 	   
	: (PARENT)
	| (ANCESTOR)
	| (PRECEDING_SIBLING)
	| (PRECEDING)
	| (ANCESTOR_OR_SELF)

	;

abbrevReverseStep 	   
	: DOTDOT
	;
	
nodeTest 	   
	:  kindTest | nameTest
	;
	
	
nameTest 	   
	:  wildcard | QNAME  
	;

					
filterExpr 	   
	: primaryExpr predicateList
	;
	
	
predicateList 	   
	: predicate*
	;
	
predicate 	   
	: LBRACKET expr RBRACKET
	;
	
primaryExpr 	   
	: literal | varRef | parenthesizedExpr | contextItemExpr | functionCall
	;
	
literal 
	: NUMERICLITERAL | STRINGLITERAL  
	;
	
varRef 	   
	: DOLLAR varName
	;
	
varName
	: QNAME
	;


parenthesizedExpr  
	: LPAREN expr? RPAREN
	;
	
contextItemExpr    
	: DOT
	;

/* we need to be explicit about the TEXT,COMMENT,NODE,...
   tokens because these are fct calls also, but the lexer won't
   make QNAME tokens for "text()", "comment()", etc, and consequently
   the parser will treat these function calls as forwardSteps */
functionCall 	   
	: (QNAME|TEXT|COMMENT|NODE) LPAREN expr? RPAREN
	;
	

singleType 	   
	: atomicType QM? 
	;


sequenceType 	   
	: (EMPTY_SEQUENCE LPAREN RPAREN)
	//(itemType (occurrenceIndicator?) => occurrenceIndicator) 
	// the statement above was inconsistent with the XPath 2.0 EBNF
	// i.e., 'a instance of xs:integer' was not recognized as valid
	// this fix below attemps to remedy this problem- however the fix
	// is not pretty because of LL(*) ambiguities
	| (itemType occurrenceIndicator) => itemType occurrenceIndicator
	| itemType 
	;
	
	
occurrenceIndicator 	   
	: QM | STAR | PLUS
	;

itemType 	   
	: kindTest | (ITEM LPAREN RPAREN) | atomicType;
	
atomicType 	   
	: QNAME;
	
kindTest 	   
	: documentTest
	| elementTest
	| attributeTest
	| schemaElementTest
	| schemaAttributeTest
	| pITest
	| commentTest
	| textTest
	| anyKindTest
	;

anyKindTest 	   
	: NODE LPAREN RPAREN
	;
	
documentTest 	   
	: DOCNODE LPAREN (elementTest | schemaElementTest)? RPAREN
	;
	
textTest 	  
	 : TEXT LPAREN RPAREN
	 ;
	 
commentTest 	   
	: COMMENT LPAREN RPAREN
	;
	
pITest 	   
	: PI LPAREN (NCNAME | STRINGLITERAL)? RPAREN
	;
	
attributeTest 	   
	:    	ATTRIBUTE LPAREN (attribNameOrWildcard (COMMA typeName)?)? RPAREN
	;
	
attribNameOrWildcard 	   
	:    	attributeName | STAR
	;
	
schemaAttributeTest 	   
	: SCHEMA_ATTR LPAREN attributeDeclaration RPAREN
	;

attributeDeclaration 
	: attributeName
	;
	

elementTest 	   
	: ELEMENT LPAREN (elementNameOrWildcard (COMMA typeName QM?)?)? RPAREN;
	
elementNameOrWildcard
	:    	elementName | STAR
	;
	
elementName
	: QNAME
	;

	
typeName
	: QNAME
	;
	
attributeName
	: QNAME
	;


schemaElementTest 	   
	: SCHEMA_ELEM LPAREN elementDeclaration RPAREN
	;	

	
elementDeclaration
	: elementName
	;
	

wildcard	   
	: NCNAME_COLON_STAR
	| STAR_COLON_NCNAME
	| STAR
	; 	


