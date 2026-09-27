parser grammar rexParser;

options { tokenVocab = rexLexer; }

grammar_  : prolog syntaxDefinition lexicalDefinition? encore? EOF ;
prolog   : processingInstruction* ;
processingInstruction : '<?' name ( WS_Space+ DirPIContents? )? '?>'
          /* ws: explicit */
	  ;
syntaxDefinition : syntaxProduction+ ;
syntaxProduction : name '::=' syntaxChoice option* ;
syntaxChoice : syntaxSequence ( ( '|' syntaxSequence )+ | ( '/' syntaxSequence )+ )? ;
syntaxSequence : syntaxItem* ;
syntaxItem : syntaxPrimary ( '?' | '*' | '+' )? ;
syntaxPrimary : nameOrString | '(' syntaxChoice ')' | processingInstruction ;
lexicalDefinition : '<?TOKENS?>' ( lexicalProduction | preference | delimiter | equivalence )* ;
lexicalProduction : ( name | '.' ) '?'? '::=' contextChoice option* ;
contextChoice : contextExpression ( '|' contextExpression )* ;
lexicalChoice : lexicalSequence ( '|' lexicalSequence )* ;
contextExpression : lexicalSequence ( '&' lexicalItem )? ;
lexicalSequence : | lexicalItem ( '-' lexicalItem | lexicalItem* ) ;
lexicalItem : lexicalPrimary ( '?' | '*' | '+' )? ;
lexicalPrimary : ( name | '.' ) | StringLiteral | '(' lexicalChoice ')' | '$' | charCode | charClass ;
nameOrString : name context? | StringLiteral context? ;
context  : CaretName ;
charCode : CharCode | unicode ;
unicode : Unicode ;
charClass : ( '[' | '[^' ) ( SetChar | SetCharCode | SetCharRange | SetCharCodeRange )+ ']'
          /* ws: explicit */
	  ;
option : '/*' WS_Space* name 'ws' ':' WS_Space* ( 'explicit' | 'definition' ) WS_Space* '*/'
          /* ws: explicit */
	  ;
preference : nameOrString ( '>>' nameOrString+ | '<<' nameOrString+ ) ;
delimiter : name '\\\\' nameOrString+ ;
equivalence : /* EquivalenceLookAhead */
    equivalenceCharRange '==' equivalenceCharRange ;
equivalenceCharRange : StringLiteral | '[' ( SetChar | SetCharCode | SetCharRange | SetCharCodeRange ) ']'
          /* ws: explicit */
	  ;
encore : '<?ENCORE?>' processingInstruction* ;
name : Name | WsLit | ExplicitLit | DefinitionLit ;

