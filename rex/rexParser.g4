
// $antlr-format alignColons hanging, alignSemicolons hanging, alignTrailingComments true, allowShortBlocksOnASingleLine true
// $antlr-format allowShortRulesOnASingleLine false, columnLimit 150, maxEmptyLinesToKeep 1, minEmptyLines 1, reflowComments false, useTab false

parser grammar rexParser;

options {
    tokenVocab = rexLexer;
}

grammar_
    : prolog syntaxDefinition lexicalDefinition? encore? EOF
    ;

prolog
    : processingInstruction*
    ;

processingInstruction
    : OpenQu name (WS_Space+ (DirPIContents | WS_Space)*)? CloseQu
    /* ws: explicit */
    ;

syntaxDefinition
    : syntaxProduction+
    ;

syntaxProduction
    : name '::=' syntaxChoice option*
    ;

syntaxChoice
    : syntaxSequence (( '|' syntaxSequence)+ | ( '/' syntaxSequence)+)?
    ;

syntaxSequence
    : syntaxItem*
    ;

syntaxItem
    : syntaxPrimary (Quest | Star | '+')?
    ;

syntaxPrimary
    : nameOrString
    | '(' syntaxChoice ')'
    | processingInstruction
    ;

lexicalDefinition
    : '<?TOKENS?>' (lexicalProduction | preference | delimiter | equivalence)*
    ;

lexicalProduction
    : (name | '.') Quest? '::=' contextChoice option*
    ;

contextChoice
    : contextExpression ('|' contextExpression)*
    ;

lexicalChoice
    : lexicalSequence ('|' lexicalSequence)*
    ;

contextExpression
    : lexicalSequence ('&' lexicalSequence)?
    ;

lexicalSequence
    :
    | lexicalItem ( '-' lexicalItem | lexicalItem*)
    ;

lexicalItem
    : lexicalPrimary (Quest | Star | '+')?
    ;

lexicalPrimary
    : (name | '.')
    | StringLiteral
    | '(' lexicalChoice ')'
    | '$'
    | charCode
    | charClass
    ;

nameOrString
    : name context_?
    | StringLiteral context_?
    ;

context_
    : CaretName
    ;

charCode
    : CharCode
    | unicode
    ;

unicode
    : Unicode
    ;

charClass
    : ('[' | '[^') (SetChar | SetCharCode | SetCharRange | SetCharCodeRange | SetUnicode | SetUnicodeRange)+ ']'
    /* ws: explicit */
    ;

option
    : OptionStart WS_Space* (ExplicitLit | DefinitionLit) WS_Space* CloseMLComment
    /* ws: explicit */
    ;

preference
    : nameOrString ('>>' nameOrString+ | '<<' nameOrString+)
    ;

delimiter
    : name '\\\\' nameOrString+
    ;

equivalence
    : /* EquivalenceLookAhead */ equivalenceCharRange '==' equivalenceCharRange
    ;

equivalenceCharRange
    : StringLiteral
    | '[' (SetChar | SetCharCode | SetCharRange | SetCharCodeRange | SetUnicode | SetUnicodeRange) ']'
    /* ws: explicit */
    ;

encore
    : '<?ENCORE?>' processingInstruction*
    ;

name
    : Name
    | WsLit
    | ExplicitLit
    | DefinitionLit
    ;
