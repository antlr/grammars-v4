// $antlr-format alignTrailingComments true, columnLimit 150, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine true, allowShortBlocksOnASingleLine true, minEmptyLines 0, alignSemicolons ownLine
// $antlr-format alignColons trailing, singleLineOverrulesHangingColon true, alignLexerCommands true, alignLabels true, alignTrailers true

lexer grammar BencodingLexer;

options {
    superClass = BencodingLexerBase;
}

INT_START: 'i';

INTEGER: '-'? [0-9]+;

STRING_START: [0-9]+ ':' {setStringLength();} -> skip, pushMode(StringMode);

LIST_START: 'l';

DICT_START: 'd';

END: 'e';

OTHER: .;

mode StringMode;

STRING: ({consumeStringChars()}? .)+ -> popMode;