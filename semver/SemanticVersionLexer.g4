// $antlr-format alignTrailingComments true, columnLimit 150, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine true, allowShortBlocksOnASingleLine true, minEmptyLines 0, alignSemicolons ownLine
// $antlr-format alignColons trailing, singleLineOverrulesHangingColon true, alignLexerCommands true, alignLabels true, alignTrailers true

lexer grammar SemanticVersionLexer;

options {
    caseInsensitive = true;
}

fragment DIGIT          : [0-9];
fragment POSITIVE_DIGIT : [1-9];
fragment LETTER         : [a-z];

DASH : '-';
PLUS : '+';
DOT  : '.';

//most common pre-release "modifiers"
ALPHA        : 'alpha';
BETA         : 'beta';
RC           : 'rc' | 'release' ('-' | '.') 'candidate';
SNAPSHOT     : 'snapshot';
PREVIEW      : 'p' | 'pre' | 'preview';
DEV          : 'dev' | 'devel' | 'development';
MILESTONE    : 'mt' | 'milestone';
DAILY        : 'daily';
NIGHTLY      : 'nightly';
BUILD        : 'bld' | 'build';
TEST         : 'test';
EXPERIMENTAL : 'experimental';

NUMBER: '0' | POSITIVE_DIGIT DIGIT*;

IDENTIFIER: LETTER+;
