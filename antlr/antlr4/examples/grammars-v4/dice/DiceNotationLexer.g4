/**
 * Copyright 2014-2019 the original author or authors
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

/** 
 * Dice notation lexical rules.
 */

// $antlr-format alignTrailingComments true, columnLimit 150, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine true, allowShortBlocksOnASingleLine true, minEmptyLines 0, alignSemicolons ownLine
// $antlr-format alignColons trailing, singleLineOverrulesHangingColon true, alignLexerCommands true, alignLabels true, alignTrailers true

lexer grammar DiceNotationLexer;

/**
 * Tokens.
 */

// Dice markers

DSEPARATOR: ( 'd' | 'D');

DIGIT: ('0' ..'9')+;

// Operation tokens

ADDOPERATOR: ( ADD | SUB);

MULTOPERATOR: ( MULT | DIV);

fragment ADD: '+';

fragment SUB: '-';

fragment MULT: '*';

fragment DIV: '/';

LPAREN: '(';

RPAREN: ')';

// Skippable tokens

WS: [\t\r\n]+ -> skip;