/*
 * MIT License
 *
 * Copyright (c) 2017 Matt DeFano
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:

 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.

 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

grammar HyperTalk;

// Start symbol accepting only well-formed HyperTalk scripts that consist of handlers, functions, whitespace and
// comments (representing scipts that are assignable to objects like buttons, fields and cards). Disallows statements or
// expressions that are not inside of a handler or function block.
script
    : handler script
    | function script
    | NEWLINE script
    | EOF
    ;

// Start symbol accepting any sequence of HyperTalk statements, expressions, whitespace and comments. Suitable when
// evaluating the message box or HyperTalk strings via the 'do' command and 'value of' function.
scriptlet
    : statement EOF
    | multilineScriptlet
    ;

multilineScriptlet
    : statement NEWLINE multilineScriptlet
    | statement EOF
    | NEWLINE multilineScriptlet
    | EOF
    ;

handler
    : 'on' handlerName NEWLINE+ statementList? 'end' handlerName
    | 'on' handlerName parameterList NEWLINE+ statementList? 'end' handlerName
    ;

function
    : 'function' ID NEWLINE+ statementList? 'end' ID
    | 'function' ID parameterList NEWLINE+ statementList? 'end' ID
    ;

handlerName
    : ID
    | commandName   // Handlers can take the name of a command keyword (other keywords are disallowed)
    ;

parameterList
    : ID
    | parameterList ',' ID
    ;

statementList
    : statement NEWLINE+ statementList
    | statement NEWLINE+
    ;

statement
    : commandStmnt
    | functionCall
    | messageStatement
    | expression
    | ifStatement
    | repeatStatement
    | globalStmnt
    | returnStmnt
    ;

globalStmnt
    : 'global' parameterList
    ;

returnStmnt
    : 'return' expression
    | 'return'
    ;

ifStatement
    : 'if' expression thenStatement
    ;

thenStatement
    : NEWLINE? 'then' statement
    | NEWLINE? 'then' statement NEWLINE? elseStatement?
    | NEWLINE? 'then' NEWLINE+ statementList? (elseStatement | 'end' 'if')
    ;

elseStatement
    : 'else' statement (NEWLINE+ 'end' 'if')?
    | 'else' NEWLINE+ statementList? 'end' 'if'
    ;

repeatStatement
    : 'repeat' repeatRange NEWLINE+ statementList 'end' 'repeat'
    | 'repeat' repeatRange NEWLINE+ 'end' 'repeat'
    ;

messageStatement
    : ID
    | ID listExpression
    ;

commandStmnt
    : 'add' expression 'to' expression
    | 'answer' expression 'with' factor 'or' factor 'or' factor
    | 'answer' expression 'with' factor 'or' factor
    | 'answer' expression 'with' factor
    | 'answer' expression
    | 'ask' expression 'with' expression
    | 'ask' expression
    | 'beep'
    | 'beep' expression
    | 'choose' toolExpression 'tool'?
    | 'choose' 'tool' toolExpression
    | 'click' 'at' listExpression
    | 'click' 'at' listExpression 'with' listExpression
    | 'close' 'file' expression
    | 'convert' container 'to' convertible
    | 'convert' container 'from' convertible 'to' convertible
    | 'convert' expression 'to' convertible
    | 'convert' expression 'from' convertible 'to' convertible
    | 'create' 'menu' expression
    | 'delete' expression
    | 'dial' expression
    | 'disable' expression
    | 'divide' expression 'by' expression
    | 'do' expression
    | 'domenu' expression
    | 'drag' 'from' listExpression 'to' listExpression
    | 'drag' 'from' listExpression 'to' listExpression 'with' listExpression
    | 'edit' 'the'? 'script' of expression
    | 'enable' expression
    | 'exit' handlerName
    | 'exit' 'repeat'
    | 'exit' 'to' 'hypercard'
    | 'export' 'paint' 'to' 'file' expression
    | 'find' expression? 'international'? expression of expression of 'marked' cards
    | 'find' expression? 'international'? expression of expression
    | 'find' expression? 'international'? expression of 'marked' cards
    | 'find' expression? 'international'? expression
    | 'get' expression
    | 'go' 'to'? expression 'with' 'visual' expression
    | 'go' 'to'? expression
    | 'go' 'back'
    | 'go' 'back' 'with' 'visual' expression
    | 'hide' expression
    | 'hide' card picture
    | 'hide' background picture
    | 'hide' picture of expression
    | 'hide' 'titlebar'
    | 'import' 'paint' 'from' 'file' expression
    | 'lock' 'screen'
    | 'multiply' expression 'by' expression
    | 'next' 'repeat'
    | 'open' 'file' expression
    | 'pass' handlerName
    | 'play' musicExpression
    | 'pop' card
    | 'push' card
    | 'push' expression
    | 'put' listExpression
    | 'put' listExpression preposition expression
    | 'read' 'from' 'file' expression
    | 'read' 'from' 'file' expression 'for' expression
    | 'read' 'from' 'file' expression 'at' expression 'for' expression
    | 'read' 'from' 'file' expression 'until' expression
    | 'reset' 'the'? 'menubar'
    | 'reset' 'paint'
    | 'select' 'empty'
    | 'select' 'text' of expression
    | 'select' 'before' 'text' of expression
    | 'select' 'after' 'text' of expression
    | 'select' expression
    | 'select' 'before' expression
    | 'select' 'after' expression
    | 'set' property 'to' propertyValue
    | 'send' listExpression 'to' expression
    | 'show' expression
    | 'show' card picture
    | 'show' background picture
    | 'show' picture of expression
    | 'show' 'titlebar'
    | 'sort' sortChunkType expression sortDirection sortStyle
    | 'sort' sortChunkType expression sortDirection sortStyle 'by' expression
    | 'sort' sortDirection sortStyle 'by' expression
    | 'sort' 'this'? 'stack' sortDirection sortStyle 'by' expression
    | 'sort' 'the'? cards (of 'this' 'stack')? sortDirection sortStyle 'by' expression
    | 'sort' 'the'? 'marked' cards (of 'this' 'stack')? sortDirection sortStyle 'by' expression
    | 'sort' expression sortDirection sortStyle 'by' expression
    | 'sort' 'the'? cards of expression sortDirection sortStyle 'by' expression
    | 'sort' 'the'? 'marked' cards of expression sortDirection sortStyle 'by' expression
    | 'speak' expression
    | 'speak' expression 'with' gender=('male'|'female'|'neuter'|'robotic') 'voice'
    | 'speak' expression 'with' 'voice' expression
    | 'subtract' expression 'from' expression
    | 'type' expression
    | 'type' expression 'with' ('commandkey' | 'cmdkey')
    | 'unlock' 'screen'
    | 'unlock' 'screen' 'with' 'visual' expression
    | 'visual' expression
    | 'wait' expression timeUnit
    | 'wait' 'for' expression timeUnit
    | 'wait' 'until' expression
    | 'wait' 'while' expression
    | 'write' expression 'to' 'file' expression
    | 'write' expression 'to' 'file' expression 'at' ('eof' | 'end')
    | 'write' expression 'to' 'file' expression 'at' expression
    ;

convertible
    : conversionFormat
    | conversionFormat 'and' conversionFormat
    ;

conversionFormat
    : seconds
    | 'dateitems'
    | length 'date'
    | length 'time'
    ;

length
    : ('english' | 'long')
    | ('abbreviated' | 'abbrev' | 'abbr')
    | 'short'
    |
    ;

sortDirection
    : 'ascending'
    | 'descending'
    |
    ;

sortChunkType
    : 'the'? line of
    | 'the'? item of
    | 'the'? word of
    | 'the'? character of
    |
    ;

sortStyle
    : 'text'
    | 'numeric'
    | 'international'
    | 'datetime'
    |
    ;

repeatRange
    : duration
    | count
    | 'with' ID '=' range
    | 'forever'
    |
    ;

duration
    : 'until' expression
    | 'while' expression
    ;

count
    : 'for' expression 'times'
    | 'for' expression
    | expression 'times'
    | expression
    ;

range
    : expression 'down' 'to' expression
    | expression 'to' expression
    ;

chunk
    : chunk chunk
    | ordinal character of
    | character expression 'to' expression of
    | character expression of
    | ordinal word of
    | word expression 'to' expression of
    | word expression of
    | ordinal item of
    | item expression 'to' expression of
    | item expression of
    | ordinal line of
    | line expression 'to' expression of
    | line expression of
    ;

menu
    : 'menu' factor
    | ordinal 'menu'
    ;

menuItem
    : 'menuitem' factor of menu
    | ordinal 'menuitem' of menu
    ;

property
    : partProperty
    | globalProperty
    ;

globalProperty
    : 'the'? propertyName
    ;

partProperty
    : 'the'? propertyName of factor
    | 'the'? length propertyName of factor
    ;

part
    : message
    | card 'part' factor
    | background 'part' factor
    | 'me'
    | buttonPart
    | fieldPart
    | bkgndPart
    | cardPart
    | stackPart
    ;

stackPart
    : 'this' stack
    | stack factor
    ;

buttonPart
    : card? button 'id' factor
    | background button 'id' factor
    | card? button factor
    | background button factor
    | ordinal card? button
    | ordinal background button
    | buttonPart of cardPart
    ;

fieldPart
    : card field 'id' factor
    | background? field 'id' factor
    | card field factor
    | background? field factor
    | ordinal card field
    | ordinal background? field
    | fieldPart of cardPart
    ;

cardPart
    : 'this' card
    | card 'id' factor
    | position card
    | ordinal card
    | card factor
    | cardPart of bkgndPart
    ;

bkgndPart
    : 'this' background
    | background 'id' factor
    | background factor
    | ordinal background
    | position background
    ;

listExpression
    : expression
    | expression ',' listExpression
    ;

expression
    : factor
    | 'not' expression
    | '-' expression
    | op=('there is a'|'there is an'|'there is no'|'there is not a'|'there is not an') expression
    | expression '^' expression
    | expression op=('mod'| 'div'| '/'| '*') expression
    | expression op=('+'| '-') expression
    | expression op=('&&'| '&') expression
    | expression op=('>='|'<='|'≤'|'≥'|'<'|'>'|'contains'|'is in'|'is not in'|'is a'|'is an'|'is not a'|'is not an'|'is within'|'is not within') expression
    | expression op=('='|'is not'|'is'|'<>'|'≠') expression
    | expression 'and' expression
    | expression 'or' expression
    ;

factor
    : literal
    | '-' literal
    | '(' expression ')'
    | effectExpression
    | functionCall
    | container
    | chunk factor
    ;

container
    : ID
    | 'the'? 'selection'
    | 'target'
    | property
    | menu
    | menuItem
    | message
    | part
    | chunk container
    ;

musicExpression
    : expression expression
    | expression 'tempo' expression expression
    | expression 'tempo' expression
    | expression
    ;

toolExpression
    : 'text'
    | 'select'
    | 'field'
    | 'button'
    | 'line'
    | ('reg' | 'regular')? ('poly' | 'polygon')
    | 'round'? ('rect' | 'rectangle')
    | 'spray' 'can'?
    | expression
    ;

effectExpression
    : 'effect'? effect
    | 'effect'? effect 'to' image
    | 'effect'? effect speed
    | 'effect'? effect speed 'to' image
    ;

functionCall
    : builtInFunc
    | ID '(' listExpression? ')'
    ;

builtInFunc
    : 'the' zeroArgFunc
    | 'the'? singleArgFunc of factor
    | singleArgFunc '(' listExpression ')'
    | multiArgFunc '(' listExpression ')'
    ;

zeroArgFunc
    : 'mouse'
    | 'mouseloc'
    | 'result'
    | ('commandkey' | 'cmdkey')
    | 'shiftkey'
    | 'optionkey'
    | 'ticks'
    | seconds
    | length 'time'
    | length 'date'
    | 'tool'
    | 'mouseclick'
    | 'number' 'of' card? 'parts'
    | 'number' 'of' background 'parts'
    | 'number' 'of' card? button
    | 'number' 'of' background button
    | 'number' 'of' card field
    | 'number' 'of' background? field
    | 'number' 'of' 'menus'
    | 'number' 'of' cards (of 'this' 'stack')?
    | 'number' 'of' 'marked' cards
    | 'number' 'of' background (of 'this' 'stack')?
    | 'menus'
    | 'diskspace'
    | 'params'
    | 'paramcount'
    | 'sound'
    | 'selectedtext'
    | 'selectedchunk'
    | 'selectedfield'
    | 'selectedline'
    | 'target'
    | 'speech'
    | 'voices'
    | 'clicktext'
    | 'mouseh'
    | 'mousev'
    | 'screenrect'
    | 'clickloc'
    | 'clickh'
    | 'clickv'
    | 'foundchunk'
    | 'foundfield'
    | 'foundline'
    | 'foundtext'
    ;

singleArgFunc
    : 'average'
    | 'min'
    | 'max'
    | 'sum'
    | 'number' 'of' character
    | 'number' 'of' word
    | 'number' 'of' item
    | 'number' 'of' line
    | 'number' 'of' 'menuitems'
    | 'number' 'of' cards
    | 'number'
    | 'random'
    | 'sqrt'
    | 'trunc'
    | 'sin'
    | 'cos'
    | 'tan'
    | 'atan'
    | 'exp'
    | 'exp1'
    | 'exp2'
    | 'ln'
    | 'ln1'
    | 'log2'
    | 'abs'
    | 'chartonum'
    | 'numtochar'
    | 'value'
    | 'length'
    | 'diskspace'
    | 'param'
    ;

multiArgFunc
    : singleArgFunc
    | 'annuity'
    | 'compound'
    | 'offset'
    ;

literal
    : constant
    | modifierKey
    | mouseState
    | knownType
    | findType
    | LITERAL
    ;

preposition
    : 'before'
    | 'after'
    | 'into'
    ;

constant
    : cardinalValue
    | 'empty'
    | 'pi'
    | 'quote'
    | 'return'
    | 'space'
    | 'tab'
    | 'formfeed'
    | 'linefeed'
    | 'comma'
    | 'colon'
    ;

cardinalValue
    : 'zero'
    | 'one'
    | 'two'
    | 'three'
    | 'four'
    | 'five'
    | 'six'
    | 'seven'
    | 'eight'
    | 'nine'
    | 'ten'
    ;

ordinal
    : 'the'? ordinalValue
    ;

ordinalValue
    : 'first'
    | 'second'
    | 'third'
    | 'fourth'
    | 'fifth'
    | 'sixth'
    | 'seventh'
    | 'eighth'
    | 'ninth'
    | 'tenth'
    | ('mid' | 'middle')
    | 'last'
    | 'any'
    ;

mouseState
    : 'up'
    | 'down'
    ;

modifierKey
    : 'commandkey'
    | 'cmdkey'
    | 'optionkey'
    | 'shiftkey'
    ;

knownType
    : 'number'
    | 'integer'
    | 'point'
    | 'rect'
    | 'rectangle'
    | 'date'
    | 'logical'
    | 'boolean'
    | 'bool'
    ;

findType
    : 'word'
    | 'chars'
    | 'whole'
    | 'string'
    ;

// Not all properties need to be enumerated here, only those sharing a name with another keyword.
propertyName
    : 'marked'
    | 'selectedtext'
    | 'selectedchunk'
    | 'selectedfield'
    | 'selectedline'
    | 'number'
    | 'id'
    | 'rect'
    | 'rectangle'
    | 'bottom'
    | 'left'
    | 'right'
    | 'top'
    | 'center'
    | 'scroll'
    | 'script'
    | ID
    ;

// Not all property values need to be enumerated here, only known values sharing a name with another keyword.
propertyValue
    : 'plain'
    | 'menu'
    | 'bottom'
    | 'left'
    | 'right'
    | 'top'
    | 'center'
    | listExpression
    ;

commandName
    : 'answer'
    | 'ask'
    | 'put'
    | 'get'
    | 'set'
    | 'send'
    | 'wait'
    | 'sort'
    | 'go'
    | 'enable'
    | 'disable'
    | 'read'
    | 'write'
    | 'hide'
    | 'show'
    | 'add'
    | 'subtract'
    | 'multiply'
    | 'divide'
    | 'choose'
    | 'click'
    | 'drag'
    | 'type'
    | 'lock'
    | 'unlock'
    | 'pass'
    | 'domenu'
    | 'visual'
    | 'reset'
    | 'create'
    | 'delete'
    | 'play'
    | 'dial'
    | 'beep'
    | 'open'
    | 'close'
    | 'select'
    | 'find'
    | 'import'
    | 'export'
    ;

picture
    : 'picture'
    | 'pict'
    ;

seconds
    : 'seconds'
    | 'secs'
    | 'second'
    | 'sec'
    ;

speed
    : 'fast'
    | ('slow' | 'slowly')
    | 'very' 'fast'
    | 'very' ('slow' | 'slowly')
    ;

image
    : 'black'
    | 'card'
    | ('gray' | 'grey')
    | 'inverse'
    | 'white'
    ;

effect
    : 'dissolve'
    | 'barn' 'door' 'open'
    | 'barn' 'door' 'close'
    | 'checkerboard'
    | 'iris' 'open'
    | 'iris' 'close'
    | 'plain'
    | 'push' 'up'
    | 'push' 'down'
    | 'push' 'left'
    | 'push' 'right'
    | 'scroll' 'down'
    | 'scroll' 'up'
    | 'scroll' 'left'
    | 'scroll' 'right'
    | 'shrink' 'to' 'top'
    | 'shrink' 'to' 'center'
    | 'shrink' 'to' 'bottom'
    | 'stretch' 'from' 'top'
    | 'stretch' 'from' 'center'
    | 'stretch' 'from' 'bottom'
    | 'venetian' 'blinds'
    | 'wipe' 'up'
    | 'wipe' 'down'
    | 'wipe' 'left'
    | 'wipe' 'right'
    | 'zoom' 'in'
    | 'zoom' 'out'
    | 'zoom' 'open'
    | 'zoom' 'close'
    ;

timeUnit
    : 'ticks'
    | 'tick'
    | seconds
    ;

position
    : 'the'? 'next'
    | 'the'? ('prev' | 'previous')
    | 'this'
    ;

message
    : 'the'? ('message' | 'msg') ('box' | 'window' | )
    ;

cards
    : 'cards'
    | 'cds'
    ;

card
    : 'card'
    | 'cd'
    ;

background
    : 'background'
    | 'backgrounds'
    | 'bkgnd'
    | 'bkgnds'
    | 'bg'
    | 'bgs'
    ;

button
    : 'button'
    | 'buttons'
    | 'btn'
    | 'btns'
    ;

field
    : 'field'
    | 'fields'
    | 'fld'
    | 'flds'
    ;

stack
    : 'stack'
    ;

character
    : 'character'
    | 'characters'
    | 'char'
    | 'chars'
    ;

word
    : 'word'
    | 'words'
    ;

line
    : 'line'
    | 'lines'
    ;

item
    : 'item'
    | 'items'
    ;

of
    : 'of'
    | 'from'
    | 'in'
    ;

ID
    : (ALPHA (ALPHA | DIGIT)*)
    ;

BREAK
    : ('|' | '¬') WHITESPACE? COMMENT? WHITESPACE? NEWLINE -> skip
    ;

LITERAL
    : STRING_LITERAL
    | NUMBER_LITERAL
    ;

INTEGER_LITERAL
    : DIGIT+
    ;

NUMBER_LITERAL
    : INTEGER_LITERAL
    | '.' INTEGER_LITERAL
    | INTEGER_LITERAL '.'
    | INTEGER_LITERAL '.' INTEGER_LITERAL
    ;

STRING_LITERAL
    : '"' ~('"' | '\r' | '\n')* '"'
    ;

ALPHA
    : ('a' .. 'z' | 'A' .. 'Z')+
    ;

DIGIT
    : ('0' .. '9')+
    ;

COMMENT
    : ('--' ~('\r' | '\n' | '|')*) -> skip
    ;

NEWLINE
    : ('\n' | '\r')+
    ;

WHITESPACE
    : (' ' | '\t')+ -> skip
    ;

UNLEXED_CHAR
    : .
    ;
