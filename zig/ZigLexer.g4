/*
Zig language grammar.
The MIT License (MIT).

Copyright (c) 2025, Michał Lorek.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
lexer grammar ZigLexer;

ADDRSPACE
   : 'addrspace'
   ;

ALIGN
   : 'align'
   ;

ALLOWZERO
   : 'allowzero'
   ;

AND
   : 'and'
   ;

ANYFRAME
   : 'anyframe'
   ;

ANYTYPE
   : 'anytype'
   ;

ASM
   : 'asm'
   ;

BREAK
   : 'break'
   ;

CALLCONV
   : 'callconv'
   ;

CATCH
   : 'catch'
   ;

COMPTIME
   : 'comptime'
   ;

CONST
   : 'const'
   ;

CONTINUE
   : 'continue'
   ;

DEFER
   : 'defer'
   ;

ELSE
   : 'else'
   ;

ENUM
   : 'enum'
   ;

ERRDEFER
   : 'errdefer'
   ;

ERROR
   : 'error'
   ;

EXPORT
   : 'export'
   ;

EXTERN
   : 'extern'
   ;

FN
   : 'fn'
   ;

FOR
   : 'for'
   ;

IF
   : 'if'
   ;

INLINE
   : 'inline'
   ;

LINKSECTION
   : 'linksection'
   ;

NOALIAS
   : 'noalias'
   ;

NOINLINE
   : 'noinline'
   ;

NOSUSPEND
   : 'nosuspend'
   ;

OPAQUE
   : 'opaque'
   ;

OR
   : 'or'
   ;

ORELSE
   : 'orelse'
   ;

PACKED
   : 'packed'
   ;

PUB
   : 'pub'
   ;

RESUME
   : 'resume'
   ;

RETURN
   : 'return'
   ;

STRUCT
   : 'struct'
   ;

SUSPEND
   : 'suspend'
   ;

SWITCH
   : 'switch'
   ;

TEST
   : 'test'
   ;

THREADLOCAL
   : 'threadlocal'
   ;

TRY
   : 'try'
   ;

UNION
   : 'union'
   ;

UNREACHABLE
   : 'unreachable'
   ;

VAR
   : 'var'
   ;

VOLATILE
   : 'volatile'
   ;

WHILE
   : 'while'
   ;

WHITESPACE
   : [ \t\r\n]+ -> channel (HIDDEN)
   ;

AMPERSAND
   : '&'
   ;

AMPERSANDEQUAL
   : '&='
   ;

ASTERISK
   : '*'
   ;

ASTERISK2
   : '**'
   ;

ASTERISKEQUAL
   : '*='
   ;

ASTERISKPERCENT
   : '*%'
   ;

ASTERISKPERCENTEQUAL
   : '*%='
   ;

ASTERISKPIPE
   : '*|'
   ;

ASTERISKPIPEEQUAL
   : '*|='
   ;

CARET
   : '^'
   ;

CARETEQUAL
   : '^='
   ;

COLON
   : ':'
   ;

COMMA
   : ','
   ;

DOT
   : '.'
   ;

DOT2
   : '..'
   ;

DOT3
   : '...'
   ;

DOTASTERISK
   : '.*'
   ;

DOTQUESTIONMARK
   : '.?'
   ;

EQUAL
   : '='
   ;

EQUALEQUAL
   : '=='
   ;

EQUALRARROW
   : '=>'
   ;

EXCLAMATIONMARK
   : '!'
   ;

EXCLAMATIONMARKEQUAL
   : '!='
   ;

LARROW
   : '<'
   ;

LARROW2
   : '<<'
   ;

LARROW2EQUAL
   : '<<='
   ;

LARROW2PIPE
   : '<<|'
   ;

LARROW2PIPEEQUAL
   : '<<|='
   ;

LARROWEQUAL
   : '<='
   ;

LBRACE
   : '{'
   ;

LBRACKET
   : '['
   ;

LPAREN
   : '('
   ;

MINUS
   : '-'
   ;

MINUSEQUAL
   : '-='
   ;

MINUSPERCENT
   : '-%'
   ;

MINUSPERCENTEQUAL
   : '-%='
   ;

MINUSPIPE
   : '-|'
   ;

MINUSPIPEEQUAL
   : '-|='
   ;

MINUSRARROW
   : '->'
   ;

PERCENT
   : '%'
   ;

PERCENTEQUAL
   : '%='
   ;

PIPE
   : '|'
   ;

PIPE2
   : '||'
   ;

PIPEEQUAL
   : '|='
   ;

PLUS
   : '+'
   ;

PLUS2
   : '++'
   ;

PLUSEQUAL
   : '+='
   ;

PLUSPERCENT
   : '+%'
   ;

PLUSPERCENTEQUAL
   : '+%='
   ;

PLUSPIPE
   : '+|'
   ;

PLUSPIPEEQUAL
   : '+|='
   ;

LETTERC
   : 'c'
   ;

QUESTIONMARK
   : '?'
   ;

RARROW
   : '>'
   ;

RARROW2
   : '>>'
   ;

RARROW2EQUAL
   : '>>='
   ;

RARROWEQUAL
   : '>='
   ;

RBRACE
   : '}'
   ;

RBRACKET
   : ']'
   ;

RPAREN
   : ')'
   ;

SEMICOLON
   : ';'
   ;

SLASH
   : '/'
   ;

SLASHEQUAL
   : '/='
   ;

TILDE
   : '~'
   ;

Container_doc_comment
   : '//!' ~ [\n\r]*
   ; //-> channel(HIDDEN);

Doc_comment
   : '///' ~ [\n\r]*
   ; //-> channel(HIDDEN);

Line_comment
   : '//' ~ [/!] ~ [\n\r]* -> channel (HIDDEN)
   ;

IDENTIFIER
   : Letter LetterOrDigit*
   ;

fragment LetterOrDigit
   : Letter
   | [0-9]
   ;

fragment Letter
   : [a-zA-Z$_\-]
   | ~ [\u0000-\u007F\uD800-\uDBFF]
   | [\uD800-\uDBFF] [\uDC00-\uDFFF]
   ;

STRINGLITERAL
   : '"' .*? '"'
   ;

CHAR_LITERAL
   : '\'' .*? '\''
   ;

INTEGER
   : [0-9] ([0-9_]* [0-9])?
   ;

FLOAT
   : [0-9]* '.' [0-9]+ ([eE] [+-]? [0-9]+)?
   | [0-9]+ [eE] [+-]? [0-9]+
   ;

BUILTINIDENTIFIER
   : '@' IDENTIFIER
   ;

