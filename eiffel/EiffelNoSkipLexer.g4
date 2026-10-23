lexer grammar EiffelNoSkipLexer;

options {
    caseInsensitive = true;
    superClass = EiffelLexerBase;
}

import EiffelLexer;

WhiteSpace: [ \t\n\r\uFEFF]+ -> channel(1);
Comment: '--' .*? '\n' -> channel(2);