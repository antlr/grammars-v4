grammar INI;

file: section? (NEWLINE section)* NEWLINE? EOF;

section: '[' SECTION_NAME ']' NEWLINE? keyvalue*;

keyvalue: KEY_NAME '=' VALUE NEWLINE?;

SECTION_NAME: /[a-zA-Z0-9_-]+/;
KEY_NAME: /[a-zA-Z0-9_-]+/;
VALUE: /[^#\n\r]+/;

NEWLINE: '\r'? '\n';

COMMENT: '#' /[^\n\r]*/;
