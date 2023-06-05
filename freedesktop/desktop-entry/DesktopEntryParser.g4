/*
MIT License

Copyright (c) 2023 Mustafa Said Ağca

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

parser grammar DesktopEntryParser;
options { tokenVocab = DesktopEntryLexer; }

desktop_entry : group* EOF ;
group : group_header entry* ;
group_header : LEFT_BRACKET group_name RIGHT_BRACKET ;
group_name : GROUP_NAME ;
entry : key locale? EQUAL value? ( SEMICOLON value )* SEMICOLON? ;
key : KEY_NAME ;
locale : LEFT_BRACKET language_ ( UNDERSCORE country )? ( DOT encoding )? ( AT modifier )? RIGHT_BRACKET ;
language_ : LANGUAGE ;
country : COUNTRY ;
encoding : ENCODING ;
modifier : MODIFIER ;
value : string_ | number | true_ | false_ ;
string_ : STRING ;
number : NUMBER ;
true_ : TRUE ;
false_ : FALSE ;
