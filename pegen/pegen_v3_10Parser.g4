// Derived from https://www.python.org/dev/peps/pep-0617/
// Tokens assumed to be derived from https://raw.githubusercontent.com/python/cpython/3.10/Grammar/Tokens
// Ken Domino, 2 Sep 2021

parser grammar pegen_v3_10Parser;

options { tokenVocab=pegen_v3_10Lexer; }

start: grammar_ EOF ;

grammar_ : metas? rules ;

metas : meta+ ;

meta
    : '@' name newline
    | '@' name name newline
    | '@' name string newline
    ;

rules : rule_+ ;

rule_
    : rulename memoflag? ':' alts newline indent more_alts dedent
    | rulename memoflag? ':' newline indent more_alts dedent
    | rulename memoflag? ':' alts newline
    ;

rulename
    : name attribute?
    ;

attribute : ('[' name '*'? ']');

// In the future this may return something more complicated
memoflag : '(' 'memo' ')' ;

alts : alt ('|' alt)* ;

more_alts
    : ('|' alts newline)+
    ;

alt
    : items '$' action?
    | items action?
    ;

items : named_item+ ;

named_item
    : attribute_name? item
    | forced_atom
    | lookahead
    ;

attribute_name : name '[' name '*' ']' '=' | name '[' name ']' '=' | name '=' ;

forced_atom
    : '&' '&' atom
    ;

lookahead
    : '&' atom
    | '!' atom
    | '~'
    ;

item
    : '[' alts ']'
    |  atom '?'
    |  atom '*'
    |  atom '+'
    |  atom '.' atom '+'
    |  atom
    ;

atom
    : '(' alts ')'
    | name
    | string
    ;

action: ACTION;
name : NAME ;
string : STRING ;
newline : ;
indent : ;
dedent : ;
number : NUMBER ;