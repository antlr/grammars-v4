
grammar euler;


program : block ENDPROG!;
vardecl : new id NEWDECL! ;
fordecl : formal id FORMALDECL! ;
labdecl : label id LABELDECL!;
var : id VARID! { '[' expr ']' SUBSCR! | '.' DOT! } ;
logval : true LOGVALTRUE! ;
logval : false LOGVALFALSE! ;
number : realN | integerN;
reference : '@' var REFERENCE! ;
# listhead -> '(' LISTHD1!;
# listhead -> listhead expr ',' LISTHD2!;
# listN -> listhead ')' LISTN1!;
# listN -> listhead expr ')' LISTN2!;
listN : '(' LISTHD1! ( ')' LISTN1! | expr listTl ) ;
listTl : ')' LISTN2! | ',' LISTHD2! ( expr listTl | ')' LISTN1! ) ;
prochead : '’' PROCHD! { fordecl ';' PROCFORDECL! } ;
procdef : prochead expr '’' PROCDEF! ;
primary : var ( listN CALL! | VALUE!) | primary1 ;
primary1 : logval LOADLOGVAL! | number LOADNUM! | symbol LOADSYMB!| reference |listN | tail primary UOP! | procdef |undef LOADUNDEF! | '[' expr ']' PARENS! | in INPUT! |isb var UOP! | isn var UOP! | isr var UOP! | isl var UOP! | isli var UOP! | isy var UOP! |isp var UOP! | isu var UOP! | abs primary UOP! |length var UOP! | integer primary UOP! | real primary UOP! | logical primary UOP! | list primary UOP! ;
factor : primary factortail;
factortail : { '**' primary BOP! } ;
term : factor termtail;
termtail : { '*' factor BOP! | '/' factor BOP! | div factor BOP! | mod factor BOP! } ;
sum : ('+' term UPLUS! | '-' term NEG! | term) sumtail;
sumtail : { '+' term BOP! | '-' term BOP! } ;
choice : sum choicetail;
choicetail : { min sum BOP! | max sum BOP! } ;
relation : choice relationtail;
relationtail : [ '=' choice BOP! | '~=' choice BOP! | '<' choice BOP! | '<=' choice BOP! | '>' choice BOP! | '>=' choice BOP! ] ;
negation : '~' relation UOP! | relation ;
conj : negation conjtail;
conjtail : [ and CONJHD! conj CONJ! ];
disj : conj disjtail;
disjtail : [ or DISJHD! disj DISJ! ] ;
catenatail : { '&' primary BOP! };
truepart : expr else TRUEPT! ;
ifclause : if expr then IFCLSE! ;
expr : var exprtail | expr1;
exprtail : '<-' expr BOP! | ( listN CALL! | VALUE!) factortail termtail sumtail choicetail relationtail conjtail disjtail catenatail ;
expr1 : block ;
expr1 : ifclause truepart expr IFEXPR! ;
expr1 : goto primary UOP! ;
expr1 : out expr UOP! ;
expr1 :primary1 factortail termtail sumtail choicetail relationtail conjtail disjtail catenatail ;
expr1 : ( '+' term UPLUS! | '-' term NEG! ) sumtail choicetail relationtail conjtail disjtail catenatail ;
expr1 : '~' relation UOP! conjtail disjtail catenatail ;
stat : expr1 | id ( ':' LABDEF! stat LABSTMT! | VARID! { '[' expr ']' SUBSCR! | '.' DOT! } exprtail ) ;
block : begin BEGIN! { vardecl ';' BLKHD! | labdecl ';' BLKHD!} stat { ';' BLKBODY! stat } end BLK! ;

WS
   : (' ' | '\r' | '\t' | '\u000C' | '\n') -> channel (HIDDEN)
   ;

