grammar TableGen;

// TableGenFile ::=  (Statement | IncludeDirective | PreprocessorDirective)*
tableGenFile
    :   (statement | includeDirective | preprocessorDirective)*
    ;

preprocessorDirective
    :   predDefine
    |   preIfdef
    |   preElse
    |   preEndif
    ;

// MacroName              ::=  ualpha (ualpha | "0"..."9")*
macroName
    :   TokIdentifier
    ;

// PreDefine              ::=  LineBegin (WhiteSpaceOrCComment)*
//                             "#define" (WhiteSpace)+ MacroName
//                             (WhiteSpaceOrAnyComment)* LineEnd
predDefine
    :   PreprocessorDefine macroName
    ;

// PreIfdef               ::=  LineBegin (WhiteSpaceOrCComment)*
//                             ("#ifdef" | "#ifndef") (WhiteSpace)+ MacroName
//                             (WhiteSpaceOrAnyComment)* LineEnd
preIfdef
    :   (PreprocessorIfdef | PreprocessorIfndef) macroName
    ;

// PreElse                ::=  LineBegin (WhiteSpaceOrCComment)*
//                             "#else" (WhiteSpaceOrAnyComment)* LineEnd
preElse
    :   PreprocessorElse
    ;

// PreEndif               ::=  LineBegin (WhiteSpaceOrCComment)*
//                             "#endif" (WhiteSpaceOrAnyComment)* LineEnd
preEndif
    :   PreprocessorEndif
    ;

// Statement    ::=  Assert | Class | Def | Defm | Defset | Deftype
//                 | Defvar | Dump  | Foreach | If | Let | MultiClass
statement
    :   assertStmt
    |   classStmt
    |   defStmt
    |   defmStmt
    |   defsetStmt
    |   deftypeStmt
    |   defvarStmt
    |   dumpStmt
    |   foreachStmt
    |   ifStmt
    |   letStmt
    |   multiclassStmt
    ;

// Class           ::=  "class" ClassID [TemplateArgList] RecordBody
classStmt
    :   Class classID templateArgList? recordBody
    ;

// TemplateArgList ::=  "<" TemplateArgDecl ("," TemplateArgDecl)* ">"
templateArgList
    :   '<' templateArgDecl (',' templateArgDecl)* '>'
    ;

// TemplateArgDecl ::=  Type TokIdentifier ["=" Value]
templateArgDecl
    :   type TokIdentifier ('=' value)?
    ;

// RecordBody            ::=  ParentClassList Body
recordBody
    :   parentClassList body
    ;

// ParentClassList       ::=  [":" ParentClassListNE]
parentClassList
    :   (':' parentClassListNE)?
    ;

// ParentClassListNE     ::=  ClassRef ("," ClassRef)*
parentClassListNE
    :   classRef (',' classRef)*
    ;

// ClassRef              ::=  (ClassID | MultiClassID) ["<" [ArgValueList] ">"]
classRef
    :   (classID | multiClassID) ('<' argValueList '>')?
    ;

// ArgValueList          ::=  PostionalArgValueList [","] NamedArgValueList
argValueList
    :   positionalArgValueList (',')? namedArgValueList
    ;

// PostionalArgValueList ::=  [Value {"," Value}*]
positionalArgValueList
    :   (value (',' value)*)?
    ;

// NamedArgValueList     ::=  [NameValue "=" Value {"," NameValue "=" Value}*]
namedArgValueList
    :   (nameValue '=' value (',' nameValue '=' value)*)?
    ;

// Body     ::=  ";" | "{" BodyItem* "}"
body
    :   ';'
    |   '{' bodyItem* '}'
    ;

// BodyItem ::=  (Type | "code") TokIdentifier ["=" Value] ";"
//             | "let" TokIdentifier ["{" RangeList "}"] "=" Value ";"
//             | "defvar" TokIdentifier "=" Value ";"
//             | Assert
bodyItem
    :   (type | Code) TokIdentifier ('=' value)? ';'
    |   Let TokIdentifier ('{' rangeList '}')? '=' value ';'
    |   Defvar TokIdentifier '=' value ';'
    |   assertStmt
    ;

// Def       ::=  "def" [NameValue] RecordBody
defStmt
    :   Def nameValue? recordBody
    ;

// NameValue ::=  Value (parsed in a special mode)
nameValue
    :   value
    ;

// Let      ::=  "let" LetList "in" "{" Statement* "}"
//             | "let" LetList "in" Statement
letStmt
    :   Let letList In '{' statement* '}'
    |   Let letList In statement
    ;

// LetList ::=  LetItem ("," LetItem)*
letList
    :   letItem (',' letItem)*
    ;

// LetItem ::=  TokIdentifier ["<" RangeList ">"] "=" Value
letItem
    :   TokIdentifier ('<' rangeList '>')? '=' value
    ;

// MultiClass          ::=  "multiclass" TokIdentifier [TemplateArgList] ParentClassList "{" MultiClassStatement+ "}"
multiclassStmt
    :   Multiclass TokIdentifier templateArgList? parentClassList '{' multiClassStatement+ '}'
    ;

// MultiClassID        ::=  TokIdentifier
multiClassID
    :   TokIdentifier
    ;

// MultiClassStatement ::=  Assert | Def | Defm | Defvar | Foreach | If | Let
multiClassStatement
    :   assertStmt
    |   defStmt
    |   defmStmt
    |   defvarStmt
    |   foreachStmt
    |   ifStmt
    |   letStmt
    ;

// Defm ::=  "defm" [NameValue] ParentClassList ";"
defmStmt
    :   Defm nameValue? parentClassList ';'
    ;

// Defset ::=  "defset" Type TokIdentifier "=" "{" Statement* "}"
defsetStmt
    :   Defset type TokIdentifier '=' '{' statement* '}'
    ;

// Deftype ::=  "deftype" TokIdentifier "=" Type ";"
deftypeStmt
    :   Deftype TokIdentifier '=' type ';'
    ;

// Defvar ::=  "defvar" TokIdentifier "=" Value ";"
defvarStmt
    :   Defvar TokIdentifier '=' value ';'
    ;

// Foreach         ::=  "foreach" ForeachIterator "in" "{" Statement* "}"
//                    | "foreach" ForeachIterator "in" Statement
foreachStmt
    :   Foreach foreachIterator In '{' statement* '}'
    |   Foreach foreachIterator In statement
    ;

// ForeachIterator ::=  TokIdentifier "=" ("{" RangeList "}" | RangePiece | Value)
foreachIterator
    :   TokIdentifier '=' ('{' rangeList '}' | rangePiece | value)
    ;

// Dump ::=  "dump" string ";"
dumpStmt
    :   Dump value ';'
    ;

// If     ::=  "if" Value "then" IfBody
//           | "if" Value "then" IfBody "else" IfBody
ifStmt
    :   If value Then ifBody
    |   If value Then ifBody Else ifBody
    ;

// IfBody ::=  "{" Statement* "}" | Statement
ifBody
    :   '{' statement* '}'
    |   statement
    ;

// Assert ::=  "assert" condition "," message ";"
assertStmt
    :   Assert value ',' value ';'
    ;

// Value         ::=  SimpleValue ValueSuffix*
//                  | Value "#" [Value]
value
    :   simpleValue valueSuffix*
    |   value '#' value?
    ;

// ValueSuffix   ::=  "{" RangeList "}"
//                  | "[" SliceElements "]"
//                  | "." TokIdentifier
valueSuffix
    :   '{' rangeList '}'
    |   '[' sliceElements ']'
    |   '.' TokIdentifier
    ;

// RangeList     ::=  RangePiece ("," RangePiece)*
rangeList
    :   rangePiece (',' rangePiece)*
    ;

// RangePiece    ::=  TokInteger
//                  | TokInteger "..." TokInteger
//                  | TokInteger "-" TokInteger
//                  | TokInteger TokInteger
rangePiece
    :   TokInteger
    |   TokInteger '...' TokInteger
    |   TokInteger '-' TokInteger
    |   TokInteger TokInteger
    ;

// SliceElements ::=  (SliceElement ",")* SliceElement ","?
sliceElements
    :   (sliceElement ',')* sliceElement ','
    ;

// SliceElement  ::=  Value
//                  | Value "..." Value
//                  | Value "-" Value
//                  | Value TokInteger
sliceElement
    :   value
    |   value '...' value
    |   value '-' value
    |   value TokInteger
    ;

simpleValue
    :   simpleValueTok
    |   simpleValueBool
    |   simpleValueQuestion
    |   simpleValueList
    |   simpleValueListType
    |   simpleValueDag
    |   simpleValueIdentifier
    |   simpleValueClass
    |   simpleValueOperator
    ;

// SimpleValue1 ::=  TokInteger | TokString+ | TokCode
simpleValueTok
    :   TokInteger
    |   TokString+
    |   TokCode
    ;

// SimpleValue2 ::=  "true" | "false"
simpleValueBool
    :   True
    |   False
    ;

// SimpleValue3 ::=  "?"
simpleValueQuestion
    :   '?'
    ;

// SimpleValue4 ::=  "{" [ValueList] "}"
simpleValueList
    :   '{' valueList '}'
    ;

// ValueList    ::=  ValueListNE
valueList
    :   valueListNE?
    ;

// ValueListNE  ::=  Value ("," Value)*
valueListNE
    :   value (',' value)* ','?
    ;

// SimpleValue5 ::=  "[" ValueList "]" ["<" Type ">"]
simpleValueListType
    :   '[' valueList ']' ('<' type '>')?
    ;

// SimpleValue6 ::=  "(" DagArg [DagArgList] ")"
simpleValueDag
    :   '(' dagArg (dagArgList)? ')'
    ;

// DagArgList   ::=  DagArg ("," DagArg)*
dagArgList
    :   dagArg (',' dagArg)* ','?
    ;

// DagArg       ::=  Value [":" TokVarName] | TokVarName
dagArg
    :   value (':' TokVarName)?
    |   TokVarName
    ;

// SimpleValue7 ::=  TokIdentifier
simpleValueIdentifier
    :   TokIdentifier
    ;

// SimpleValue8 ::=  ClassID "<" ArgValueList ">"
simpleValueClass
    :   classID '<' argValueList '>'
    ;

// SimpleValue9 ::=  BangOperator ["<" Type ">"] "(" ValueListNE ")"
//                 | CondOperator "(" CondClause ("," CondClause)* ")"
simpleValueOperator
    :   bangOperator ('<' type '>')? '(' valueListNE ')'
    |   condOperator '(' condClause (',' condClause)* ')'
    ;

// CondClause   ::=  Value ":" Value
condClause
    :   value ':' value
    ;

// Type    ::=  "bit" | "int" | "string" | "dag"
//            | "bits" "<" TokInteger ">"
//            | "list" "<" Type ">"
//            | ClassID
type
    :   Bit
    |   Int
    |   String
    |   Dag
    |   Bits '<' TokInteger '>'
    |   List '<' type '>'
    |   classID
    ;

// ClassID ::=  TokIdentifier
classID
    :   TokIdentifier
    ;

includeDirective
    :   Include TokString
    ;

// BangOperator ::=  one of
//                  !add         !and         !cast        !con         !dag
//                  !div         !empty       !eq          !exists      !filter
//                  !find        !foldl       !foreach     !ge          !getdagarg
//                  !getdagname  !getdagop    !gt          !head        !if
//                  !interleave  !isa         !le          !listconcat  !listremove
//                  !listsplat   !logtwo      !lt          !mul         !ne
//                  !not         !or          !range       !repr        !setdagarg
//                  !setdagname  !setdagop    !shl         !size        !sra
//                  !srl         !strconcat   !sub         !subst       !substr
//                  !tail        !tolower     !toupper     !xor
bangOperator
    :   '!add' | '!and' | '!cast' | '!con' | '!dag'
    |   '!div' | '!empty' | '!eq' | '!exists' | '!filter'
    |   '!find' | '!foldl' | '!foreach' | '!ge' | '!getdagarg'
    |   '!getdagname' | '!getdagop' | '!gt' | '!head' | '!if'
    |   '!interleave' | '!isa' | '!le' | '!listconcat' | '!listremove'
    |   '!listsplat' | '!logtwo' | '!lt' | '!mul' | '!ne'
    |   '!not' | '!or' | '!range' | '!repr' | '!setdagarg'
    |   '!setdagname' | '!setdagop' | '!shl' | '!size' | '!sra'
    |   '!srl' | '!strconcat' | '!sub' | '!subst' | '!substr'
    |   '!tail' | '!tolower' | '!toupper' | '!xor'
    ;

// CondOperator ::=  !cond
condOperator
    :   '!cond'
    ;

// Keywords:
//  assert     bit           bits          class         code
//  dag        def           dump          else          false
//  foreach    defm          defset        defvar        field
//  if         in            include       int           let
//  list       multiclass    string        then          true
Assert          : 'assert';
Bit             : 'bit';
Bits            : 'bits';
Class           : 'class';
Code            : 'code';
Dag             : 'dag';
Def             : 'def';
Dump            : 'dump';
Else            : 'else';
False           : 'false';
Foreach         : 'foreach';
Defm            : 'defm';
Defset          : 'defset';
Defvar          : 'defvar';
Deftype         : 'deftype';
Field           : 'field';
If              : 'if';
In              : 'in';
Include         : 'include';
Int             : 'int';
Let             : 'let';
List            : 'list';
Multiclass      : 'multiclass';
String          : 'string';
Then            : 'then';
True            : 'true';

// '"' (non-'"' characters and escapes) '"'
TokString       : '"' (~('"') | Escape)* '"';
// "[{" (shortest text not containing "}]") "}]"
TokCode         : '[{' (~(']') | ']' ~('}') | Escape)*? '}]';

TokInteger      :  DecimalInteger | HexInteger | BinInteger;
DecimalInteger  :  ('+' | '-')? (UDigit)+;
HexInteger      :  '0x' (UDigit | 'a'..'f' | 'A'..'F')+;
BinInteger      :  '0b' ('0' | '1')+;

TokIdentifier   :  (UDigit)* UAlpha (UAlpha | UDigit)*;
TokVarName      :  '$' UAlpha (UAlpha |  UDigit)*;

// Preprocessor directives
PreprocessorDefine  : '#define';
PreprocessorIfdef   : '#ifdef';
PreprocessorIfndef  : '#ifndef';
PreprocessorElse    : '#else';
PreprocessorEndif   : '#endif';

WhiteSpace          : [ \n\t\r]+ -> skip;
SingleLineComment   : '//' ~[\r\n]* -> skip;
MultiLineComment    : '/*' .*? '*/' -> skip;

fragment UDigit     : '0'..'9';
fragment UAlpha     : 'a'..'z' | 'A'..'Z' | '_';
fragment EOL        : '\r'? '\n';
fragment Escape     : '\\' [btnfr"'\\];
