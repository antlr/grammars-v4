/*
BSD License

Copyright (c) 2024, Tom Everett
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Tom Everett nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
grammar fantom;

file_
   : program? EOF
   ;

<compilationUnit> :=  <using*> <typeDef>* [<doc>]
<using>           :=  <usingPod> | <usingType> | <usingAs>
<usingPod>        :=  "using" <podSpec> <eos>
<usingType>       :=  "using" <podSpec> "::" <id> <eos>
<usingAs>         :=  "using" <podSpec> "::" <id> "as" <id> <eos>
<podSpec>         :=  <id> | <str> | <ffiPodSpec>
<ffiPodSpec>      := "[" <id> "]" <id> ("." <id>)*

<typeDef>        :=  <classDef> | <mixinDef> | <facetDef> | <enumDef>

<classDef>       :=  <typeHeader> "class" <id> [<inheritance>] "{" <slotDefs> "}"
<mixinDef>       :=  <typeHeader> "mixin" <id> [<inheritance>] "{" <slotDefs> "}"
<facetDef>       :=  <typeHeader> "facet" "class" <id> [<inheritance>] "{" <slotDefs> "}"
<enumDef>        :=  <typeHeader> "enum" "class" <id> [<inheritance>] "{" <enumValDefs> <slotDefs> "}"

<typeHeader>     :=  [<doc>] <facets> <typeFlags>
<typeFlags>      :=  <typeFlag>*
<typeFlag>       :=  <protection> | "abstract" | "final" | "const" | "native"
<protection>     :=  "public" | "protected" | "private" | "internal"
<inheritance>    :=  ":" <typeList>

<enumValDefs>    :=  <enumValDef> ("," <enumValDef>)* <eos>
<enumValDef>     :=  <facets> <id> ["(" <args> ")"]

<slotDefs>       :=  <slotDef>*
<slotDef>        :=  <fieldDef> | <methodDef> | <ctorDef> | <staticInit>

<fieldDef>       :=  <facets> <fieldFlags> <type> <id> [":=" <expr>]
                     [ "{" [<fieldGetter>] [<fieldSetter>] "}" ] <eos>
<fieldFlags>     :=  <fieldFlag>*
<fieldFlag>      :=  <protection> | "abstract" | "const" | "final" | "native" |
                     "override" | "readonly" | "static" | "virtual"
<fieldGetter>    :=  "get" (<eos> | <block>)
<fieldSetter>    :=  <protection> "set" (<eos> | <block>)

<methodDef>      :=  <facets> <methodFlags> <type> <id> "(" <params> ")" <methodBody>
<methodFlags>    :=  <methodFlag>*
<methodFlag>     :=  <protection> | "abstract" | "native" | "once" |
                     "override" | "static" | "virtual" | "final"
<params>         :=  [<param> ("," <param>)*]
<param>          :=  <type> <id> [":=" <expr>]
<methodBody>     :=  <eos> | ( "{" <stmts> "}" )

<ctorDef>        :=  <facets> <ctorFlags> "new" <id> "(" <params> ")" [ctorChain] <methodBody>
<ctorFlags>      :=  [<protection>]
<ctorChain>      :=  ":" <ctorChainThis> | <ctorChainSuper>
<ctorChainThis>  :=  "this" "." <id> "(" <args> ")"
<ctorChainSuper> :=  "super" ["." <id>] "(" <args> ")"

<staticInit>     :=  "static" "{" <stmts> "}"


<facets>        := <facet>*
<facet>         := "@" <simpleType> [<facetVals>]
<facetVals>     := "{" <facetVal> (<eos> <facetVal>)* "}"
<facetVal>      := <id> "=" <expr>

<block>          :=  <stmt> | ( "{" <stmts> "}" )
<stmts>          :=  <stmt>*
<stmt>           :=  <break> | <continue> | <for> | <if> | <return> | <switch> |
                     <throw> | <while> | <try> | <exprStmt> | <localDef> | <itAdd>
<break>          :=  "break" <eos>
<continue>       :=  "continue" <eos>
<for>            :=  "for" "(" [<forInit>] ";" [<expr>] ";" [<expr>] ")" <block>
<forInit>        :=  <expr> | <localDef>
<if>             :=  "if" "(" <expr> ")" <block> [ "else" <block> ]
<return>         :=  "return" [<expr>] <eos>
<throw>          :=  "throw" <expr> <eos>
<while>          :=  "while" "(" <expr> ")" <block>
<exprStmt>       :=  <expr> <eos>
<localDef>       :=  [<type>] <id> [":=" <expr>] <eos>
<itAdd>          :=  <expr> ("," <expr>)* [","] <eos>

<try>            :=  "try" <block> <catch>* [<finally>]
<catch>          :=  "catch" [<catchDef>] <block>
<catchDef>       :=  "(" <type> <id> ")"
<finally>        :=  "finally" <block>

<switch>         :=  "switch" "(" <expr> ")" "{" <case>* [<default>] "}"
<case>           :=  "case" <expr> ":" <stmts>
<default>        :=  "default" ":" <stmts>

<expr>           :=  <assignExpr>
<assignExpr>     :=  <ifExpr> [<assignOp> <assignExpr>]
<assignOp>       :=  "=" | "*=" | "/=" | "%=" | "+=" | "-="

<ifExpr>         :=  <ternaryExpr> | <elvisExpr>
<ternaryExpr>    :=  <condOrExpr> ["?" <ifExprBody> ":" <ifExprBody>]
<elvisExpr>      :=  <condOrExpr> "?:" <ifExprBody>
<ifExprBody>     :=  <condOrExpr> | <ifExprThrow>
<ifExprThrow>    :=  "throw" <expr>

<condOrExpr>     :=  <condAndExpr>  ("||" <condAndExpr>)*
<condAndExpr>    :=  <equalityExpr> ("&&" <equalityExpr>)*
<equalityExpr>   :=  <relationalExpr> [("==" | "!=" | "===" | "!==") <relationalExpr>]
<relationalExpr> :=  <typeCheckExpr> | <compareExpr>
<typeCheckExpr>  :=  <rangeExpr> [("is" | "as" | "isnot") <type>]
<compareExpr>    :=  <rangeExpr> [("<" | "<=" | ">" | ">=" | "<=>") <rangeExpr>]
<rangeExpr>      :=  <addExpr> ((".." | "..<") <addExpr>)*
<addExpr>        :=  <multExpr> (("+" | "-") <multExpr>)*
<multExpr>       :=  <parenExpr> (("*" | "/" | "%") <parenExpr>)*
<parenExpr>      :=  <unaryExpr> | <castExpr> | <groupedExpr>
<castExpr>       :=  "(" <type> ")" <parenExpr>
<groupedExpr>    :=  "(" <expr> ")" <termChain>*
<unaryExpr>      :=  <prefixExpr> | <termExpr> | <postfixExpr>
<prefixExpr>     :=  ("!" | "+" | "-" | "++" | "--") <parenExpr>
<postfixExpr>    :=  <termExpr> ("++" | "--")

<termExpr>       :=  <termBase> <termChain>*
<termBase>       :=  <literal> | <idExpr> | <typeBase>
<typeBase>       :=  <typeLiteral> | <slotLiteral> | <namedSuper> | <staticCall> |
                     <dsl> | <closure> | <simple> | <ctorBlock>
<typeLiteral>    :=  <type> "#"
<slotLiteral>    :=  [<type>] "#" <id>
<namedSuper>     :=  <type> "." "super"
<staticCall>     :=  <type> "." <slotExpr>
<dsl>            :=  <type> "<|" <anyChar>* "|>"
<simple>         :=  <type> "(" <expr> ")"
<ctorBlock>      :=  <type> <itBlock>
<termChain>      :=  <dotCall> | <dynCall> | <safeDotCall> | <safeDynCall> |
                     <indexExpr> | <callOp> | <itBlock>
<itBlock>        :=  "{" <stmts> "}"
<dotCall>        :=  "." <slotExpr>
<dynCall>        :=  "->" <slotExpr>
<safeDotCall>    :=  "?." <slotExpr>
<safeDynCall>    :=  "?->" <slotExpr>
<idExpr>         :=  <local> | <slotExpr>
<slotExpr>       :=  <field> | <call>
<local>          :=  <id>
<field>          :=  ["&"] <id>
<call>           :=  <id> ["(" <args> ")"] [<closure>]
<args>           :=  [<expr> ("," <expr>)*]
<indexExpr>      :=  "[" <expr> "]"
<callOp>         :=  "(" <args> ")" [<closure>]

<literal>        :=  "null" | "this" | "super" | "it" |
                     <bool> | <int> | <float> | <decimal> | <str> |
                     <duration> | <uri> | <list> | <map> | <uri>
<list>           :=  [<type>] "[" <listItems> "]"
<listItems>      :=  "," | (<expr> ("," <expr>)*) [","]
<map>            :=  [<mapType>] "[" <mapItems> [","] "]"
<mapItems>       :=  ":" | (<mapPair> ("," <mapPair>)*)
<mapPair>        :=  <expr> ":" <expr>

<closure>        :=  <closureSig> "{" <stmts> "}"
<closureSig>     :=  "|" <closureParams> ["->" <type>] "|"
<closureParams>  :=  [<closureParam> ("," <closureParam>)*]
<closureParam>   :=  <formal> | <id>

<type>           :=  <nullType> | <nonNullType>
<nullType>       :=  <nonNullType> "?"
<nonNullType>    :=  <simpleType> | <listType> | <mapType> | <funcType>
<typeList>       :=  <type> ("," <type>)*
<simpleType>     :=  <id> ["::" <id>]
<listType>       :=  <type> "[]"
<mapType>        :=  ["["] <type> ":" <type> ["]"]
<funcType>       :=  "|" [formals] ["->" <type>] "|"
<formals>        :=  [<formal> ("," <formal>)*]
<formal>         :=  <formalFull> | <formalInferred> | <formalTypeOnly>
<formalFull>     :=  <type> <id>
<formalInferred> :=  <id>
<formalTypeOnly> :=  <type>

<id>             := <idStart> (idChar)*
<idStart>        := A-Z | a-z | _
<idChar>         := A-Z | a-z | _ | 0-9
<eos>            := ; | \n | }
    

COMMENT
   : ';' ~ ('\r' | '\n')* -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

