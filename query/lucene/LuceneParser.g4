/*
 * Copyright (c) 2022 by Bart Kiers
 *
 * The MIT license.
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * Project      : An ANTLR 4 grammar for Lucene
 * Developed by : Bart Kiers, bart@big-o.nl
 */
parser grammar LuceneParser;

options { tokenVocab=LuceneLexer; }

// https://github.com/apache/lucene/blob/main/lucene/queryparser/src/java/org/apache/lucene/queryparser/flexible/standard/parser/StandardSyntaxParser.jj

// TopLevelQuery ::= Query <EOF>
topLevelQuery
 : query EOF
 ;

// Query ::= DisjQuery ( DisjQuery )*
query
 : disjQuery+
 ;

// DisjQuery ::= ConjQuery ( OR ConjQuery )*
disjQuery
 : conjQuery ( OR conjQuery )*
 ;

// ConjQuery ::= ModClause ( AND ModClause )*
conjQuery
 : modClause ( AND modClause )*
 ;

// ModClause ::= (Modifier)? Clause
modClause
 : modifier? clause
 ;

// <PLUS> | (<MINUS> | <NOT>)
modifier
 : PLUS
 | MINUS
 | NOT
 ;

// Clause ::= FieldRangeExpr
//          | (FieldName (':' | '='))? (Term | GroupingExpr)
clause
 : fieldRangeExpr
 | ( fieldName ( OP_COLON | OP_EQUAL ) )? ( term | groupingExpr )
 ;

// FieldRangeExpr ::= FieldName ('<' | '>' | '<=' | '>=') (<TERM> | <QUOTED> | <NUMBER>)
fieldRangeExpr
 : fieldName ( OP_LESSTHAN | OP_MORETHAN | OP_LESSTHANEQ | OP_MORETHANEQ ) ( TERM | QUOTED | NUMBER )
 ;

// Term ::= (<TERM> | <NUMBER>) ('~' <NUM>)? ('^' <NUM>)?
//        | <REGEXPTERM> ('^' <NUM>)?
//        | TermRangeExpr ('^' <NUM>)?
//        | QuotedTerm ('^' <NUM>)?
term
 : term fuzzy ( CARAT NUMBER )?
 | REGEXPTERM ( CARAT NUMBER )?
 | termRangeExpr ( CARAT NUMBER )?
 | quotedTerm ( CARAT NUMBER )?
 | NUMBER ( CARAT NUMBER )?
 | TERM ( CARAT NUMBER )?
 ;

// GroupingExpr ::= '(' Query ')' ('^' <NUMBER>)?
groupingExpr
 : LPAREN query RPAREN ( CARAT NUMBER )?
 ;

// <TERM>
fieldName
 : TERM
 ;

// (<RANGEIN_START> | <RANGEEX_START>)
// (<RANGE_GOOP> | <RANGE_QUOTED> | <RANGE_TO>) { left = token; }
// <RANGE_TO>
// (<RANGE_GOOP> | <RANGE_QUOTED> | <RANGE_TO>) { right = token; }
// (<RANGEIN_END> | <RANGEEX_END>)
termRangeExpr
 : ( RANGEIN_START | RANGEEX_START )
   left=( RANGE_GOOP | RANGE_QUOTED | RANGE_TO )
   RANGE_TO
   right=( RANGE_GOOP | RANGE_QUOTED | RANGE_TO )
   ( RANGEIN_END | RANGEEX_END )
 ;

// QuotedTerm ::= <QUOTED> ('~' <NUM>)?
quotedTerm
 : QUOTED ( CARAT NUMBER )?
 ;

// Fuzzy ::= '~' <NUMBER>?
fuzzy
 : TILDE NUMBER?
 ;