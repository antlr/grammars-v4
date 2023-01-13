grammar ReStructuredText;

// Copyright (C) 2011 Bart Kiers
// Copyright (C) 2017 Lex Li
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy of this
// software and associated documentation files (the "Software"), to deal in the Software
// without restriction, including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
// to whom the Software is furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
// PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
// FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

/*
 *  A grammar for reStructuredText language written in ANTLR v4.
 *  This is just a snapshot of its 0.9 release.
 *  You can find the latest release from the official repo, https://github.com/lextm/restructuredtext-antlr
 *  Issues and pull requests should also go to the official repo.
 */

parse
  :  (element | empty_line)+? EOF
  ;

element
  :  section | sectionElement
  ;
  
sectionElement
  :  listItemBullet | listItemEnumerated | paragraph | lineBlock | comment
  ;

comment
  :  Space* Comment Space* (commentLineNoBreak commentParagraphs?)?
  ;
  
commentParagraphs
  : main=commentParagraph commentRest
  ;

commentRest
  : (empty_line commentParagraph)*
  ;

commentParagraph
  :  commentLine+
  ;

commentLineNoBreak
  :  commentLineAtoms
  ;

commentLine
  :  LineBreak Space Space Space commentLineNoBreak
  ;

commentLineAtoms
  :  ~(LineBreak)+
  ;

paragraph
  :  lines
  ;

section
  :  (LineBreak overline=SectionSeparator)? title LineBreak? SectionSeparator (LineBreak)* sectionElement*
  ;

title
  :  LineBreak textStart
  |  LineBreak lineSpecial Space+ (paragraphNoBreak)?
  |  lineNormal
  |  lineStar
  ;

lineBlock
  :  LineBreak lineBlockLine LineBreak? lineBlockLine*
  ;

lineBlockLine
  :  Block Space indentation? span*? starText
  |  Block Space indentation? span+
  ;

listItemBullet
  :  bulletCrossLine
  |  bulletSimple
  |  LineBreak Space* special=(Minus | Plus)
  ;

bulletCrossLine
  :  LineBreak Space* bullet Space* (paragraph+)? 
  ;

bulletSimple 
  :  LineBreak Space* bullet Space+ paragraphNoBreak paragraph* 
  ;

bullet
  :  Star 
  |  Minus 
  |  Plus
  ;

listItemEnumerated
  :  LineBreak enumerated=lineSpecial Space+ (paragraphNoBreak paragraph*)?
  ;
  
paragraphNoBreak
  :  lineNoBreak lines*
  ;

lineNoBreak
  :  indentation? spanLineStartNoStar span*?
  ;
  
lines
  :  linesStar
  |  linesNormal
  ;

linesNormal
  :  lineNormal (linesStar | linesNormal?)
  ;
  
linesStar
  :  lineStar
  |  lineStar lineNoBreak linesNormal??  
  |  lineStar lineNoBreak linesStar
  ;

lineNormal
  :  LineBreak indentation? spanLineStartNoStar+? (span*? spanNoStar+?)?
  |  lineSpecial
  ;
  
lineStar
  :  LineBreak indentation? spanLineStartNoStar*? starText
  |  LineBreak indentation? text_fragment+ starText
  ;
 
lineSpecial
  :  Numbers Dot
  |  LineBreak indentation? Numbers
  |  LineBreak indentation? SectionSeparator (Space+ SectionSeparator) Space* // for table.
  //|  Alphabet Dot
  ;
  
empty_line
  :  LineBreak Space*
  ;

indentation
  :  Space+
  ;

spanLineStartNoStar
  :  reference
  |  referenceIn
  |  hyperlinkTarget
  |  hyperlink
  |  hyperlinkDoc
  |  backTickText
  |  quotedLiteral
  |  textLineStart
  ;

textLineStart
  :  lineStart_fragment+ text_fragment*
  ;
  
lineStart_fragment
  :  (Minus ~(Space | LineBreak | Star))
  |  (Plus ~(Space | Star))
  |  (Numbers Dot ~(Space | LineBreak | Star))
  |  (Numbers ~(Dot | LineBreak | Star))
  //|  (Alphabet Dot ~(Space | LineBreak | Star))
  |  (Alphabet Dot)
  |  (Block ~(Space | Star))
  |  (UnderScore ~(Space | Star))
  |  (Alphabet ~(Dot | LineBreak | Star))
  |  Alphabet
  |  separator separator
  |  TimeStar
   |  SquareLeft
    |  SquareRight
    |  RoundLeft
    |  RoundRight
    |  SemiColon
    |  Colon
    |  QuotationDouble
    |  QuotationSingle
    |  Dot
    |  UnderScore
    |  AngleLeft
    |  AngleRight
    |  Any
  ;
  
text
  :  textStart+ text_fragment*
  ;

textStart
  :  forcedText
  |  lineStart_fragment
  |  text_fragment_start text_fragment_start+
  |  Space
  ;

forcedText 
  :  RoundLeft Star RoundRight 
  |  SquareLeft Star SquareRight 
  |  QuotationSingle Star QuotationSingle 
  |  QuotationSingle QuotationDouble Star QuotationDouble QuotationSingle
  ;

spanNoStar
  :  reference
  |  referenceIn
  |  hyperlinkTarget
  |  hyperlink
  |  hyperlinkDoc
  |  backTickText
  |  quotedLiteral
  |  text
  ;

span
  :  starText
  |  spanNoStar
  ;

quotedLiteral
  : AngleRight Space lineNoBreak
  ;

text_fragment_start
  :  SemiColon
  |  Numbers
  |  Alphabet
  |  Space
  |  SquareLeft
  |  SquareRight
  |  RoundLeft
  |  RoundRight
  |  Colon
  |  separator
  |  AngleLeft
  |  AngleRight
  |  QuotationDouble
  |  Dot
  |  Star Space
  |  Any
  ;

text_fragment
  :  text_fragment_start
  |  forcedText
  |  Block
  |  Literal
  |  Comment
  |  Dot
  |  Quote
  ;

starText
  :  Star+ LineBreak
  |  Star+ starNoSpace starAtoms (LineBreak Star* starNoSpace starAtoms)* Star* LineBreak
  |  Star+ starNoSpace starAtoms Star* LineBreak
  |  Star+ Space+ starAtoms Star+ LineBreak
  ;

starAtoms
  :  starAtom* (Star* starAtom)*
  ;

starNoSpace
  :  ~(Star | LineBreak | Space | SectionSeparator)
  ;

starAtom
  :  ~(Star | LineBreak)
  ;

backTickText
  :  (Colon titled=Alphabet Colon)? body UnderScore?
  ;

body
  :  BackTick BackTick* backTickAtoms BackTick+
  |  BackTick backTickNoSpace backTickAtoms BackTick+
  |  BackTick BackTick
  ;

backTickAtoms
  :  backTickAtom+
  ;

backTickNoSpace
  :  ~(BackTick | LineBreak | Space)
  ;

backTickAtom
  :  ~(BackTick | LineBreak)
  |  BackTick ~(BackTick | LineBreak)
  ;

reference
  :  Any+ UnderScore
  ;

referenceIn
  :  UnderScore hyperlinkAtom+ Colon Space url
  ;

hyperlinkTarget
  :  UnderScore Any+
  ;
  
hyperlink
  :  BackTick hyperlinkAtom+ Space AngleLeft url AngleRight BackTick UnderScore Space
  ;
 
hyperlinkDoc
  :  ':doc:' BackTick hyperlinkAtom+ Space AngleLeft url AngleRight BackTick
  |  ':doc:' BackTick url BackTick
  ;

url
  :  urlAtom+
  ;
  
urlAtom
  :  ~( LineBreak | BackTick )
  ;
  
hyperlinkAtom
  :  ~( LineBreak | AngleLeft | AngleRight | BackTick | Star )
  ;

separator
  :  (Minus | Equal | Plus | Hat)
  ;

SectionSeparator
  :  (Minus | Equal | Plus | Hat) (Minus | Equal | Plus | Hat) (Minus | Equal | Plus | Hat)+
  ;

Literal
  :  Colon LineBreak LineBreak* Colon Colon
  ;

TimeStar
  : Numbers Star
  | 'x' Star
  ;

Alphabet
  : [A-Za-z]+
  ;
  
Numbers
  : [0-9]+
  ;

Quote
  :  Colon Colon
  ;

SquareLeft
  :  '['
  ;

SquareRight
  :  ']'
  ;

RoundLeft
  :  '('
  ;
  
RoundRight
  :  ')'
  ;
  
AngleLeft
  :  '<'
  ;

AngleRight
  :  '>'
  ;

Hat
  :  '^'
  ;
  
QuotationDouble
  :  '"'
  ;

QuotationSingle
  :  '\''
  ;

Dot
  :  '.'
  ;
  
SemiColon
  :  ';'
  ;
  
Colon
  :  ':'
  ;

Equal
  :  '='
  ;

Plus
  :  '+'
  ;

Minus
  :  '-'
  ;

Block
  :  '|'
  ;

Comment
  :  ('.. ' LineBreak?)
  |  ('..' LineBreak)
  ;

UnderScore
  :  '_'
  ;

BackTick
  :  '`'
  ;

Star
  :  '*'
  ;

Space
  :  ' ' 
  |  '\t'
  ;

LineBreak
  :  '\r'? '\n'
  ;

Any
  :  .
  ;