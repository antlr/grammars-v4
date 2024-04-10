/*
PostgreSQL grammar.
The MIT License (MIT).
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

package parser

import (
	"github.com/antlr/antlr4/runtime/Go/antlr"
	"unicode"
)

type PostgreSQLLexerBase struct {
	*antlr.BaseLexer

	stack StringStack
}

func (receiver *PostgreSQLLexerBase) pushTag() {
	receiver.stack.Push(receiver.GetText())
}

func (receiver *PostgreSQLLexerBase) isTag() bool {
	if receiver.stack.IsEmpty() {
		return false
	}
	return receiver.GetText() == receiver.stack.PeekOrEmpty()
}

func (receiver *PostgreSQLLexerBase) popTag() {
	_, _ = receiver.stack.Pop()
}

func (receiver *PostgreSQLLexerBase) checkLA(c int) bool {
	return receiver.GetInputStream().LA(1) != c
}

func (receiver *PostgreSQLLexerBase) charIsLetter() bool {
	c := receiver.GetInputStream().LA(-1)
	return unicode.IsLetter(rune(c))
}

func (receiver *PostgreSQLLexerBase) HandleNumericFail() {
	index := receiver.GetInputStream().Index() - 2
	receiver.GetInputStream().Seek(index)
	receiver.SetType(PostgreSQLLexerIntegral)
}

func (receiver *PostgreSQLLexerBase) HandleLessLessGreaterGreater() {
	if receiver.GetText() == "<<" {
		receiver.SetType(PostgreSQLLexerLESS_LESS)
	}
	if receiver.GetText() == ">>" {
		receiver.SetType(PostgreSQLLexerGREATER_GREATER)
	}
}

func (receiver *PostgreSQLLexerBase) UnterminatedBlockCommentDebugAssert() {
	//Debug.Assert(InputStream.LA(1) == -1 /*EOF*/);
}

func (receiver *PostgreSQLLexerBase) CheckIfUtf32Letter() bool {
	codePoint := receiver.GetInputStream().LA(-2)<<8 + receiver.GetInputStream().LA(-1)
	var c []rune
	if codePoint < 0x10000 {
		c = []rune{rune(codePoint)}
	} else {
		codePoint -= 0x10000
		c = []rune{
			(rune)(codePoint/0x400 + 0xd800),
			(rune)(codePoint%0x400 + 0xdc00),
		}
	}
	return unicode.IsLetter(c[0])
}
