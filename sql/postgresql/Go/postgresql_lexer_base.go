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
