package parser

import (
	"github.com/antlr4-go/antlr/v4"
)

type AdaParserBase struct {
	*antlr.BaseParser
}

func (p *AdaParserBase) ParsePragmas() {
	stream, ok := p.GetTokenStream().(*antlr.CommonTokenStream)
	if !ok {
		return
	}
	stream.Fill()
	allTokens := stream.GetAllTokens()
	const PRAGMA_CHANNEL = 2
	var currentPragma []antlr.Token
	var pragmas [][]antlr.Token
	for _, token := range allTokens {
		if token.GetChannel() != PRAGMA_CHANNEL {
			continue
		}
		if token.GetTokenType() == AdaLexerPRAGMA {
			currentPragma = []antlr.Token{token}
		} else if currentPragma != nil {
			currentPragma = append(currentPragma, token)
			if token.GetTokenType() == AdaLexerSEMI {
				pragmas = append(pragmas, currentPragma)
				currentPragma = nil
			}
		}
	}
	for _, pragmaTokens := range pragmas {
		var defaultChannelTokens []antlr.Token
		for _, t := range pragmaTokens {
			ct := antlr.NewCommonToken(nil, t.GetTokenType(), antlr.TokenDefaultChannel, t.GetStart(), t.GetStop())
			ct.SetText(t.GetText())
			ct.SetLine(t.GetLine())
			ct.SetColumn(t.GetColumn())
			ct.SetTokenIndex(t.GetTokenIndex())
			defaultChannelTokens = append(defaultChannelTokens, ct)
		}
		eof := antlr.NewCommonToken(nil, antlr.TokenEOF, antlr.TokenDefaultChannel, -1, -1)
		defaultChannelTokens = append(defaultChannelTokens, eof)
		tokenSource := antlr.NewListTokenSource(defaultChannelTokens)
		tokenStream := antlr.NewCommonTokenStream(tokenSource, antlr.TokenDefaultChannel)
		parser := NewAdaParser(tokenStream)
		parser.RemoveErrorListeners()
		for _, listener := range p.GetErrorListenerDispatch().GetChildren() {
			parser.AddErrorListener(listener)
		}
		parser.PragmaRule()
	}
}
