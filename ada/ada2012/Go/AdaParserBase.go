package parser

import (
	"github.com/antlr4-go/antlr/v4"
)

type AdaParserBase struct {
	*antlr.BaseParser
}

// pragmaTokenSource wraps a list of tokens as a Lexer for CommonTokenStream.
type pragmaTokenSource struct {
	*antlr.BaseLexer
	toks []antlr.Token
	pos  int
}

func (s *pragmaTokenSource) NextToken() antlr.Token {
	if s.pos >= len(s.toks) {
		return s.toks[len(s.toks)-1] // EOF
	}
	t := s.toks[s.pos]
	s.pos++
	return t
}

func (s *pragmaTokenSource) GetATN() *antlr.ATN {
	return nil
}

func (s *pragmaTokenSource) GetSourceName() string {
	return "pragma"
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
		source := pragmaTokens[0].GetSource()
		var tokens []antlr.Token
		for _, t := range pragmaTokens {
			ct := antlr.NewCommonToken(source, t.GetTokenType(), antlr.TokenDefaultChannel, t.GetStart(), t.GetStop())
			ct.SetText(t.GetText())
			tokens = append(tokens, ct)
		}
		eof := antlr.NewCommonToken(source, antlr.TokenEOF, antlr.TokenDefaultChannel, -1, -1)
		tokens = append(tokens, eof)
		src := &pragmaTokenSource{
			BaseLexer: &antlr.BaseLexer{BaseRecognizer: antlr.NewBaseRecognizer()},
			toks:      tokens,
		}
		tokenStream := antlr.NewCommonTokenStream(src, antlr.TokenDefaultChannel)
		parser := NewAdaParser(tokenStream)
		parser.RemoveErrorListeners()
		parser.AddErrorListener(p.GetErrorListenerDispatch())
		parser.PragmaRule()
	}
}
