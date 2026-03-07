package parser

import (
	"os"
	"strings"
	"unicode"

	"github.com/antlr4-go/antlr/v4"
)

type CSharpLexerBase struct {
	*antlr.BaseLexer

	interpolatedStringLevel int
	interpolatedVerbatiums  []bool
	curlyLevels             []int
	verbatium               bool

	// Preprocessor state
	pending   []antlr.Token
	symbols   map[string]bool
	condition []bool
	taken     []bool

	// Expression evaluator cursor
	exprTokens []antlr.Token
	epos       int
}

func (l *CSharpLexerBase) init() {
	l.symbols = make(map[string]bool)
	l.initPreprocessor()
}

func (l *CSharpLexerBase) OnInterpolatedRegularStringStart() {
	l.interpolatedStringLevel++
	l.interpolatedVerbatiums = append(l.interpolatedVerbatiums, false)
	l.verbatium = false
}

func (l *CSharpLexerBase) OnInterpolatedVerbatiumStringStart() {
	l.interpolatedStringLevel++
	l.interpolatedVerbatiums = append(l.interpolatedVerbatiums, true)
	l.verbatium = true
}

func (l *CSharpLexerBase) OnOpenBrace() {
	if l.interpolatedStringLevel > 0 {
		l.curlyLevels[len(l.curlyLevels)-1]++
	}
}

func (l *CSharpLexerBase) OnCloseBrace() {
	if l.interpolatedStringLevel > 0 {
		l.curlyLevels[len(l.curlyLevels)-1]--
		if l.curlyLevels[len(l.curlyLevels)-1] == 0 {
			l.curlyLevels = l.curlyLevels[:len(l.curlyLevels)-1]
			l.Skip()
			l.PopMode()
		}
	}
}

func (l *CSharpLexerBase) OnColon() {
	if l.interpolatedStringLevel > 0 {
		ind := 1
		switchToFormatString := true
		stream := l.GetInputStream().(antlr.CharStream)
		for stream.LA(ind) != '}' {
			ch := stream.LA(ind)
			if ch == ':' || ch == ')' {
				switchToFormatString = false
				break
			}
			ind++
		}
		if switchToFormatString {
			l.SetMode(CSharpLexerINTERPOLATION_FORMAT)
		}
	}
}

func (l *CSharpLexerBase) OpenBraceInside() {
	l.curlyLevels = append(l.curlyLevels, 1)
}

func (l *CSharpLexerBase) OnDoubleQuoteInside() {
	l.interpolatedStringLevel--
	if len(l.interpolatedVerbatiums) > 0 {
		l.interpolatedVerbatiums = l.interpolatedVerbatiums[:len(l.interpolatedVerbatiums)-1]
	}
	if len(l.interpolatedVerbatiums) > 0 {
		l.verbatium = l.interpolatedVerbatiums[len(l.interpolatedVerbatiums)-1]
	} else {
		l.verbatium = false
	}
}

func (l *CSharpLexerBase) OnCloseBraceInside() {
	if len(l.curlyLevels) > 0 {
		l.curlyLevels = l.curlyLevels[:len(l.curlyLevels)-1]
	}
}

func (l *CSharpLexerBase) IsRegularCharInside() bool {
	return !l.verbatium
}

func (l *CSharpLexerBase) IsVerbatiumDoubleQuoteInside() bool {
	return l.verbatium
}

// -------------------------------------------------------------------------
// Preprocessor initialisation
// -------------------------------------------------------------------------
func (l *CSharpLexerBase) initPreprocessor() {
	for _, arg := range os.Args {
		if strings.HasPrefix(arg, "--D") {
			for _, sym := range strings.Split(arg[3:], ";") {
				if sym != "" {
					l.symbols[sym] = true
				}
			}
		}
	}
}

func (l *CSharpLexerBase) isActive() bool {
	return len(l.condition) == 0 || l.condition[len(l.condition)-1]
}

// -------------------------------------------------------------------------
// NextToken override — intercepts DIRECTIVE-channel tokens
// -------------------------------------------------------------------------
func (l *CSharpLexerBase) NextToken() antlr.Token {
	if len(l.pending) > 0 {
		tok := l.pending[0]
		l.pending = l.pending[1:]
		return tok
	}

	tok := l.BaseLexer.NextToken()

	if tok.GetChannel() == CSharpLexerDIRECTIVE {
		var skipped antlr.Token
		switch tok.GetTokenType() {
		case CSharpLexerDEFINE:
			l.handleDefine()
		case CSharpLexerUNDEF:
			l.handleUndef()
		case CSharpLexerIF:
			skipped = l.handleIf()
		case CSharpLexerELIF:
			skipped = l.handleElif()
		case CSharpLexerELSE:
			skipped = l.handleElse()
		case CSharpLexerENDIF:
			l.handleEndif()
		}
		if skipped != nil {
			l.pending = append(l.pending, skipped)
		}
	}

	return tok
}

// -------------------------------------------------------------------------
// Directive handlers
// -------------------------------------------------------------------------
func (l *CSharpLexerBase) handleDefine() {
	line := l.collectLine()
	sym := symbolFromLine(line)
	if l.isActive() && sym != "" {
		l.symbols[sym] = true
	}
}

func (l *CSharpLexerBase) handleUndef() {
	line := l.collectLine()
	sym := symbolFromLine(line)
	if l.isActive() && sym != "" {
		delete(l.symbols, sym)
	}
}

func (l *CSharpLexerBase) handleIf() antlr.Token {
	line := l.collectLine()
	outer := l.isActive()
	result := outer && l.evaluate(line)
	l.condition = append(l.condition, result)
	l.taken = append(l.taken, result)
	if result {
		return nil
	}
	return l.skipFalseBlock()
}

func (l *CSharpLexerBase) handleElif() antlr.Token {
	line := l.collectLine()
	alreadyTaken := false
	if len(l.taken) > 0 {
		alreadyTaken = l.taken[len(l.taken)-1]
		l.taken = l.taken[:len(l.taken)-1]
	}
	if len(l.condition) > 0 {
		l.condition = l.condition[:len(l.condition)-1]
	}
	outer := l.isActive()
	result := !alreadyTaken && outer && l.evaluate(line)
	l.condition = append(l.condition, result)
	l.taken = append(l.taken, alreadyTaken || result)
	if result {
		return nil
	}
	return l.skipFalseBlock()
}

func (l *CSharpLexerBase) handleElse() antlr.Token {
	l.collectLine()
	alreadyTaken := false
	if len(l.taken) > 0 {
		alreadyTaken = l.taken[len(l.taken)-1]
		l.taken = l.taken[:len(l.taken)-1]
	}
	if len(l.condition) > 0 {
		l.condition = l.condition[:len(l.condition)-1]
	}
	outer := l.isActive()
	result := !alreadyTaken && outer
	l.condition = append(l.condition, result)
	l.taken = append(l.taken, true)
	if result {
		return nil
	}
	return l.skipFalseBlock()
}

func (l *CSharpLexerBase) handleEndif() {
	l.collectLine()
	if len(l.condition) > 0 {
		l.condition = l.condition[:len(l.condition)-1]
	}
	if len(l.taken) > 0 {
		l.taken = l.taken[:len(l.taken)-1]
	}
}

// -------------------------------------------------------------------------
// collectLine — drain DIRECTIVE_MODE tokens up to DIRECTIVE_NEW_LINE
// -------------------------------------------------------------------------
func (l *CSharpLexerBase) collectLine() []antlr.Token {
	var tokens []antlr.Token
	for {
		t := l.BaseLexer.NextToken()
		if t.GetChannel() != antlr.TokenHiddenChannel &&
			t.GetChannel() != CSharpLexerCOMMENTS_CHANNEL {
			tokens = append(tokens, t)
		}
		if t.GetTokenType() == CSharpLexerDIRECTIVE_NEW_LINE ||
			t.GetTokenType() == antlr.TokenEOF {
			break
		}
	}
	return tokens
}

func symbolFromLine(line []antlr.Token) string {
	for _, t := range line {
		if t.GetTokenType() == CSharpLexerCONDITIONAL_SYMBOL {
			return t.GetText()
		}
	}
	return ""
}

// -------------------------------------------------------------------------
// skipFalseBlock — scan char stream, return SKIPPED_SECTION on HIDDEN channel
// -------------------------------------------------------------------------
func (l *CSharpLexerBase) skipFalseBlock() antlr.Token {
	var sb strings.Builder
	stream := l.GetInputStream().(antlr.CharStream)
	depth := 1
	atLineStart := true
	startLine := l.GetLine()

	for {
		c := stream.LA(1)
		if c == antlr.TokenEOF {
			break
		}

		if c == '\r' || c == '\n' || c == 0x85 || c == 0x2028 || c == 0x2029 {
			stream.Consume()
			sb.WriteRune(rune(c))
			if c == '\r' && stream.LA(1) == '\n' {
				stream.Consume()
				sb.WriteRune('\n')
			}
			atLineStart = true
			continue
		}

		if atLineStart && (c == ' ' || c == '\t') {
			stream.Consume()
			sb.WriteRune(rune(c))
			continue
		}

		if atLineStart && c == '#' {
			kw := peekKeyword(stream)
			if kw == "if" {
				depth++
			} else if kw == "endif" {
				depth--
				if depth == 0 {
					break
				}
			} else if (kw == "else" || kw == "elif") && depth == 1 {
				break
			}
		}

		atLineStart = false
		stream.Consume()
		sb.WriteRune(rune(c))
	}

	ctf := l.GetTokenFactory()
	tok := ctf.Create(
		l.GetTokenSourceCharStreamPair(),
		CSharpLexerSKIPPED_SECTION,
		sb.String(),
		antlr.TokenHiddenChannel,
		-1, -1,
		startLine, 0)
	return tok
}

func peekKeyword(stream antlr.CharStream) string {
	i := 2 // LA(1) is '#'
	for stream.LA(i) == ' ' || stream.LA(i) == '\t' {
		i++
	}
	var sb strings.Builder
	for {
		c := stream.LA(i)
		if c == antlr.TokenEOF {
			break
		}
		r := rune(c)
		if !unicode.IsLetter(r) {
			break
		}
		sb.WriteRune(r)
		i++
	}
	return sb.String()
}

// -------------------------------------------------------------------------
// Recursive-descent expression evaluator
// -------------------------------------------------------------------------
func (l *CSharpLexerBase) evaluate(tokens []antlr.Token) bool {
	l.exprTokens = tokens
	l.epos = 0
	return l.parseOr()
}

func (l *CSharpLexerBase) peekType() int {
	if l.epos < len(l.exprTokens) {
		t := l.exprTokens[l.epos].GetTokenType()
		if t != CSharpLexerDIRECTIVE_NEW_LINE && t != antlr.TokenEOF {
			return t
		}
	}
	return -1
}

func (l *CSharpLexerBase) eConsume() antlr.Token {
	tok := l.exprTokens[l.epos]
	l.epos++
	return tok
}

func (l *CSharpLexerBase) parseOr() bool {
	v := l.parseAnd()
	for l.peekType() == CSharpLexerOP_OR {
		l.eConsume()
		v = l.parseAnd() || v
	}
	return v
}

func (l *CSharpLexerBase) parseAnd() bool {
	v := l.parseEq()
	for l.peekType() == CSharpLexerOP_AND {
		l.eConsume()
		v = l.parseEq() && v
	}
	return v
}

func (l *CSharpLexerBase) parseEq() bool {
	v := l.parseUnary()
	if l.peekType() == CSharpLexerOP_EQ {
		l.eConsume()
		return v == l.parseUnary()
	}
	if l.peekType() == CSharpLexerOP_NE {
		l.eConsume()
		return v != l.parseUnary()
	}
	return v
}

func (l *CSharpLexerBase) parseUnary() bool {
	if l.peekType() == CSharpLexerBANG {
		l.eConsume()
		return !l.parseUnary()
	}
	return l.parsePrimary()
}

func (l *CSharpLexerBase) parsePrimary() bool {
	t := l.peekType()
	if t == CSharpLexerTRUE {
		l.eConsume()
		return true
	}
	if t == CSharpLexerFALSE {
		l.eConsume()
		return false
	}
	if t == CSharpLexerCONDITIONAL_SYMBOL {
		return l.symbols[l.eConsume().GetText()]
	}
	if t == CSharpLexerOPEN_PARENS {
		l.eConsume()
		v := l.parseOr()
		if l.peekType() == CSharpLexerCLOSE_PARENS {
			l.eConsume()
		}
		return v
	}
	return false
}
