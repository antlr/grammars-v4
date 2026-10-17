package parser

import (
	"fmt"
	"os"
	"strings"
	"unicode"

	"github.com/antlr4-go/antlr/v4"
)

type CSharpLexerBase struct {
	*antlr.BaseLexer

	// currentMode mirrors BaseLexer.mode (unexported) so we can push it onto
	// ownModeStack when PushMode is called.
	currentMode  int
	ownModeStack []int

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
	l.currentMode = antlr.LexerDefaultMode
	l.symbols = make(map[string]bool)
	l.initPreprocessor()
}

// -------------------------------------------------------------------------
// Mode-stack helpers
// -------------------------------------------------------------------------

// SetMode shadows BaseLexer.SetMode to keep currentMode in sync.
func (l *CSharpLexerBase) SetMode(m int) {
	l.currentMode = m
	l.BaseLexer.SetMode(m)
}

// PushMode shadows BaseLexer.PushMode to keep ownModeStack in sync.
func (l *CSharpLexerBase) PushMode(m int) {
	l.ownModeStack = append(l.ownModeStack, l.currentMode)
	l.currentMode = m
	l.BaseLexer.PushMode(m)
}

// PeekMode returns the mode that PopMode would restore (top of the stack).
func (l *CSharpLexerBase) PeekMode() int {
	if len(l.ownModeStack) == 0 {
		return antlr.LexerDefaultMode
	}
	return l.ownModeStack[len(l.ownModeStack)-1]
}

func (l *CSharpLexerBase) PopMode() int {
	if len(l.ownModeStack) == 0 {
		fmt.Fprintln(os.Stderr, "unbalanced ()/{}/[]")
		return antlr.LexerDefaultMode
	}
	l.currentMode = l.ownModeStack[len(l.ownModeStack)-1]
	l.ownModeStack = l.ownModeStack[:len(l.ownModeStack)-1]
	return l.BaseLexer.PopMode()
}

func (l *CSharpLexerBase) PeekModeIs(mode int) bool {
	return l.PeekMode() == mode
}

func (l *CSharpLexerBase) LookAheadIs(pos int, value int) bool {
	return l.GetInputStream().(antlr.CharStream).LA(pos) == value
}

func (l *CSharpLexerBase) LookAheadIsNot(pos int, value int) bool {
	return l.GetInputStream().(antlr.CharStream).LA(pos) != value
}

func (l *CSharpLexerBase) LookAheadIsRBrace1() bool {
	return l.GetInputStream().(antlr.CharStream).LA(1) == '}'
}

func (l *CSharpLexerBase) LookAheadIsNotLBrace2() bool {
	return l.GetInputStream().(antlr.CharStream).LA(2) != '{'
}

func (l *CSharpLexerBase) PeekModeIsIrsCont() bool {
	return l.PeekModeIs(CSharpLexerIRS_CONT)
}

func (l *CSharpLexerBase) PeekModeIsIvsCont() bool {
	return l.PeekModeIs(CSharpLexerIVS_CONT)
}

func (l *CSharpLexerBase) WrapToken() {
	text := l.GetText()
	result := "\u3014" + strings.ReplaceAll(text, "\u3015", "\u3015\u3015") + "\u3015"
	l.SetText(result)
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
		case CSharpLexerKW_IF:
			skipped = l.handleIf()
		case CSharpLexerELIF:
			skipped = l.handleElif()
		case CSharpLexerKW_ELSE:
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
		if t.GetChannel() != antlr.TokenHiddenChannel {
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
	for l.peekType() == CSharpLexerTK_OR_OR {
		l.eConsume()
		v = l.parseAnd() || v
	}
	return v
}

func (l *CSharpLexerBase) parseAnd() bool {
	v := l.parseEq()
	for l.peekType() == CSharpLexerTK_AND_AND {
		l.eConsume()
		v = l.parseEq() && v
	}
	return v
}

func (l *CSharpLexerBase) parseEq() bool {
	v := l.parseUnary()
	if l.peekType() == CSharpLexerTK_EQ_EQ {
		l.eConsume()
		return v == l.parseUnary()
	}
	if l.peekType() == CSharpLexerTK_NOT_EQ {
		l.eConsume()
		return v != l.parseUnary()
	}
	return v
}

func (l *CSharpLexerBase) parseUnary() bool {
	if l.peekType() == CSharpLexerTK_NOT {
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
	if t == CSharpLexerTK_LPAREN {
		l.eConsume()
		v := l.parseOr()
		if l.peekType() == CSharpLexerTK_RPAREN {
			l.eConsume()
		}
		return v
	}
	return false
}
