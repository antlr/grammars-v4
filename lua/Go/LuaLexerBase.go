package parser

import (
    "github.com/antlr4-go/antlr/v4"
)

type LuaLexerBase struct {
    *antlr.BaseLexer
    start_line int
    start_col int
}

func (l *LuaLexerBase) HandleComment() {
    l.start_line = l.GetLine()
    l.start_col = l.GetCharPositionInLine() - 2
    cs := l.GetInputStream().(antlr.CharStream)
    if cs.LA(1) == '[' {
        sep := l.skip_sep(cs)
        if sep >= 2 {
            l.read_long_string(cs, sep)
            return
        }
    }
    for cs.LA(1) != '\n' && cs.LA(1) != -1 {
        cs.Consume()
    }
}

func (l *LuaLexerBase) read_long_string(cs antlr.CharStream, sep int) {
    done := false
    cs.Consume()
    for {
        c := cs.LA(1)
        switch c {
        case -1:
            done = true
            listener := l.GetErrorListenerDispatch()
            listener.SyntaxError(l, nil, l.start_line, l.start_col, "unfinished long comment", nil)
        case ']':
            if l.skip_sep(cs) == sep {
                cs.Consume()
                done = true
            }
        default:
            if cs.LA(1) == -1 {
                done = true
                break
            }
            cs.Consume()
        }
        if done {
            break
        }
    }
}

func (l *LuaLexerBase) skip_sep(cs antlr.CharStream) int {
    count := 0
    s := cs.LA(1)
    cs.Consume()
    for cs.LA(1) == '=' {
        cs.Consume()
        count++
    }
    if cs.LA(1) == s {
        count += 2
    } else if count == 0 {
        count = 1
    } else {
        count = 0
    }
    return count
}

func (l *LuaLexerBase) IsLine1Col0() bool {
	cs := l.GetInputStream().(antlr.CharStream)
	if cs.Index() == 1 {
		return true
	}
	return false
}
