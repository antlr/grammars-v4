import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:convert';

abstract class LuaLexerBase extends Lexer
{
    int start_line = 0;
    int start_col = 0;

    LuaLexerBase(CharStream input) : super(input)
    {
    }

    bool IsColumnZero()
    {
        return charPositionInLine == 0;
    }

    void HandleComment()
    {
        start_line = this.line;
        start_col = this.charPositionInLine - 2;
        var cs = this.inputStream;
        var c = cs.LA(1);
        var yo = '['.codeUnitAt(0);
        if (cs.LA(1) == '['.codeUnitAt(0))
        {
            int sep = skip_sep(cs);
            if (sep >= 2)
            {
                read_long_string(cs, sep);
                return;
            }
        }
        for (;;) {
            var c2 = cs.LA(1);
            var yo2 = '\n'.codeUnitAt(0);
            if (c2 == yo2) break;
            if (c2 == -1) break;
            cs.consume();
        }
    }

    void read_long_string(CharStream cs, int sep)
    {
        bool done = false;
        cs.consume();
        for (; ; )
        {
            var c = cs.LA(1);
            var cc = c;
            var yo = ']'.codeUnitAt(0);
            if (c == -1)
            {
                done = true;
                final listener = this.errorListenerDispatch;
                listener.syntaxError(this, null, this.start_line, this.start_col, "unfinished long comment", null);
            } else if (c == yo) {
                if (skip_sep(cs) == sep)
                {
                    cs.consume();
                    done = true;
                }
            } else {
                if (cs.LA(1) == -1)
                {
                    done = true;
                } else {
                    cs.consume();
                }
            }
            if (done) break;
        }
    }

    int skip_sep(CharStream cs)
    {
        int count = 0;
        var s = cs.LA(1);
        cs.consume();
        var yoeq = '='.codeUnitAt(0);
        while (cs.LA(1) == yoeq)
        {
            cs.consume();
            count++;
        }
        if (cs.LA(1) == s) count += 2;
        else if (count == 0) count = 1;
        else count = 0;
        return count;
    }

    bool IsLine1Col0()
    {
        var cs = this.inputStream;
        if (cs.index == 1) return true;
        return false;
    }
}
