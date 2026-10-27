import 'dart:io';
import 'package:antlr4/antlr4.dart';
import 'CSharpLexer.dart';

abstract class CSharpLexerBase extends Lexer {
    int interpolatedStringLevel = 0;
    List<bool> interpolatedVerbatiums = [];
    List<int> curlyLevels = [];
    bool verbatium = false;

    // Preprocessor state
    final List<Token> _pending = [];
    final Set<String> _symbols = {};
    final List<bool> _condition = [];
    final List<bool> _taken = [];

    // Expression evaluator cursor
    List<Token> _expr = [];
    int _epos = 0;

    CSharpLexerBase(CharStream input) : super(input) {
        _initPreprocessor();
    }

    void OnInterpolatedRegularStringStart() {
        interpolatedStringLevel++;
        interpolatedVerbatiums.add(false);
        verbatium = false;
    }

    void OnInterpolatedVerbatiumStringStart() {
        interpolatedStringLevel++;
        interpolatedVerbatiums.add(true);
        verbatium = true;
    }

    void OnOpenBrace() {
        if (interpolatedStringLevel > 0) {
            curlyLevels[curlyLevels.length - 1]++;
        }
    }

    void OnCloseBrace() {
        if (interpolatedStringLevel > 0) {
            curlyLevels[curlyLevels.length - 1]--;
            if (curlyLevels.last == 0) {
                curlyLevels.removeLast();
                skip();
                popMode();
            }
        }
    }

    void OnColon() {
        if (interpolatedStringLevel > 0) {
            var ind = 1;
            var switchToFormatString = true;
            while ((inputStream.LA(ind) ?? -1) != 0x7D /* } */) {
                final ch = inputStream.LA(ind) ?? -1;
                if (ch == 0x3A /* : */ || ch == 0x29 /* ) */) {
                    switchToFormatString = false;
                    break;
                }
                ind++;
            }
            if (switchToFormatString) {
                mode(CSharpLexer.INTERPOLATION_FORMAT);
            }
        }
    }

    void OpenBraceInside() {
        curlyLevels.add(1);
    }

    void OnDoubleQuoteInside() {
        interpolatedStringLevel--;
        interpolatedVerbatiums.removeLast();
        verbatium = interpolatedVerbatiums.isNotEmpty && interpolatedVerbatiums.last;
    }

    void OnCloseBraceInside() {
        curlyLevels.removeLast();
    }

    bool IsRegularCharInside() {
        return !verbatium;
    }

    bool IsVerbatiumDoubleQuoteInside() {
        return verbatium;
    }

    // -------------------------------------------------------------------------
    // Preprocessor initialisation
    // -------------------------------------------------------------------------
    void _initPreprocessor() {
        for (final arg in Platform.executableArguments) {
            if (arg.startsWith("--D")) {
                for (final sym in arg.substring(3).split(';')) {
                    if (sym.isNotEmpty) _symbols.add(sym);
                }
            }
        }
    }

    bool _isActive() {
        return _condition.isEmpty || _condition.last;
    }

    // -------------------------------------------------------------------------
    // nextToken override — intercepts DIRECTIVE-channel tokens
    // -------------------------------------------------------------------------
    @override
    Token nextToken() {
        if (_pending.isNotEmpty) return _pending.removeAt(0);

        final tok = super.nextToken();

        if (tok.channel == CSharpLexer.DIRECTIVE) {
            Token? skipped;
            switch (tok.type) {
                case CSharpLexer.TOKEN_DEFINE: _handleDefine(); break;
                case CSharpLexer.TOKEN_UNDEF:  _handleUndef();  break;
                case CSharpLexer.TOKEN_IF:     skipped = _handleIf();   break;
                case CSharpLexer.TOKEN_ELIF:   skipped = _handleElif(); break;
                case CSharpLexer.TOKEN_ELSE:   skipped = _handleElse(); break;
                case CSharpLexer.TOKEN_ENDIF:  _handleEndif();  break;
            }
            if (skipped != null) _pending.add(skipped);
        }

        return tok;
    }

    // -------------------------------------------------------------------------
    // Directive handlers
    // -------------------------------------------------------------------------
    void _handleDefine() {
        final line = _collectLine();
        final sym = _symbolFromLine(line);
        if (_isActive() && sym != null) _symbols.add(sym);
    }

    void _handleUndef() {
        final line = _collectLine();
        final sym = _symbolFromLine(line);
        if (_isActive() && sym != null) _symbols.remove(sym);
    }

    Token? _handleIf() {
        final line = _collectLine();
        final outer = _isActive();
        final result = outer && _evaluate(line);
        _condition.add(result);
        _taken.add(result);
        return result ? null : _skipFalseBlock();
    }

    Token? _handleElif() {
        final line = _collectLine();
        final alreadyTaken = _taken.isNotEmpty ? _taken.removeLast() : false;
        if (_condition.isNotEmpty) _condition.removeLast();
        final outer = _isActive();
        final result = !alreadyTaken && outer && _evaluate(line);
        _condition.add(result);
        _taken.add(alreadyTaken || result);
        return result ? null : _skipFalseBlock();
    }

    Token? _handleElse() {
        _collectLine();
        final alreadyTaken = _taken.isNotEmpty ? _taken.removeLast() : false;
        if (_condition.isNotEmpty) _condition.removeLast();
        final outer = _isActive();
        final result = !alreadyTaken && outer;
        _condition.add(result);
        _taken.add(true);
        return result ? null : _skipFalseBlock();
    }

    void _handleEndif() {
        _collectLine();
        if (_condition.isNotEmpty) _condition.removeLast();
        if (_taken.isNotEmpty)    _taken.removeLast();
    }

    // -------------------------------------------------------------------------
    // _collectLine — drain DIRECTIVE_MODE tokens up to DIRECTIVE_NEW_LINE
    // -------------------------------------------------------------------------
    List<Token> _collectLine() {
        final tokens = <Token>[];
        Token t;
        do {
            t = super.nextToken();
            if (t.channel != Token.HIDDEN_CHANNEL
                && t.channel != CSharpLexer.COMMENTS_CHANNEL)
                tokens.add(t);
        } while (t.type != CSharpLexer.TOKEN_DIRECTIVE_NEW_LINE
              && t.type != Token.EOF);
        return tokens;
    }

    String? _symbolFromLine(List<Token> line) {
        for (final t in line)
            if (t.type == CSharpLexer.TOKEN_CONDITIONAL_SYMBOL) return t.text;
        return null;
    }

    // -------------------------------------------------------------------------
    // _skipFalseBlock — scan char stream, return SKIPPED_SECTION on HIDDEN channel
    // -------------------------------------------------------------------------
    Token _skipFalseBlock() {
        final buf = StringBuffer();
        final stream = inputStream;
        var depth = 1;
        var atLineStart = true;
        final startLine = line;

        while (true) {
            final c = stream.LA(1) ?? Token.EOF;
            if (c == Token.EOF) break;

            if (c == 0x0D || c == 0x0A || c == 0x85 || c == 0x2028 || c == 0x2029) {
                stream.consume();
                buf.writeCharCode(c);
                if (c == 0x0D && (stream.LA(1) ?? -1) == 0x0A) {
                    stream.consume();
                    buf.write('\n');
                }
                atLineStart = true;
                continue;
            }

            if (atLineStart && (c == 0x20 || c == 0x09)) {
                stream.consume();
                buf.writeCharCode(c);
                continue;
            }

            if (atLineStart && c == 0x23 /* '#' */) {
                final kw = _peekKeyword();
                if (kw == "if") {
                    depth++;
                } else if (kw == "endif") {
                    depth--;
                    if (depth == 0) break;
                } else if ((kw == "else" || kw == "elif") && depth == 1) {
                    break;
                }
            }

            atLineStart = false;
            stream.consume();
            buf.writeCharCode(c);
        }

        final tok = CommonTokenFactory.DEFAULT.create(
            CSharpLexer.TOKEN_SKIPPED_SECTION,
            buf.toString(),
            Pair(this, inputStream),
            Token.HIDDEN_CHANNEL,
            -1, -1,
            startLine, 0);
        return tok;
    }

    String _peekKeyword() {
        var i = 2; // LA(1) is '#'
        while (inputStream.LA(i) == 0x20 || inputStream.LA(i) == 0x09) i++;
        final buf = StringBuffer();
        int c;
        while (true) {
            final cv = inputStream.LA(i);
            if (cv == null || cv == -1) break;
            c = cv;
            if ((c < 0x61 || c > 0x7A) && (c < 0x41 || c > 0x5A)) break;
            buf.writeCharCode(c);
            i++;
        }
        return buf.toString();
    }

    // -------------------------------------------------------------------------
    // Recursive-descent expression evaluator
    // -------------------------------------------------------------------------
    bool _evaluate(List<Token> tokens) {
        _expr = tokens;
        _epos = 0;
        return _parseOr();
    }

    int _peekType() {
        if (_epos < _expr.length) {
            final t = _expr[_epos].type;
            if (t != CSharpLexer.TOKEN_DIRECTIVE_NEW_LINE && t != Token.EOF) return t;
        }
        return -1;
    }

    Token _eConsume() => _expr[_epos++];

    bool _parseOr() {
        var v = _parseAnd();
        while (_peekType() == CSharpLexer.TOKEN_OP_OR) { _eConsume(); v = _parseAnd() || v; }
        return v;
    }

    bool _parseAnd() {
        var v = _parseEq();
        while (_peekType() == CSharpLexer.TOKEN_OP_AND) { _eConsume(); v = _parseEq() && v; }
        return v;
    }

    bool _parseEq() {
        final v = _parseUnary();
        if (_peekType() == CSharpLexer.TOKEN_OP_EQ) { _eConsume(); return v == _parseUnary(); }
        if (_peekType() == CSharpLexer.TOKEN_OP_NE) { _eConsume(); return v != _parseUnary(); }
        return v;
    }

    bool _parseUnary() {
        if (_peekType() == CSharpLexer.TOKEN_BANG) { _eConsume(); return !_parseUnary(); }
        return _parsePrimary();
    }

    bool _parsePrimary() {
        final t = _peekType();
        if (t == CSharpLexer.TOKEN_TRUE)               { _eConsume(); return true; }
        if (t == CSharpLexer.TOKEN_FALSE)              { _eConsume(); return false; }
        if (t == CSharpLexer.TOKEN_CONDITIONAL_SYMBOL) { return _symbols.contains(_eConsume().text ?? ""); }
        if (t == CSharpLexer.TOKEN_OPEN_PARENS) {
            _eConsume();
            final v = _parseOr();
            if (_peekType() == CSharpLexer.TOKEN_CLOSE_PARENS) _eConsume();
            return v;
        }
        return false;
    }
}
