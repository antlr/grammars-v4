import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:core';
import 'dart:convert';
import 'dart:collection';
import 'Python3Lexer.dart';
import 'Python3Parser.dart';

abstract class Python3LexerBase extends Lexer
{
    Python3LexerBase(CharStream input) : super(input)
    {
    }

    List<Token> Tokens = [];
    List<int> Indents = [];
    int Opened = 0;
    Token? LastToken = null;


    Token MakeCommonToken(int type, String text)
    {
        int stop = this.charIndex - 1;
        int start = text.length == 0 ? stop : stop - text.length + 1;
        var ctf = this.tokenFactory;
        final t = ctf.create(
            type,
            text,
            Pair<TokenSource?, CharStream?>(this, this.inputStream),
            channel,
            start,
            charIndex - 1,
            tokenStartLine,
            tokenStartCharPositionInLine);
        return t;
    }

    @override
    void emitToken(Token token)
    {
        super.emitToken(token);
        Tokens.add(token);
    }

    Token CreateDedent()
    {
        var dedent = MakeCommonToken(Python3Parser.TOKEN_DEDENT, "");
        CommonToken t = dedent as CommonToken;
        t.line = LastToken?.line;
        return dedent;
    }

    @override Token nextToken()
    {
        // Check if the end-of-file is ahead and there are still some DEDENTS expected.
        if (this.inputStream.LA(1) == IntStream.EOF && !Indents.isEmpty)
        {
            // Remove any trailing EOF tokens from our buffer.
            for (int i = Tokens.length - 1; i >= 0; --i)
            {
                var node = Tokens[i];
                if (node.type == IntStream.EOF)
                {
                    Tokens.removeAt(i);
                }
            }
            
            // First emit an extra line break that serves as the end of the statement.
            this.emitToken(MakeCommonToken(Python3Parser.TOKEN_NEWLINE, "\n"));

            // Now emit as much DEDENT tokens as needed.
            while (! Indents.isEmpty)
            {
                this.emitToken(CreateDedent());
                Indents.removeLast();
            }

            // Put the EOF back on the token stream.
            this.emitToken(MakeCommonToken(IntStream.EOF, "<EOF>"));
        }

        Token next = super.nextToken();
        if (next.channel == Token.DEFAULT_CHANNEL)
        {
            // Keep track of the last token on the default channel.
            LastToken = next;
        }

        if (Tokens.isEmpty)
        {
            return next;
        }
        else
        {
            Token x = Tokens.first;
            Tokens.removeAt(0);
            return x;
        }
    }

    int getIndentationCount(String spaces)
    {
        int count = 0;
        for (int i = 0; i < spaces.length; i++) {
            count += spaces[i] == '\t'.codeUnitAt(0) ? 8 - (count % 8) : 1;
        }
        return count;
    }

    bool atStartOfInput()
    {
        return this.charPositionInLine == 0 && this.line == 1;
    }

    void openBrace(){
        Opened++;
    }

    void closeBrace(){
        Opened--;
    }

    void onNewLine(){
        var newLine = this.text.replaceAll(RegExp("[^\r\n\f]+"), "");
        var spaces = this.text.replaceAll(RegExp("[\r\n\f]+"), "");

        // Strip newlines inside open clauses except if we are near EOF. We keep NEWLINEs near EOF to
        // satisfy the final newline needed by the single_put rule used by the REPL.
        int? next = this.inputStream.LA(1);
        int? nextnext = this.inputStream.LA(2);
        if (Opened > 0 || (nextnext != -1 && (next == '\r'.codeUnitAt(0) || next == '\n'.codeUnitAt(0) || next == '\f'.codeUnitAt(0) || next == '#'.codeUnitAt(0))))
        {
            // If we're inside a list or on a blank line, ignore all indents, 
            // dedents and line breaks.
            skip();
        }
        else
        {
            this.emitToken(MakeCommonToken(Python3Lexer.TOKEN_NEWLINE, newLine));
            int indent = getIndentationCount(spaces);
            int previous = Indents.isEmpty ? 0 : Indents.last;
            if (indent == previous)
            {
                // skip indents of the same size as the present indent-size
                skip();
            }
            else if (indent > previous) {
                Indents.add(indent);
                this.emitToken(MakeCommonToken(Python3Parser.TOKEN_INDENT, spaces));
            }
            else {
                // Possibly emit more than 1 DEDENT token.
                while(!Indents.isEmpty && Indents.last > indent)
                {
                    this.emitToken(CreateDedent());
                    Indents.removeLast();
                }
            }
        }
    }

    @override void reset([bool resetInput = false])
    {
        Tokens = List<Token>.empty(growable: true);
        Indents = [];
        Opened = 0;
        LastToken = null;
        super.reset(resetInput);
    }
}
