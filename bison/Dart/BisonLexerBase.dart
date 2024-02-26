import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:core';
import 'dart:convert';
import 'dart:collection';
import 'BisonLexer.dart';

abstract class BisonLexerBase extends Lexer
{
    BisonLexerBase(CharStream input) : super(input)
    {
    }

    int percent_percent_count = 0;

    void NextMode()
    {
        ++percent_percent_count;
        if (percent_percent_count == 1)
        {
            return;
        } else if (percent_percent_count == 2)
        {
            this.pushMode(BisonLexer.EpilogueMode);
            return;
        } else
        {
            this.type = BisonLexer.TOKEN_PercentPercent;
            return;
        }
    }

    @override void reset([bool resetInput = false])
    {
	percent_percent_count = 0;
        super.reset(resetInput);
    }
}
