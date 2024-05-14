import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:convert';

abstract class Fortran90LexerBase extends Lexer
{
    Fortran90LexerBase(CharStream input) : super(input)
    {
    }

    bool IsColumnZero()
    {
        return charPositionInLine == 0;
    }

    bool VerifyNotOperator()
    {
        var c1 = this.inputStream.LA(1);
        if (c1 == "a".codeUnitAt(0))
        {
            var c2 = this.inputStream.LA(2);
            if (c2 == "n".codeUnitAt(0))
            {
                var c3 = this.inputStream.LA(3);
                if (c3 == "d".codeUnitAt(0))
                {
                    var c4 = this.inputStream.LA(4);
                    if (c4 == ".".codeUnitAt(0))
                    {
                        return false;
                    }
                }
            }
        }
        else if (c1 == 'o')
        {
            var c2 = this.inputStream.LA(2);
            if (c2 == "r".codeUnitAt(0))
            {
                var c3 = this.inputStream.LA(3);
                if (c3 == ".".codeUnitAt(0))
                {
                    return false;
                }
            }
        }
        return true;
    }
}
