import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:convert';

abstract class Python3ParserBase extends Parser
{
    Python3ParserBase(TokenStream input) : super(input)
    {
    }

    bool CannotBePlusMinus()
    {
        return true;
    }

    bool CannotBeDotLpEq()
    {
        return true;
    }
}
