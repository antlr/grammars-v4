import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:convert';
import 'gvprParser.dart';

abstract class GvprParserBase extends Parser {
    GvprParserBase(TokenStream input) : super(input)
    {
    }

    bool IsSemiRequired()
    {
        var c = this.tokenStream.LT(-1);
        var d = this.tokenStream.LT(1);
        return c?.type != gvprParser.TOKEN_CCBC;
    }

    bool IsSemiNotRequired()
    {
        var c = this.tokenStream.LT(-1);
        var d = this.tokenStream.LT(1);
        return c?.type == gvprParser.TOKEN_CCBC;
    }
}
