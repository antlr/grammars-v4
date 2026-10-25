import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:core';
import 'dart:convert';
import 'dart:collection';
import 'RustLexer.dart';
import 'RustParser.dart';

abstract class RustParserBase extends Parser {

    RustParserBase(TokenStream input) : super(input)
    {
    }

    bool NextGT() {
        return inputStream.LA(1) == RustParser.TOKEN_GT;
    }

    bool NextLT() {
        return inputStream.LA(1) == RustParser.TOKEN_LT;
    }
}