import 'package:antlr4/antlr4.dart';
import 'GoParser.dart';
import 'dart:collection';

abstract class GoParserBase extends Parser
{
    final bool debug = false;
    HashSet<String> table = new HashSet<String>();

    GoParserBase(TokenStream input) : super(input);

    void myreset()
    {
        table = new HashSet<String>();
    }

    bool closingBracket()
    {
        final la = this.tokenStream.LT(1);
        return la?.type == GoParser.TOKEN_R_PAREN || la?.type == GoParser.TOKEN_R_CURLY || la?.type == IntStream.EOF;
    }

    bool isNotReceive()
    {
        final la = tokenStream.LT(2);
        return la?.type != GoParser.TOKEN_RECEIVE;
    }

    void addImportSpec()
    {
        final ctx = context;
        if (ctx == null) return; // Null check for safety
        final importSpec = ctx as ImportSpecContext;
        if (importSpec == null) return;
        final packageName = importSpec.packageName();
        if (packageName != null) {
            final name = packageName.text;
            if (debug) {
                print('Entering $name');
            }
            table.add(name);
        } else {
            var name = importSpec.importPath()?.text;
            name = name?.replaceAll('"', '');
            name = name?.replaceAll('\\', '/');
            final pathArr = name?.split('/');
            final fileArr = pathArr?.last.split('.');
            final fileName = fileArr?.last;
            if (fileName == null) return;
            if (debug) {
                print('Entering $fileName');
            }
            table.add(fileName);
        }
    }

    bool isOperand()
    {
        final la = this.tokenStream.LT(1);
        bool result = true;
        if (la?.text == "err") {
            return true;
        }
        if (la?.type != GoParser.TOKEN_IDENTIFIER) {
            if (debug) {
                print('isOperand Returning $result for ${la?.text}');
            }
            return result;
        }
        result = table.contains(la?.text);
        final la2 = this.tokenStream.LT(2);
        if (la2?.type != GoParser.TOKEN_DOT) {
            result = true;
            if (debug) {
                print('isOperand Returning $result for ${la?.text}');
            }
            return result;
        }
        final la3 = this.tokenStream.LT(3);
        if (la3?.type == GoParser.TOKEN_L_PAREN) {
            result = true;
            if (debug) {
                print('isOperand Returning $result for ${la?.text}');
            }
            return result;
        }
        if (debug) {
            print('isOperand Returning $result for ${la?.text}');
        }
        return result;
    }

    bool isConversion()
    {
        var la = this.tokenStream.LT(1);
        return la?.type != GoParser.TOKEN_IDENTIFIER;
    }

    bool isMethodExpr()
    {
        final la = this.tokenStream.LT(1);
        bool result = true;
        if (la?.type == GoParser.TOKEN_STAR) {
            if (debug) {
                print('isMethodExpr Returning $result for ${la?.text}');
            }
            return result;
        }
        if (la?.type != GoParser.TOKEN_IDENTIFIER) {
            result = false;
            if (debug) {
                print('isMethodExpr Returning $result for ${la?.text}');
            }
            return result;
        }
        result = !table.contains(la?.text);
        if (debug) {
            print('isMethodExpr Returning $result for ${la?.text}');
        }
        return result;
    }
}
