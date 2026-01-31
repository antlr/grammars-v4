import 'package:antlr4/antlr4.dart';
import 'GoParser.dart';
import 'dart:collection';
import 'dart:io';

abstract class GoParserBase extends Parser
{
    static bool _debugInitialized = false;
    static bool _debug = false;
    HashSet<String> table = new HashSet<String>();

    static bool _hasArg(List<String> args, String arg) {
        for (var a in args) {
            if (a.toLowerCase().contains(arg.toLowerCase())) {
                return true;
            }
        }
        return false;
    }

    static void _initDebug() {
        if (!_debugInitialized) {
            try {
                _debug = _hasArg(Platform.executableArguments, '--debug');
                if (_debug) {
                    print('debug = $_debug');
                }
            } catch (e) {
                // Platform may not be available in all environments
                _debug = false;
            }
            _debugInitialized = true;
        }
    }

    bool get debug => _debug;

    GoParserBase(TokenStream input) : super(input) {
        _initDebug();
    }

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
        if (ctx == null) return;
        if (ctx is! ImportSpecContext) return;
        final importSpec = ctx as ImportSpecContext;
        final packageName = importSpec.packageName();
        if (packageName != null) {
            final name = packageName.text;
            if (debug) {
                print('Entering $name');
            }
            if (name != null) {
                table.add(name);
            }
            return;
        }
        final importPath = importSpec.importPath();
        if (importPath == null) return;
        var name = importPath.text;
        if (name == null) return;
        if (debug) {
            print('import path $name');
        }
        name = name.replaceAll('"', '');
        if (name.isEmpty) return;
        name = name.replaceAll('\\', '/');
        final pathArr = name.split('/');
        if (pathArr.isEmpty) return;
        final lastComponent = pathArr.last;
        if (lastComponent.isEmpty) return;
        // Handle special cases like "." and ".."
        if (lastComponent == '.' || lastComponent == '..') return;
        final fileArr = lastComponent.split('.');
        // Guard against empty array (can happen if lastComponent is all dots)
        if (fileArr.isEmpty) {
            table.add(lastComponent);
            if (debug) {
                print('Entering $lastComponent');
            }
            return;
        }
        var fileName = fileArr.last;
        if (fileName.isEmpty) {
            // Fall back to lastComponent if split resulted in empty string
            fileName = lastComponent;
        }
        if (debug) {
            print('Entering $fileName');
        }
        table.add(fileName);
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
