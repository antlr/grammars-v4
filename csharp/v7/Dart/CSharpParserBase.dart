import 'dart:io';
import 'package:antlr4/antlr4.dart';
import 'CSharpParser.dart';

const List<String> ALL_SEMANTIC_FUNCTIONS = ["IsLocalVariableDeclaration"];

Set<String> parseNoSemantics(List<String> args) {
    final result = <String>{};
    for (final a in args) {
        if (a.toLowerCase().startsWith("--no-semantics")) {
            final eq = a.indexOf('=');
            if (eq == -1) {
                for (final f in ALL_SEMANTIC_FUNCTIONS) result.add(f);
            } else {
                for (final f in a.substring(eq + 1).split(',')) result.add(f.trim());
            }
        }
    }
    return result;
}

abstract class CSharpParserBase extends Parser {
    late Set<String> _noSemantics;

    CSharpParserBase(TokenStream input) : super(input) {
        _noSemantics = parseNoSemantics(Platform.executableArguments);
    }

    bool adjacentTokens(Token? first, Token? second) =>
        first != null && second != null &&
        first.tokenIndex + 1 == second.tokenIndex;

    bool IsLocalVariableDeclaration() {
        if (_noSemantics.contains("IsLocalVariableDeclaration")) return true;
        final local_var_decl = context;
        if (local_var_decl is! Local_variable_declarationContext) return true;
        final local_variable_type = local_var_decl.local_variable_type();
        if (local_variable_type == null) return true;
        if (local_variable_type.text == "var") return false;
        return true;
    }
}
