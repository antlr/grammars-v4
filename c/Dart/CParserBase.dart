import 'dart:io';
import 'package:antlr4/antlr4.dart';
import 'SymbolTable.dart';
import 'Symbol.dart';
import 'TypeClassification.dart';
import 'CLexer.dart';
import 'CParser.dart';

// List of all semantic function names
const List<String> ALL_SEMANTIC_FUNCTIONS = [
  "IsAlignmentSpecifier", "IsAtomicTypeSpecifier", "IsAttributeDeclaration",
  "IsAttributeSpecifier", "IsAttributeSpecifierSequence", "IsDeclaration",
  "IsDeclarationSpecifier", "IsTypeSpecifierQualifier", "IsEnumSpecifier",
  "IsFunctionSpecifier", "IsStatement", "IsStaticAssertDeclaration",
  "IsStorageClassSpecifier", "IsStructOrUnionSpecifier", "IsTypedefName",
  "IsTypeofSpecifier", "IsTypeQualifier", "IsTypeSpecifier", "IsCast",
  "IsNullStructDeclarationListExtension"
];

Set<String> parseNoSemantics(List<String> args) {
  var result = <String>{};
  for (var a in args) {
    var lower = a.toLowerCase();
    if (lower.startsWith("--no-semantics")) {
      var eqIndex = a.indexOf('=');
      if (eqIndex == -1) {
        // --no-semantics without value: disable all semantic functions
        for (var func in ALL_SEMANTIC_FUNCTIONS) {
          result.add(func);
        }
      } else {
        // --no-semantics=Func1,Func2,...
        var value = a.substring(eqIndex + 1);
        var funcs = value.split(',');
        for (var func in funcs) {
          result.add(func.trim());
        }
      }
    }
  }
  return result;
}

abstract class CParserBase extends Parser {
  late SymbolTable _st;
  bool _debug = false;
  bool _outputSymbolTable = false;
  Set<String> _noSemantics = <String>{};

  CParserBase(TokenStream input) : super(input) {
    // Get options from command line args
    var args = Platform.executableArguments;
    _noSemantics = parseNoSemantics(args);
    _debug = args.any((a) => a.toLowerCase().contains("--debug"));
    _outputSymbolTable = args.any((a) => a.toLowerCase().contains("--output-symbol-table"));
    _st = SymbolTable();
  }

  bool IsAlignmentSpecifier() {
    if (_noSemantics.contains("IsAlignmentSpecifier")) return true;
    var lt1 = (inputStream as CommonTokenStream).LT(1);
    var text = lt1?.text ?? "";
    if (_debug) stdout.write("IsAlignmentSpecifier $lt1");
    var resolved = _st.resolve(text);
    bool result = false;
    if (resolved == null) {
      result = false;
    } else if (resolved.classification.contains(TypeClassification.alignmentSpecifier)) {
      result = true;
    } else {
      result = false;
    }
    if (_debug) print(" $result");
    return result;
  }

  bool IsAtomicTypeSpecifier() {
    if (_noSemantics.contains("IsAtomicTypeSpecifier")) return true;
    var lt1 = (inputStream as CommonTokenStream).LT(1);
    var text = lt1?.text ?? "";
    if (_debug) stdout.write("IsAtomicTypeSpecifier $lt1");
    var resolved = _st.resolve(text);
    bool result = false;
    if (resolved == null) {
      result = false;
    } else if (resolved.classification.contains(TypeClassification.atomicTypeSpecifier)) {
      result = true;
    } else {
      result = false;
    }
    if (_debug) print(" $result");
    return result;
  }

  bool IsAttributeDeclaration() {
    if (_noSemantics.contains("IsAttributeDeclaration")) return true;
    return IsAttributeSpecifierSequence();
  }

  bool IsAttributeSpecifier() {
    if (_noSemantics.contains("IsAttributeSpecifier")) return true;
    var lt1 = (inputStream as CommonTokenStream).LT(1);
    if (_debug) stdout.write("IsAttributeSpecifier $lt1");
    var result = lt1?.type == CLexer.TOKEN_LeftBracket;
    if (_debug) print(" $result");
    return result;
  }

  bool IsAttributeSpecifierSequence() {
    if (_noSemantics.contains("IsAttributeSpecifierSequence")) return true;
    return IsAttributeSpecifier();
  }

  bool IsDeclaration() {
    if (_noSemantics.contains("IsDeclaration")) return true;
    if (_debug) print("IsDeclaration");
    var result = IsDeclarationSpecifiers() ||
        IsAttributeSpecifierSequence() ||
        IsStaticAssertDeclaration() ||
        IsAttributeDeclaration();
    if (_debug) print("IsDeclaration $result");
    return result;
  }

  bool IsDeclarationSpecifier() {
    if (_noSemantics.contains("IsDeclarationSpecifier")) return true;
    var lt1 = (inputStream as CommonTokenStream).LT(1);
    if (_debug) print("IsDeclarationSpecifier $lt1");
    var result = IsStorageClassSpecifier() ||
        IsTypeSpecifier() ||
        IsTypeQualifier() ||
        IsFunctionSpecifier() ||
        IsAlignmentSpecifier();
    if (_debug) print("IsDeclarationSpecifier $result for $lt1");
    return result;
  }

  bool IsTypeSpecifierQualifier() {
    if (_noSemantics.contains("IsTypeSpecifierQualifier")) return true;
    if (_debug) print("IsTypeSpecifierQualifier");
    var result = IsTypeSpecifier() || IsTypeQualifier() || IsAlignmentSpecifier();
    if (_debug) print("IsTypeSpecifierQualifier $result");
    return result;
  }

  bool IsDeclarationSpecifiers() {
    return IsDeclarationSpecifier();
  }

  bool IsEnumSpecifier() {
    if (_noSemantics.contains("IsEnumSpecifier")) return true;
    var lt1 = (inputStream as CommonTokenStream).LT(1);
    if (_debug) stdout.write("IsEnumSpecifier $lt1");
    var result = lt1?.type == CLexer.TOKEN_Enum;
    if (_debug) print(" $result");
    return result;
  }

  bool IsFunctionSpecifier() {
    if (_noSemantics.contains("IsFunctionSpecifier")) return true;
    var lt1 = (inputStream as CommonTokenStream).LT(1);
    var text = lt1?.text ?? "";
    if (_debug) stdout.write("IsFunctionSpecifier $lt1");
    var resolved = _st.resolve(text);
    bool result = false;
    if (resolved == null) {
      result = false;
    } else if (resolved.classification.contains(TypeClassification.functionSpecifier)) {
      result = true;
    } else {
      result = false;
    }
    if (_debug) print("IsFunctionSpecifier $result");
    return result;
  }

  bool IsStatement() {
    if (_noSemantics.contains("IsStatement")) return true;
    var t1 = (inputStream as CommonTokenStream).LT(1);
    var t2 = (inputStream as CommonTokenStream).LT(2);
    if (_debug) print("IsStatement1 $t1");
    if (_debug) print("IsStatement2 $t2");
    if (t1?.type == CLexer.TOKEN_Identifier && t2?.type == CLexer.TOKEN_Colon) {
      if (_debug) stdout.write("IsStatement3 true");
      return true;
    }
    var result = !IsDeclaration();
    if (_debug) stdout.write("IsStatement $result");
    return result;
  }

  bool IsStaticAssertDeclaration() {
    if (_noSemantics.contains("IsStaticAssertDeclaration")) return true;
    var token = (inputStream as CommonTokenStream).LT(1);
    if (_debug) stdout.write("IsStaticAssertDeclaration $token");
    var result = token?.type == CLexer.TOKEN_Static_assert;
    if (_debug) print(" $result");
    return result;
  }

  bool IsStorageClassSpecifier() {
    if (_noSemantics.contains("IsStorageClassSpecifier")) return true;
    var lt1 = (inputStream as CommonTokenStream).LT(1);
    var text = lt1?.text ?? "";
    if (_debug) stdout.write("IsStorageClassSpecifier $lt1");
    var resolved = _st.resolve(text);
    bool result = false;
    if (resolved == null) {
      result = false;
    } else if (resolved.classification.contains(TypeClassification.storageClassSpecifier)) {
      result = true;
    } else {
      result = false;
    }
    if (_debug) print(" $result");
    return result;
  }

  bool IsStructOrUnionSpecifier() {
    if (_noSemantics.contains("IsStructOrUnionSpecifier")) return true;
    var token = (inputStream as CommonTokenStream).LT(1);
    if (_debug) stdout.write("IsStructOrUnionSpecifier $token");
    var result = token?.type == CLexer.TOKEN_Struct || token?.type == CLexer.TOKEN_Union;
    if (_debug) print(" $result");
    return result;
  }

  bool IsTypedefName() {
    if (_noSemantics.contains("IsTypedefName")) return true;
    var lt1 = (inputStream as CommonTokenStream).LT(1);
    var text = lt1?.text ?? "";
    if (_debug) stdout.write("IsTypedefName $lt1");
    var resolved = _st.resolve(text);
    bool result = false;
    if (resolved == null) {
      result = false;
    } else if (resolved.classification.contains(TypeClassification.variable)) {
      result = false;
    } else if (resolved.classification.contains(TypeClassification.function_)) {
      result = false;
    } else {
      result = true;
    }
    if (_debug) print(" $result");
    return result;
  }

  bool IsTypeofSpecifier() {
    if (_noSemantics.contains("IsTypeofSpecifier")) return true;
    var token = (inputStream as CommonTokenStream).LT(1);
    if (_debug) stdout.write("IsTypeofSpecifier $token");
    var result = token?.type == CLexer.TOKEN_Typeof || token?.type == CLexer.TOKEN_Typeof_unqual;
    if (_debug) print(" $result");
    return result;
  }

  bool IsTypeQualifier() {
    if (_noSemantics.contains("IsTypeQualifier")) return true;
    var lt1 = (inputStream as CommonTokenStream).LT(1);
    var text = lt1?.text ?? "";
    if (_debug) stdout.write("IsTypeQualifier $lt1");
    var resolved = _st.resolve(text);
    bool result = false;
    if (resolved == null) {
      result = false;
    } else if (resolved.classification.contains(TypeClassification.typeQualifier)) {
      result = true;
    } else {
      result = false;
    }
    if (_debug) print(" $result");
    return result;
  }

  bool IsTypeSpecifier() {
    if (_noSemantics.contains("IsTypeSpecifier")) return true;
    var lt1 = (inputStream as CommonTokenStream).LT(1);
    var text = lt1?.text ?? "";
    if (_debug) stdout.write("IsTypeSpecifier $lt1");
    var resolved = _st.resolve(text);
    bool result = false;
    if (resolved == null) {
      result = false;
    } else if (resolved.classification.contains(TypeClassification.typeSpecifier)) {
      result = true;
    } else {
      result = false;
    }

    if (result) {
      if (_debug) print(" $result");
      return result;
    }
    result = IsAtomicTypeSpecifier() ||
        IsStructOrUnionSpecifier() ||
        IsEnumSpecifier() ||
        IsTypedefName() ||
        IsTypeofSpecifier();
    if (_debug) print(" $result");
    return result;
  }

  String? _myGetDeclarationId(DeclaratorContext? y) {
    // Go down the tree and find a declarator with Identifier.
    if (y == null) return null;

    // Check if this declarator has a direct declarator with an identifier
    var directDeclarator = y.directDeclarator();
    if (directDeclarator != null) {
      var more = directDeclarator.declarator();
      var xxx = _myGetDeclarationId(more);
      if (xxx != null) return xxx;
      if (directDeclarator.Identifier() != null) {
        return directDeclarator.Identifier()!.text;
      }
    }

    return null;
  }

  void EnterDeclaration() {
    if (_debug) print("EnterDeclaration");
    ParserRuleContext? ctx = context;
    for (; ctx != null; ctx = ctx.parent) {
      if (ctx is DeclarationContext) {
        var declarationContext = ctx;
        var declarationSpecifiers = declarationContext.declarationSpecifiers();
        var declarationSpecifierList = declarationSpecifiers?.declarationSpecifiers();

        bool isTypedef = false;
        if (declarationSpecifierList != null) {
          for (var ds in declarationSpecifierList) {
            if (ds.storageClassSpecifier()?.Typedef() != null) {
              isTypedef = true;
              break;
            }
          }
          for (var ds in declarationSpecifierList) {
            var typeSpec = ds.typeSpecifier();
            if (typeSpec != null) {
              var sous = typeSpec.structOrUnionSpecifier();
              if (sous != null) {
                var id = sous.Identifier();
                if (id != null) {
                  var text = id.text;
                  var symbol = Symbol();
                  symbol.name = text ?? "";
                  symbol.classification = {TypeClassification.typeSpecifier};
                  _st.define(symbol);
                  if (_debug) print("New symbol Declaration1 Declarator $symbol");
                }
              }
            }
          }
        }

        var initDeclarationList = declarationContext.initDeclaratorList();
        var initDeclarators = initDeclarationList?.initDeclarators();

        if (initDeclarators != null) {
          for (var id in initDeclarators) {
            var y = id.declarator();
            var mid = _myGetDeclarationId(y);
            if (mid != null) {
              var text = mid;
              if (isTypedef) {
                var symbol = Symbol();
                symbol.name = text;
                symbol.classification = {TypeClassification.typeSpecifier};
                _st.define(symbol);
                if (_debug) print("New symbol Declaration2 Declarator $symbol");
              } else {
                var symbol = Symbol();
                symbol.name = text;
                symbol.classification = {TypeClassification.variable};
                _st.define(symbol);
                if (_debug) print("New symbol Declaration3 Declarator $symbol");
              }
            }
          }
        }
      }
      if (ctx is FunctionDefinitionContext) {
        var fd = ctx;
        var de = fd.declarator();
        if (de == null) continue;
        var dd = de.directDeclarator();
        if (dd == null) continue;
        if (dd.Identifier() != null) {
          var text = dd.Identifier()!.text;
          var symbol = Symbol();
          symbol.name = text ?? "";
          symbol.classification = {TypeClassification.function_};
          _st.define(symbol);
          if (_debug) print("New symbol Declarationf Declarator $symbol");
          return;
        }
      }
    }
  }

  // Define to return "true" because "gcc -c -std=c2x" accepts an empty
  // struct-declaration-list.
  bool IsNullStructDeclarationListExtension() {
    if (_noSemantics.contains("IsNullStructDeclarationListExtension")) return true;
    return true;
  }

  void OutputSymbolTable() {
    if (_outputSymbolTable) {
      stderr.writeln(_st.toString());
    }
  }

  bool IsCast() {
    var result = false;
    // Look for a cast.
    if (_noSemantics.contains("IsCast")) return true;
    var t1 = (inputStream as CommonTokenStream).LT(1);
    var t2 = (inputStream as CommonTokenStream).LT(2);
    if (_debug) print("IsCast1 $t1");
    if (_debug) print("IsCast2 $t2");
    if (t1?.type != CLexer.TOKEN_LeftParen) {
      if (_debug) stdout.write("IsCast $result");
    } else if (t2?.type != CLexer.TOKEN_Identifier) {
      // Assume typecast until otherwise.
      result = true;
    } else {
      // Check id.
      var text = t2?.text ?? "";
      var resolved = _st.resolve(text);
      if (resolved == null) {
        // It's not in symbol table.
        result = false;
      } else if (resolved.classification.contains(TypeClassification.typeSpecifier)) {
        result = true;
      } else {
        result = false;
      }
    }
    if (_debug) stdout.write("IsStatement $result");
    return result;
  }
}
