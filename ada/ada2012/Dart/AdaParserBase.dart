import 'dart:io';
import 'package:antlr4/antlr4.dart';
import 'AdaLexer.dart';
import 'AdaParser.dart';
import 'SymbolTable.dart';
import 'Symbol.dart' as ada;
import 'TypeClassification.dart';

abstract class AdaParserBase extends Parser {
    late SymbolTable _st;
    final List<ada.Symbol?> _expectedTypeStack = [];
    bool _debug = false;
    bool _outputSymbolTable = false;
    bool _outputAppliedOccurrences = false;
    final Set<String> _noSemantics = {};
    final List<String> _searchPaths = [];
    static final Map<String, List<ada.Symbol>> _packageCache = {};
    static final Set<String> _parsingInProgress = {};
    String _currentFile = "";

    static const List<String> _allSemanticFunctions = ["IsAggregate", "IsTypeName"];

    AdaParserBase(TokenStream input) : super(input) {
        _st = SymbolTable();
    }

    bool IsAggregate() {
        if (_noSemantics.contains("IsAggregate")) return true;
        var stream = tokenStream as CommonTokenStream;
        var lt1 = stream.LT(1)!;
        if (lt1.type != AdaLexer.TOKEN_LP) return false;
        int depth = 0;
        for (int i = 2; ; i++) {
            var t = stream.LT(i);
            if (t == null || t.type == Token.EOF) break;
            if (t.type == AdaLexer.TOKEN_LP) {
                depth++;
            } else if (t.type == AdaLexer.TOKEN_RP) {
                if (depth == 0) break;
                depth--;
            } else if (depth == 0) {
                if (t.type == AdaLexer.TOKEN_COMMA) return true;
                if (t.type == AdaLexer.TOKEN_ARROW) return true;
                if (t.type == AdaLexer.TOKEN_WITH) return true;
                if (t.type == AdaLexer.TOKEN_NULL_) {
                    var next = stream.LT(i + 1);
                    if (next != null && next.type == AdaLexer.TOKEN_RECORD) return true;
                }
            }
        }
        if (_expectedTypeStack.isNotEmpty) {
            var expected = _expectedTypeStack.last;
            if (expected != null && expected.isComposite) return true;
        }
        return false;
    }

    bool IsTypeName() {
        if (_noSemantics.contains("IsTypeName")) return true;
        var stream = tokenStream as CommonTokenStream;
        var lt1 = stream.LT(1)!;
        if (lt1.type != AdaLexer.TOKEN_IDENTIFIER_) return false;
        var firstName = lt1.text!;
        var resolved = _st.resolve(firstName);
        return resolved != null && resolved.classification.contains(TypeClassification.typeName_);
    }

    void EnterDeclaration() {
        ParserRuleContext? ctx = context;
        while (ctx != null) {
            if (ctx is Full_type_declarationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) {
                    bool isComposite = false;
                    var typeDef = ctx.type_definition();
                    if (typeDef != null) {
                        isComposite = typeDef.record_type_definition() != null || typeDef.array_type_definition() != null;
                    }
                    _defineSymbol(defId.text, TypeClassification.typeName_, defId.start, isComposite);
                }
                return;
            }
            if (ctx is Subtype_declarationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) {
                    bool isComposite = false;
                    var si = ctx.subtype_indication();
                    if (si != null) {
                        var sm = si.subtype_mark();
                        if (sm != null) {
                            var baseSym = _st.resolve(sm.text);
                            if (baseSym != null) isComposite = baseSym.isComposite;
                        }
                    }
                    _defineSymbol(defId.text, TypeClassification.typeName_, defId.start, isComposite);
                }
                return;
            }
            if (ctx is Object_declarationContext) {
                var defIdList = ctx.defining_identifier_list();
                if (defIdList != null) {
                    for (var defId in defIdList.defining_identifiers()) {
                        _defineSymbol(defId.text, TypeClassification.objectName_, defId.start);
                    }
                }
                return;
            }
            if (ctx is Number_declarationContext) {
                var defIdList = ctx.defining_identifier_list();
                if (defIdList != null) {
                    for (var defId in defIdList.defining_identifiers()) {
                        _defineSymbol(defId.text, TypeClassification.objectName_, defId.start);
                    }
                }
                return;
            }
            if (ctx is Subprogram_declarationContext) {
                _defineSubprogramFromSpec(ctx.subprogram_specification());
                return;
            }
            if (ctx is Subprogram_bodyContext) {
                _defineSubprogramFromSpec(ctx.subprogram_specification());
                return;
            }
            if (ctx is Package_declarationContext) {
                var pkgSpec = ctx.package_specification();
                if (pkgSpec != null) {
                    var dpun = pkgSpec.defining_program_unit_name();
                    if (dpun != null) {
                        var defId = dpun.defining_identifier();
                        if (defId != null) _defineSymbol(defId.text, TypeClassification.packageName_, defId.start);
                    }
                }
                return;
            }
            if (ctx is Package_bodyContext) {
                var dpun = ctx.defining_program_unit_name();
                if (dpun != null) {
                    var defId = dpun.defining_identifier();
                    if (defId != null) _defineSymbol(defId.text, TypeClassification.packageName_, defId.start);
                }
                return;
            }
            if (ctx is Exception_declarationContext) {
                var defIdList = ctx.defining_identifier_list();
                if (defIdList != null) {
                    for (var defId in defIdList.defining_identifiers()) {
                        _defineSymbol(defId.text, TypeClassification.exceptionName_, defId.start);
                    }
                }
                return;
            }
            if (ctx is Task_type_declarationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.typeName_, defId.start);
                return;
            }
            if (ctx is Single_task_declarationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.objectName_, defId.start);
                return;
            }
            if (ctx is Protected_type_declarationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.typeName_, defId.start);
                return;
            }
            if (ctx is Single_protected_declarationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.objectName_, defId.start);
                return;
            }
            if (ctx is Entry_declarationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.subprogramName_, defId.start);
                return;
            }
            if (ctx is Component_declarationContext) {
                var defIdList = ctx.defining_identifier_list();
                if (defIdList != null) {
                    for (var defId in defIdList.defining_identifiers()) {
                        _defineSymbol(defId.text, TypeClassification.componentName_, defId.start);
                    }
                }
                return;
            }
            if (ctx is Incomplete_type_declarationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.typeName_, defId.start);
                return;
            }
            if (ctx is Private_type_declarationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.typeName_, defId.start);
                return;
            }
            if (ctx is Private_extension_declarationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.typeName_, defId.start, true);
                return;
            }
            if (ctx is Generic_instantiationContext) {
                var dpun = ctx.defining_program_unit_name();
                if (dpun != null) {
                    var defId = dpun.defining_identifier();
                    if (defId != null) {
                        var tc = TypeClassification.packageName_;
                        if (ctx.PROCEDURE() != null || ctx.FUNCTION() != null) tc = TypeClassification.subprogramName_;
                        _defineSymbol(defId.text, tc, defId.start);
                    }
                }
                var dd = ctx.defining_designator();
                if (dd != null) {
                    var ddDpun = dd.defining_program_unit_name();
                    if (ddDpun != null) {
                        var defId = ddDpun.defining_identifier();
                        if (defId != null) _defineSymbol(defId.text, TypeClassification.subprogramName_, defId.start);
                    }
                }
                return;
            }
            if (ctx is Object_renaming_declarationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.objectName_, defId.start);
                return;
            }
            if (ctx is Exception_renaming_declarationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.exceptionName_, defId.start);
                return;
            }
            if (ctx is Package_renaming_declarationContext) {
                var dpun = ctx.defining_program_unit_name();
                if (dpun != null) {
                    var defId = dpun.defining_identifier();
                    if (defId != null) _defineSymbol(defId.text, TypeClassification.packageName_, defId.start);
                }
                return;
            }
            if (ctx is Formal_complete_type_declarationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.typeName_, defId.start);
                return;
            }
            if (ctx is Formal_incomplete_type_declarationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.typeName_, defId.start);
                return;
            }
            if (ctx is Formal_object_declarationContext) {
                var defIdList = ctx.defining_identifier_list();
                if (defIdList != null) {
                    for (var defId in defIdList.defining_identifiers()) {
                        _defineSymbol(defId.text, TypeClassification.objectName_, defId.start);
                    }
                }
                return;
            }
            if (ctx is Formal_package_declarationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.packageName_, defId.start);
                return;
            }
            if (ctx is Parameter_specificationContext) {
                var defIdList = ctx.defining_identifier_list();
                if (defIdList != null) {
                    for (var defId in defIdList.defining_identifiers()) {
                        _defineSymbol(defId.text, TypeClassification.objectName_, defId.start);
                    }
                }
                return;
            }
            if (ctx is Loop_parameter_specificationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.objectName_, defId.start);
                return;
            }
            if (ctx is Iterator_specificationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.objectName_, defId.start);
                return;
            }
            if (ctx is Enumeration_literal_specificationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.enumerationLiteral_, defId.start);
                return;
            }
            if (ctx is Choice_parameter_specificationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.objectName_, defId.start);
                return;
            }
            if (ctx is Entry_index_specificationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.objectName_, defId.start);
                return;
            }
            if (ctx is Extended_return_object_declarationContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.objectName_, defId.start);
                return;
            }
            if (ctx is Entry_bodyContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.subprogramName_, defId.start);
                return;
            }
            if (ctx is Task_bodyContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.objectName_, defId.start);
                return;
            }
            if (ctx is Protected_bodyContext) {
                var defId = ctx.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.objectName_, defId.start);
                return;
            }
            if (ctx is Discriminant_specificationContext) {
                var defIdList = ctx.defining_identifier_list();
                if (defIdList != null) {
                    for (var defId in defIdList.defining_identifiers()) {
                        _defineSymbol(defId.text, TypeClassification.objectName_, defId.start);
                    }
                }
                return;
            }
            ctx = ctx.parent as ParserRuleContext?;
        }
    }

    void _defineSubprogramFromSpec(Subprogram_specificationContext? spec) {
        if (spec == null) return;
        var procSpec = spec.procedure_specification();
        if (procSpec != null) {
            var dpun = procSpec.defining_program_unit_name();
            if (dpun != null) {
                var defId = dpun.defining_identifier();
                if (defId != null) _defineSymbol(defId.text, TypeClassification.subprogramName_, defId.start);
            }
            return;
        }
        var funcSpec = spec.function_specification();
        if (funcSpec != null) {
            var dd = funcSpec.defining_designator();
            if (dd != null) {
                var dpun = dd.defining_program_unit_name();
                if (dpun != null) {
                    var defId = dpun.defining_identifier();
                    if (defId != null) _defineSymbol(defId.text, TypeClassification.subprogramName_, defId.start);
                }
            }
        }
    }

    void _defineSymbol(String name, TypeClassification classification, Token? token, [bool isComposite = false]) {
        var sym = ada.Symbol()
          ..name = name
          ..classification = {classification}
          ..isComposite = isComposite
          ..definedFile = token?.tokenSource?.sourceName ?? ""
          ..definedLine = token?.line ?? 0
          ..definedColumn = token?.charPositionInLine ?? 0;
        _st.define(sym);
    }

    void EnterScope() {
        _st.pushBlockScope();
    }

    void ExitScope() {
        _st.popBlockScope();
    }

    void PushExpectedType() {
        _expectedTypeStack.add(null);
    }

    void PopExpectedType() {
        if (_expectedTypeStack.isNotEmpty) _expectedTypeStack.removeLast();
    }

    void OutputSymbolTable() {
        if (_outputSymbolTable) {
            stderr.writeln(_st.toString());
        }
    }

    void ImportWithClause() {
        if (_noSemantics.contains("IsTypeName") && _noSemantics.contains("IsAggregate")) return;

        // Auto-detect current file from token stream
        if (_currentFile.isEmpty) {
            var stream = tokenStream;
            if (stream is CommonTokenStream) {
                var sourceName = stream.tokenSource.sourceName;
                if (sourceName != null && sourceName.isNotEmpty && sourceName != "unknown") {
                    var f = File(sourceName);
                    if (f.existsSync()) {
                        _currentFile = f.absolute.path;
                    }
                }
            }
        }

        var ctx = context;
        List<dynamic>? names;
        if (ctx is Nonlimited_with_clauseContext) {
            names = ctx.names();
        } else if (ctx is Limited_with_clauseContext) {
            names = ctx.names();
        }
        if (names == null || names.isEmpty) return;

        for (var nameCtx in names) {
            var packageName = nameCtx.text;
            if (_debug) stderr.writeln("ImportWithClause: processing 'with $packageName'");

            var fileName = _packageNameToFileName(packageName);
            var cacheKey = packageName.toLowerCase();

            if (_packageCache.containsKey(cacheKey)) {
                if (_debug) stderr.writeln("ImportWithClause: using cached symbols for $packageName");
                for (var sym in _packageCache[cacheKey]!) {
                    var copy = ada.Symbol()
                      ..name = sym.name
                      ..classification = Set.from(sym.classification)
                      ..isComposite = sym.isComposite
                      ..definedFile = sym.definedFile
                      ..definedLine = sym.definedLine
                      ..definedColumn = sym.definedColumn;
                    _st.define(copy);
                }
                continue;
            }

            var adsPath = _findAdsFile(fileName);
            if (adsPath == null) {
                if (_debug) stderr.writeln("ImportWithClause: could not find $fileName");
                continue;
            }

            var fullPath = File(adsPath).absolute.path.toLowerCase();
            if (_parsingInProgress.contains(fullPath)) {
                if (_debug) stderr.writeln("ImportWithClause: skipping $fileName (cycle detected)");
                continue;
            }

            var symbols = _parseAdsFile(adsPath);
            if (symbols != null) {
                _packageCache[cacheKey] = symbols;
                for (var sym in symbols) {
                    var copy = ada.Symbol()
                      ..name = sym.name
                      ..classification = Set.from(sym.classification)
                      ..isComposite = sym.isComposite
                      ..definedFile = sym.definedFile
                      ..definedLine = sym.definedLine
                      ..definedColumn = sym.definedColumn;
                    _st.define(copy);
                    if (_debug) stderr.writeln("ImportWithClause: imported symbol ${sym.name} from $packageName");
                }
            }
        }
    }

    String _packageNameToFileName(String packageName) {
        return packageName.toLowerCase().replaceAll('.', '-') + ".ads";
    }

    String? _findAdsFile(String fileName) {
        if (_currentFile.isNotEmpty) {
            var dir = File(_currentFile).parent.path;
            var candidate = '$dir/$fileName';
            if (File(candidate).existsSync()) return candidate;
        }
        for (var searchPath in _searchPaths) {
            var candidate = '$searchPath/$fileName';
            if (File(candidate).existsSync()) return candidate;
        }
        return null;
    }

    List<ada.Symbol>? _parseAdsFile(String adsPath) {
        var fullPath = File(adsPath).absolute.path.toLowerCase();
        _parsingInProgress.add(fullPath);
        try {
            if (_debug) stderr.writeln("ImportWithClause: parsing $adsPath");
            var content = File(adsPath).readAsStringSync();
            var input = InputStream.fromString(content);
            var lexer = AdaLexer(input);
            lexer.removeErrorListeners();
            var stream = CommonTokenStream(lexer);
            var parser = AdaParser(stream);
            parser.removeErrorListeners();
            parser._currentFile = File(adsPath).absolute.path;
            parser.compilation();
            return parser._st.getExportedSymbols();
        } catch (ex) {
            if (_debug) stderr.writeln("ImportWithClause: error parsing $adsPath: $ex");
            return null;
        } finally {
            _parsingInProgress.remove(fullPath);
        }
    }

    void ParsePragmas() {
        var stream = tokenStream as BufferedTokenStream;
        stream.fill();
        var allTokens = stream.getTokens() ?? [];
        const int PRAGMA_CHANNEL = 2;
        List<Token>? currentPragma;
        var pragmas = <List<Token>>[];
        for (var token in allTokens) {
            if (token.channel != PRAGMA_CHANNEL) continue;
            if (token.type == AdaLexer.TOKEN_PRAGMA) {
                currentPragma = [token];
            } else if (currentPragma != null) {
                currentPragma.add(token);
                if (token.type == AdaLexer.TOKEN_SEMI) {
                    pragmas.add(currentPragma);
                    currentPragma = null;
                }
            }
        }
        for (var pragmaTokens in pragmas) {
            var defaultChannelTokens = <Token>[];
            for (var t in pragmaTokens) {
                var ct = CommonToken.copy(t);
                ct.channel = Token.DEFAULT_CHANNEL;
                defaultChannelTokens.add(ct);
            }
            var eof = CommonToken(Token.EOF);
            eof.channel = Token.DEFAULT_CHANNEL;
            defaultChannelTokens.add(eof);
            var tokenSource = ListTokenSource(defaultChannelTokens);
            var tokenStream = CommonTokenStream(tokenSource);
            var parser = AdaParser(tokenStream);
            parser.removeErrorListeners();
            for (var listener in errorListeners) {
                parser.addErrorListener(listener);
            }
            parser.pragmaRule();
        }
    }
}
