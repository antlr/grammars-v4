import antlr4 from 'antlr4';
import AdaLexer from './AdaLexer.js';
import AdaParser from './AdaParser.js';
import Symbol from './Symbol.js';
import SymbolTable from './SymbolTable.js';
import TypeClassification from './TypeClassification.js';

const ALL_SEMANTIC_FUNCTIONS = ["IsAggregate", "IsTypeName"];

function parseNoSemantics(args) {
    var result = new Set();
    if (!args) return result;
    for (var a of args) {
        if (a.toLowerCase().startsWith("--no-semantics")) {
            var eqIndex = a.indexOf('=');
            if (eqIndex === -1) {
                for (var func of ALL_SEMANTIC_FUNCTIONS) {
                    result.add(func);
                }
            } else {
                var value = a.substring(eqIndex + 1);
                var funcs = value.split(',');
                for (var f of funcs) {
                    result.add(f.trim());
                }
            }
        }
    }
    return result;
}

class SimpleTokenSource {
    constructor(tokens) {
        this.tokens = tokens;
        this.pos = 0;
    }
    nextToken() {
        if (this.pos >= this.tokens.length) {
            return this.tokens[this.tokens.length - 1];
        }
        return this.tokens[this.pos++];
    }
    getSourceName() { return "pragma"; }
}

export default class AdaParserBase extends antlr4.Parser {
    constructor(input) {
        super(input);
        this._st = new SymbolTable();
        this._expectedTypeStack = [];
        this._debug = false;
        this._outputSymbolTable = false;
        this._outputAppliedOccurrences = false;
        this._noSemantics = new Set();
        this._initOptions();
    }

    _initOptions() {
        var args = (typeof process !== 'undefined' && process.argv) ? process.argv : [];
        this._noSemantics = parseNoSemantics(args);
        this._debug = args.some(a => a.toLowerCase().indexOf("--debug") >= 0);
        this._outputSymbolTable = args.some(a => a.toLowerCase().indexOf("--output-symbol-table") >= 0);
        this._outputAppliedOccurrences = args.some(a => a.toLowerCase().indexOf("--output-applied-occurrences") >= 0);
    }

    IsAggregate() {
        if (this._noSemantics.has("IsAggregate")) return true;
        var stream = this._input;
        var lt1 = stream.LT(1);
        if (lt1.type !== AdaLexer.LP) {
            if (this._debug) process.stderr.write("IsAggregate: LT(1) is not LP, returning false\n");
            return false;
        }
        // Structural scan from LT(2), tracking paren depth
        var depth = 0;
        for (var i = 2; ; i++) {
            var t = stream.LT(i);
            if (t === null || t.type === antlr4.Token.EOF) break;
            if (t.type === AdaLexer.LP) {
                depth++;
            } else if (t.type === AdaLexer.RP) {
                if (depth === 0) {
                    // Reached closing paren without indicators -- fall through to type context
                    break;
                }
                depth--;
            } else if (depth === 0) {
                if (t.type === AdaLexer.COMMA) {
                    if (this._debug) process.stderr.write("IsAggregate: found COMMA at depth 0, returning true\n");
                    return true;
                }
                if (t.type === AdaLexer.ARROW) {
                    if (this._debug) process.stderr.write("IsAggregate: found ARROW at depth 0, returning true\n");
                    return true;
                }
                if (t.type === AdaLexer.WITH) {
                    if (this._debug) process.stderr.write("IsAggregate: found WITH at depth 0, returning true\n");
                    return true;
                }
                if (t.type === AdaLexer.NULL_) {
                    var next = stream.LT(i + 1);
                    if (next !== null && next.type === AdaLexer.RECORD) {
                        if (this._debug) process.stderr.write("IsAggregate: found NULL RECORD at depth 0, returning true\n");
                        return true;
                    }
                }
            }
        }
        // Type-context check
        if (this._expectedTypeStack.length > 0) {
            var expected = this._expectedTypeStack[this._expectedTypeStack.length - 1];
            if (expected !== null && expected.isComposite) {
                if (this._debug) process.stderr.write("IsAggregate: expected type is composite, returning true\n");
                return true;
            }
        }
        if (this._debug) process.stderr.write("IsAggregate: no aggregate indicators, returning false\n");
        return false;
    }

    IsTypeName() {
        if (this._noSemantics.has("IsTypeName")) return true;
        var stream = this._input;
        var lt1 = stream.LT(1);
        if (lt1.type !== AdaLexer.IDENTIFIER_) {
            if (this._debug) process.stderr.write("IsTypeName: LT(1) is not IDENTIFIER_, returning false\n");
            return false;
        }
        // Collect dotted name — but for lookup use just first component
        var firstName = lt1.text;
        var resolved = this._st.resolve(firstName);
        if (resolved !== null && resolved.classification.has(TypeClassification.TypeName_)) {
            if (this._debug) process.stderr.write("IsTypeName: " + firstName + " resolved as TypeName_, returning true\n");
            return true;
        }
        if (this._debug) process.stderr.write("IsTypeName: " + firstName + " not a type name, returning false\n");
        return false;
    }

    EnterDeclaration() {
        if (this._debug) process.stderr.write("EnterDeclaration\n");
        var context = this._ctx;
        for (; context !== null; context = context.parentCtx) {
            if (context instanceof AdaParser.Full_type_declarationContext) {
                var defId = context.defining_identifier();
                if (defId !== null) {
                    var name = defId.getText();
                    var isComposite = false;
                    var typeDef = context.type_definition();
                    if (typeDef !== null) {
                        isComposite = typeDef.record_type_definition() !== null
                                   || typeDef.array_type_definition() !== null;
                    }
                    this._defineSymbol(name, TypeClassification.TypeName_, defId.start, isComposite);
                }
                return;
            }
            if (context instanceof AdaParser.Subtype_declarationContext) {
                var defId = context.defining_identifier();
                if (defId !== null) {
                    var name = defId.getText();
                    var isComposite = false;
                    var si = context.subtype_indication();
                    if (si !== null) {
                        var sm = si.subtype_mark();
                        if (sm !== null) {
                            var baseSym = this._st.resolve(sm.getText());
                            if (baseSym !== null) isComposite = baseSym.isComposite;
                        }
                    }
                    this._defineSymbol(name, TypeClassification.TypeName_, defId.start, isComposite);
                }
                return;
            }
            if (context instanceof AdaParser.Object_declarationContext) {
                var defIdList = context.defining_identifier_list();
                if (defIdList !== null) {
                    var defIds = defIdList.defining_identifier();
                    for (var di of defIds) {
                        this._defineSymbol(di.getText(), TypeClassification.ObjectName_, di.start);
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Number_declarationContext) {
                var defIdList = context.defining_identifier_list();
                if (defIdList !== null) {
                    var defIds = defIdList.defining_identifier();
                    for (var di of defIds) {
                        this._defineSymbol(di.getText(), TypeClassification.ObjectName_, di.start);
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Subprogram_declarationContext) {
                this._defineSubprogramFromSpec(context.subprogram_specification());
                return;
            }
            if (context instanceof AdaParser.Subprogram_bodyContext) {
                this._defineSubprogramFromSpec(context.subprogram_specification());
                return;
            }
            if (context instanceof AdaParser.Package_declarationContext) {
                var pkgSpec = context.package_specification();
                if (pkgSpec !== null) {
                    var dpun = pkgSpec.defining_program_unit_name();
                    if (dpun !== null) {
                        var defId = dpun.defining_identifier();
                        if (defId !== null)
                            this._defineSymbol(defId.getText(), TypeClassification.PackageName_, defId.start);
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Package_bodyContext) {
                var dpun = context.defining_program_unit_name();
                if (dpun !== null) {
                    var defId = dpun.defining_identifier();
                    if (defId !== null)
                        this._defineSymbol(defId.getText(), TypeClassification.PackageName_, defId.start);
                }
                return;
            }
            if (context instanceof AdaParser.Exception_declarationContext) {
                var defIdList = context.defining_identifier_list();
                if (defIdList !== null) {
                    var defIds = defIdList.defining_identifier();
                    for (var di of defIds) {
                        this._defineSymbol(di.getText(), TypeClassification.ExceptionName_, di.start);
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Task_type_declarationContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Single_task_declarationContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Protected_type_declarationContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Single_protected_declarationContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Entry_declarationContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.SubprogramName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Component_declarationContext) {
                var defIdList = context.defining_identifier_list();
                if (defIdList !== null) {
                    var defIds = defIdList.defining_identifier();
                    for (var di of defIds) {
                        this._defineSymbol(di.getText(), TypeClassification.ComponentName_, di.start);
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Incomplete_type_declarationContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Private_type_declarationContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Private_extension_declarationContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start, true);
                return;
            }
            if (context instanceof AdaParser.Generic_instantiationContext) {
                var dpuns = context.defining_program_unit_name();
                if (dpuns !== null && dpuns.length > 0) {
                    var dpun = dpuns[0];
                    var defId = dpun.defining_identifier();
                    if (defId !== null) {
                        var tc = TypeClassification.PackageName_;
                        if (context.PROCEDURE() !== null) tc = TypeClassification.SubprogramName_;
                        else if (context.FUNCTION() !== null) tc = TypeClassification.SubprogramName_;
                        this._defineSymbol(defId.getText(), tc, defId.start);
                    }
                }
                var dds = context.defining_designator();
                if (dds !== null && dds.length > 0) {
                    var dd = dds[0];
                    var dpun = dd.defining_program_unit_name();
                    if (dpun !== null) {
                        var defId = dpun.defining_identifier();
                        if (defId !== null)
                            this._defineSymbol(defId.getText(), TypeClassification.SubprogramName_, defId.start);
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Object_renaming_declarationContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Exception_renaming_declarationContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ExceptionName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Package_renaming_declarationContext) {
                var dpun = context.defining_program_unit_name();
                if (dpun !== null) {
                    var defId = dpun.defining_identifier();
                    if (defId !== null)
                        this._defineSymbol(defId.getText(), TypeClassification.PackageName_, defId.start);
                }
                return;
            }
            if (context instanceof AdaParser.Formal_complete_type_declarationContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Formal_incomplete_type_declarationContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Formal_object_declarationContext) {
                var defIdList = context.defining_identifier_list();
                if (defIdList !== null) {
                    var defIds = defIdList.defining_identifier();
                    for (var di of defIds) {
                        this._defineSymbol(di.getText(), TypeClassification.ObjectName_, di.start);
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Formal_package_declarationContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.PackageName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Parameter_specificationContext) {
                var defIdList = context.defining_identifier_list();
                if (defIdList !== null) {
                    var defIds = defIdList.defining_identifier();
                    for (var di of defIds) {
                        this._defineSymbol(di.getText(), TypeClassification.ObjectName_, di.start);
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Loop_parameter_specificationContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Iterator_specificationContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Enumeration_literal_specificationContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.EnumerationLiteral_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Choice_parameter_specificationContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Entry_index_specificationContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Extended_return_object_declarationContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Entry_bodyContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.SubprogramName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Task_bodyContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Protected_bodyContext) {
                var defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Discriminant_specificationContext) {
                var defIdList = context.defining_identifier_list();
                if (defIdList !== null) {
                    var defIds = defIdList.defining_identifier();
                    for (var di of defIds) {
                        this._defineSymbol(di.getText(), TypeClassification.ObjectName_, di.start);
                    }
                }
                return;
            }
        }
    }

    _defineSubprogramFromSpec(spec) {
        if (spec === null) return;
        var procSpec = spec.procedure_specification();
        if (procSpec !== null) {
            var dpun = procSpec.defining_program_unit_name();
            if (dpun !== null) {
                var defId = dpun.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.SubprogramName_, defId.start);
            }
            return;
        }
        var funcSpec = spec.function_specification();
        if (funcSpec !== null) {
            var dd = funcSpec.defining_designator();
            if (dd !== null) {
                var dpun = dd.defining_program_unit_name();
                if (dpun !== null) {
                    var defId = dpun.defining_identifier();
                    if (defId !== null)
                        this._defineSymbol(defId.getText(), TypeClassification.SubprogramName_, defId.start);
                }
            }
        }
    }

    _defineSymbol(name, classification, token, isComposite) {
        var s = new Symbol();
        s.name = name;
        s.classification = new Set([classification]);
        s.isComposite = isComposite || false;
        s.definedFile = (token && token.source && token.source[0]) ? token.source[0].sourceName || "" : "";
        s.definedLine = token ? token.line || 0 : 0;
        s.definedColumn = token ? token.column || 0 : 0;
        this._st.define(s);
        if (this._debug) process.stderr.write("Defined symbol: " + s.toString() + "\n");
    }

    EnterScope() {
        if (this._debug) process.stderr.write("EnterScope\n");
        this._st.pushBlockScope();
    }

    ExitScope() {
        if (this._debug) process.stderr.write("ExitScope\n");
        this._st.popBlockScope();
    }

    PushExpectedType() {
        this._expectedTypeStack.push(null);
    }

    PopExpectedType() {
        if (this._expectedTypeStack.length > 0) {
            this._expectedTypeStack.pop();
            if (this._debug) process.stderr.write("PopExpectedType\n");
        }
    }

    OutputSymbolTable() {
        if (this._outputSymbolTable) {
            process.stderr.write(this._st.toString());
        }
    }

    ParsePragmas() {
        var stream = this._input;
        stream.fill();
        var allTokens = stream.tokens;
        var PRAGMA_CHANNEL = 2;
        var currentPragma = null;
        var pragmas = [];
        for (var i = 0; i < allTokens.length; i++) {
            var token = allTokens[i];
            if (token.channel !== PRAGMA_CHANNEL) continue;
            if (token.type === AdaLexer.PRAGMA) {
                currentPragma = [token];
            } else if (currentPragma !== null) {
                currentPragma.push(token);
                if (token.type === AdaLexer.SEMI) {
                    pragmas.push(currentPragma);
                    currentPragma = null;
                }
            }
        }
        for (var j = 0; j < pragmas.length; j++) {
            var pragmaTokens = pragmas[j];
            var defaultChannelTokens = [];
            for (var k = 0; k < pragmaTokens.length; k++) {
                var t = pragmaTokens[k];
                var ct = new antlr4.CommonToken(t.source, t.type, antlr4.Token.DEFAULT_CHANNEL, t.start, t.stop);
                ct.text = t.text;
                ct.line = t.line;
                ct.column = t.column;
                ct.tokenIndex = t.tokenIndex;
                defaultChannelTokens.push(ct);
            }
            var eof = new antlr4.CommonToken([null, null], antlr4.Token.EOF, antlr4.Token.DEFAULT_CHANNEL, -1, -1);
            defaultChannelTokens.push(eof);
            var tokenSource = new SimpleTokenSource(defaultChannelTokens);
            var tokenStream = new antlr4.CommonTokenStream(tokenSource);
            var parser = new AdaParser(tokenStream);
            parser.removeErrorListeners();
            var listeners = this._listeners;
            if (listeners) {
                for (var l = 0; l < listeners.length; l++) {
                    parser.addErrorListener(listeners[l]);
                }
            }
            parser.pragmaRule();
        }
    }
}
