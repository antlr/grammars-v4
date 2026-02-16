import { Parser, TokenStream, Token, CommonToken, CommonTokenStream, ParserRuleContext } from "antlr4";
import AdaLexer from './AdaLexer';
import AdaParser from './AdaParser';
import { SymbolTable } from './SymbolTable';
import { Symbol } from './Symbol';
import { TypeClassification } from './TypeClassification';

const ALL_SEMANTIC_FUNCTIONS = ["IsAggregate", "IsTypeName"];

function parseNoSemantics(args: string[]): Set<string> {
    const result = new Set<string>();
    for (const a of args) {
        if (a.toLowerCase().startsWith("--no-semantics")) {
            const eqIndex = a.indexOf('=');
            if (eqIndex === -1) {
                for (const func of ALL_SEMANTIC_FUNCTIONS) {
                    result.add(func);
                }
            } else {
                const value = a.substring(eqIndex + 1);
                for (const f of value.split(',')) {
                    result.add(f.trim());
                }
            }
        }
    }
    return result;
}

class SimpleTokenSource {
    private tokens: Token[];
    private pos: number;
    constructor(tokens: Token[]) {
        this.tokens = tokens;
        this.pos = 0;
    }
    nextToken(): Token {
        if (this.pos >= this.tokens.length) {
            return this.tokens[this.tokens.length - 1];
        }
        return this.tokens[this.pos++];
    }
    getSourceName(): string { return "pragma"; }
}

export default abstract class AdaParserBase extends Parser {
    private _st: SymbolTable;
    private _expectedTypeStack: (Symbol | null)[] = [];
    private _debug: boolean = false;
    private _outputSymbolTable: boolean = false;
    private _outputAppliedOccurrences: boolean = false;
    private _noSemantics: Set<string> = new Set();

    constructor(input: TokenStream) {
        super(input);
        this._st = new SymbolTable();
        this._initOptions();
    }

    private _initOptions(): void {
        const args = (typeof process !== 'undefined' && process.argv) ? process.argv : [];
        this._noSemantics = parseNoSemantics(args);
        this._debug = args.some(a => a.toLowerCase().indexOf("--debug") >= 0);
        this._outputSymbolTable = args.some(a => a.toLowerCase().indexOf("--output-symbol-table") >= 0);
        this._outputAppliedOccurrences = args.some(a => a.toLowerCase().indexOf("--output-applied-occurrences") >= 0);
    }

    IsAggregate(): boolean {
        if (this._noSemantics.has("IsAggregate")) return true;
        const stream = this._input as CommonTokenStream;
        const lt1 = stream.LT(1);
        if (lt1 === null || lt1.type !== AdaLexer.LP) {
            if (this._debug) process.stderr.write("IsAggregate: LT(1) is not LP, returning false\n");
            return false;
        }
        // Structural scan from LT(2), tracking paren depth
        let depth = 0;
        for (let i = 2; ; i++) {
            const t = stream.LT(i);
            if (t === null || t.type === Token.EOF) break;
            if (t.type === AdaLexer.LP) {
                depth++;
            } else if (t.type === AdaLexer.RP) {
                if (depth === 0) break;
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
                    const next = stream.LT(i + 1);
                    if (next !== null && next.type === AdaLexer.RECORD) {
                        if (this._debug) process.stderr.write("IsAggregate: found NULL RECORD at depth 0, returning true\n");
                        return true;
                    }
                }
            }
        }
        // Type-context check
        if (this._expectedTypeStack.length > 0) {
            const expected = this._expectedTypeStack[this._expectedTypeStack.length - 1];
            if (expected !== null && expected.isComposite) {
                if (this._debug) process.stderr.write("IsAggregate: expected type is composite, returning true\n");
                return true;
            }
        }
        if (this._debug) process.stderr.write("IsAggregate: no aggregate indicators, returning false\n");
        return false;
    }

    IsTypeName(): boolean {
        if (this._noSemantics.has("IsTypeName")) return true;
        const stream = this._input as CommonTokenStream;
        const lt1 = stream.LT(1);
        if (lt1 === null || lt1.type !== AdaLexer.IDENTIFIER_) {
            if (this._debug) process.stderr.write("IsTypeName: LT(1) is not IDENTIFIER_, returning false\n");
            return false;
        }
        const firstName = lt1.text ?? "";
        const resolved = this._st.resolve(firstName);
        if (resolved !== null && resolved.classification.has(TypeClassification.TypeName_)) {
            if (this._debug) process.stderr.write("IsTypeName: " + firstName + " resolved as TypeName_, returning true\n");
            return true;
        }
        if (this._debug) process.stderr.write("IsTypeName: " + firstName + " not a type name, returning false\n");
        return false;
    }

    EnterDeclaration(): void {
        if (this._debug) process.stderr.write("EnterDeclaration\n");
        let context: any = this._ctx;
        for (; context !== null; context = context.parentCtx) {
            if (context instanceof AdaParser.Full_type_declarationContext) {
                const defId = context.defining_identifier();
                if (defId !== null) {
                    const name = defId.getText();
                    let isComposite = false;
                    const typeDef = context.type_definition();
                    if (typeDef !== null) {
                        isComposite = typeDef.record_type_definition() !== null
                                   || typeDef.array_type_definition() !== null;
                    }
                    this._defineSymbol(name, TypeClassification.TypeName_, defId.start, isComposite);
                }
                return;
            }
            if (context instanceof AdaParser.Subtype_declarationContext) {
                const defId = context.defining_identifier();
                if (defId !== null) {
                    const name = defId.getText();
                    let isComposite = false;
                    const si = context.subtype_indication();
                    if (si !== null) {
                        const sm = si.subtype_mark();
                        if (sm !== null) {
                            const baseSym = this._st.resolve(sm.getText());
                            if (baseSym !== null) isComposite = baseSym.isComposite;
                        }
                    }
                    this._defineSymbol(name, TypeClassification.TypeName_, defId.start, isComposite);
                }
                return;
            }
            if (context instanceof AdaParser.Object_declarationContext) {
                const defIdList = context.defining_identifier_list();
                if (defIdList !== null) {
                    const defIds = defIdList.defining_identifier();
                    for (const di of defIds) {
                        this._defineSymbol(di.getText(), TypeClassification.ObjectName_, di.start);
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Number_declarationContext) {
                const defIdList = context.defining_identifier_list();
                if (defIdList !== null) {
                    const defIds = defIdList.defining_identifier();
                    for (const di of defIds) {
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
                const pkgSpec = context.package_specification();
                if (pkgSpec !== null) {
                    const dpun = pkgSpec.defining_program_unit_name();
                    if (dpun !== null) {
                        const defId = dpun.defining_identifier();
                        if (defId !== null)
                            this._defineSymbol(defId.getText(), TypeClassification.PackageName_, defId.start);
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Package_bodyContext) {
                const dpun = context.defining_program_unit_name();
                if (dpun !== null) {
                    const defId = dpun.defining_identifier();
                    if (defId !== null)
                        this._defineSymbol(defId.getText(), TypeClassification.PackageName_, defId.start);
                }
                return;
            }
            if (context instanceof AdaParser.Exception_declarationContext) {
                const defIdList = context.defining_identifier_list();
                if (defIdList !== null) {
                    const defIds = defIdList.defining_identifier();
                    for (const di of defIds) {
                        this._defineSymbol(di.getText(), TypeClassification.ExceptionName_, di.start);
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Task_type_declarationContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Single_task_declarationContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Protected_type_declarationContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Single_protected_declarationContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Entry_declarationContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.SubprogramName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Component_declarationContext) {
                const defIdList = context.defining_identifier_list();
                if (defIdList !== null) {
                    const defIds = defIdList.defining_identifier();
                    for (const di of defIds) {
                        this._defineSymbol(di.getText(), TypeClassification.ComponentName_, di.start);
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Incomplete_type_declarationContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Private_type_declarationContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Private_extension_declarationContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start, true);
                return;
            }
            if (context instanceof AdaParser.Generic_instantiationContext) {
                const dpuns = context.defining_program_unit_name();
                if (dpuns !== null && dpuns.length > 0) {
                    const dpun = dpuns[0];
                    const defId = dpun.defining_identifier();
                    if (defId !== null) {
                        let tc = TypeClassification.PackageName_;
                        if (context.PROCEDURE() !== null) tc = TypeClassification.SubprogramName_;
                        else if (context.FUNCTION() !== null) tc = TypeClassification.SubprogramName_;
                        this._defineSymbol(defId.getText(), tc, defId.start);
                    }
                }
                const dds = context.defining_designator();
                if (dds !== null && dds.length > 0) {
                    const dd = dds[0];
                    const dpun = dd.defining_program_unit_name();
                    if (dpun !== null) {
                        const defId = dpun.defining_identifier();
                        if (defId !== null)
                            this._defineSymbol(defId.getText(), TypeClassification.SubprogramName_, defId.start);
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Object_renaming_declarationContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Exception_renaming_declarationContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ExceptionName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Package_renaming_declarationContext) {
                const dpun = context.defining_program_unit_name();
                if (dpun !== null) {
                    const defId = dpun.defining_identifier();
                    if (defId !== null)
                        this._defineSymbol(defId.getText(), TypeClassification.PackageName_, defId.start);
                }
                return;
            }
            if (context instanceof AdaParser.Formal_complete_type_declarationContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Formal_incomplete_type_declarationContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Formal_object_declarationContext) {
                const defIdList = context.defining_identifier_list();
                if (defIdList !== null) {
                    const defIds = defIdList.defining_identifier();
                    for (const di of defIds) {
                        this._defineSymbol(di.getText(), TypeClassification.ObjectName_, di.start);
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Formal_package_declarationContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.PackageName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Parameter_specificationContext) {
                const defIdList = context.defining_identifier_list();
                if (defIdList !== null) {
                    const defIds = defIdList.defining_identifier();
                    for (const di of defIds) {
                        this._defineSymbol(di.getText(), TypeClassification.ObjectName_, di.start);
                    }
                }
                return;
            }
            if (context instanceof AdaParser.Loop_parameter_specificationContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Iterator_specificationContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Enumeration_literal_specificationContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.EnumerationLiteral_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Choice_parameter_specificationContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Entry_index_specificationContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Extended_return_object_declarationContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Entry_bodyContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.SubprogramName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Task_bodyContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Protected_bodyContext) {
                const defId = context.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (context instanceof AdaParser.Discriminant_specificationContext) {
                const defIdList = context.defining_identifier_list();
                if (defIdList !== null) {
                    const defIds = defIdList.defining_identifier();
                    for (const di of defIds) {
                        this._defineSymbol(di.getText(), TypeClassification.ObjectName_, di.start);
                    }
                }
                return;
            }
        }
    }

    private _defineSubprogramFromSpec(spec: any): void {
        if (spec === null) return;
        const procSpec = spec.procedure_specification();
        if (procSpec !== null) {
            const dpun = procSpec.defining_program_unit_name();
            if (dpun !== null) {
                const defId = dpun.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.SubprogramName_, defId.start);
            }
            return;
        }
        const funcSpec = spec.function_specification();
        if (funcSpec !== null) {
            const dd = funcSpec.defining_designator();
            if (dd !== null) {
                const dpun = dd.defining_program_unit_name();
                if (dpun !== null) {
                    const defId = dpun.defining_identifier();
                    if (defId !== null)
                        this._defineSymbol(defId.getText(), TypeClassification.SubprogramName_, defId.start);
                }
            }
        }
    }

    private _defineSymbol(name: string, classification: TypeClassification, token: any, isComposite: boolean = false): void {
        const s = new Symbol(name, new Set([classification]));
        s.isComposite = isComposite;
        s.definedFile = (token && token.source && token.source[0]) ? token.source[0].sourceName || "" : "";
        s.definedLine = token ? token.line || 0 : 0;
        s.definedColumn = token ? token.column || 0 : 0;
        this._st.define(s);
        if (this._debug) process.stderr.write("Defined symbol: " + s.toString() + "\n");
    }

    EnterScope(): void {
        if (this._debug) process.stderr.write("EnterScope\n");
        this._st.pushBlockScope();
    }

    ExitScope(): void {
        if (this._debug) process.stderr.write("ExitScope\n");
        this._st.popBlockScope();
    }

    PushExpectedType(): void {
        let context: any = this._ctx;
        for (; context !== null; context = context.parentCtx) {
            if (context instanceof AdaParser.Object_declarationContext) {
                const si = context.subtype_indication();
                if (si !== null) {
                    const sm = si.subtype_mark();
                    if (sm !== null) {
                        const resolved = this._st.resolve(sm.getText());
                        this._expectedTypeStack.push(resolved);
                        if (this._debug) process.stderr.write("PushExpectedType: " + (resolved?.name ?? "null") + " from object_declaration\n");
                        return;
                    }
                }
                this._expectedTypeStack.push(null);
                return;
            }
            if (context instanceof AdaParser.Assignment_statementContext) {
                const n = context.name();
                if (n !== null) {
                    const nameText = n.getText();
                    const resolved = this._st.resolve(nameText);
                    this._expectedTypeStack.push(resolved);
                    if (this._debug) process.stderr.write("PushExpectedType: " + (resolved?.name ?? "null") + " from assignment_statement\n");
                    return;
                }
                this._expectedTypeStack.push(null);
                return;
            }
            if (context instanceof AdaParser.Component_declarationContext) {
                const compDef = context.component_definition();
                if (compDef !== null) {
                    const si = compDef.subtype_indication();
                    if (si !== null) {
                        const sm = si.subtype_mark();
                        if (sm !== null) {
                            const resolved = this._st.resolve(sm.getText());
                            this._expectedTypeStack.push(resolved);
                            if (this._debug) process.stderr.write("PushExpectedType: " + (resolved?.name ?? "null") + " from component_declaration\n");
                            return;
                        }
                    }
                }
                this._expectedTypeStack.push(null);
                return;
            }
            if (context instanceof AdaParser.Number_declarationContext) {
                this._expectedTypeStack.push(null);
                if (this._debug) process.stderr.write("PushExpectedType: null (scalar) from number_declaration\n");
                return;
            }
            if (context instanceof AdaParser.Parameter_specificationContext) {
                const sm = context.subtype_mark();
                if (sm !== null) {
                    const resolved = this._st.resolve(sm.getText());
                    this._expectedTypeStack.push(resolved);
                    if (this._debug) process.stderr.write("PushExpectedType: " + (resolved?.name ?? "null") + " from parameter_specification\n");
                    return;
                }
                this._expectedTypeStack.push(null);
                return;
            }
            if (context instanceof AdaParser.Simple_return_statementContext) {
                this._expectedTypeStack.push(null);
                if (this._debug) process.stderr.write("PushExpectedType: null from simple_return_statement\n");
                return;
            }
        }
        this._expectedTypeStack.push(null);
    }

    PopExpectedType(): void {
        if (this._expectedTypeStack.length > 0) {
            this._expectedTypeStack.pop();
            if (this._debug) process.stderr.write("PopExpectedType\n");
        }
    }

    OutputSymbolTable(): void {
        if (this._outputSymbolTable) {
            process.stderr.write(this._st.toString());
        }
    }

    ParsePragmas(): void {
        const stream = this._input as CommonTokenStream;
        stream.fill();
        const allTokens = stream.tokens;
        const PRAGMA_CHANNEL = 2;
        let currentPragma: Token[] | null = null;
        const pragmas: Token[][] = [];
        for (const token of allTokens) {
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
        for (const pragmaTokens of pragmas) {
            const defaultChannelTokens: Token[] = [];
            for (const t of pragmaTokens) {
                const ct = new CommonToken((t as any).source, t.type, Token.DEFAULT_CHANNEL, t.start, t.stop);
                ct.text = t.text;
                ct.line = t.line;
                ct.column = t.column;
                ct.tokenIndex = t.tokenIndex;
                defaultChannelTokens.push(ct);
            }
            const eof = new CommonToken([null, null] as any, Token.EOF, Token.DEFAULT_CHANNEL, -1, -1);
            defaultChannelTokens.push(eof);
            const tokenSource = new SimpleTokenSource(defaultChannelTokens);
            const tokenStream = new CommonTokenStream(tokenSource as any);
            const parser = new AdaParser(tokenStream);
            parser.removeErrorListeners();
            parser.pragmaRule();
        }
    }
}
