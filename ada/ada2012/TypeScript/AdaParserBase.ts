import * as fs from 'fs';
import * as path from 'path';
import { Parser, TokenStream, Token, CommonToken, CommonTokenStream, CharStream, ParserRuleContext } from "antlr4";
import AdaLexer from './AdaLexer';
import AdaParser, {
    Full_type_declarationContext,
    Subtype_declarationContext,
    Object_declarationContext,
    Number_declarationContext,
    Subprogram_declarationContext,
    Subprogram_bodyContext,
    Package_declarationContext,
    Package_bodyContext,
    Exception_declarationContext,
    Task_type_declarationContext,
    Single_task_declarationContext,
    Protected_type_declarationContext,
    Single_protected_declarationContext,
    Entry_declarationContext,
    Component_declarationContext,
    Incomplete_type_declarationContext,
    Private_type_declarationContext,
    Private_extension_declarationContext,
    Generic_instantiationContext,
    Object_renaming_declarationContext,
    Exception_renaming_declarationContext,
    Package_renaming_declarationContext,
    Formal_complete_type_declarationContext,
    Formal_incomplete_type_declarationContext,
    Formal_object_declarationContext,
    Formal_package_declarationContext,
    Parameter_specificationContext,
    Loop_parameter_specificationContext,
    Iterator_specificationContext,
    Enumeration_literal_specificationContext,
    Choice_parameter_specificationContext,
    Entry_index_specificationContext,
    Extended_return_object_declarationContext,
    Entry_bodyContext,
    Task_bodyContext,
    Protected_bodyContext,
    Discriminant_specificationContext,
    Assignment_statementContext,
    Simple_return_statementContext,
    Nonlimited_with_clauseContext,
    Limited_with_clauseContext,
} from './AdaParser';
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

// Type-narrowing helpers using ruleIndex, avoiding instanceof issues with tsx module resolution.
function isFull_type_declarationContext(x: any): x is Full_type_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_full_type_declaration;
}
function isSubtype_declarationContext(x: any): x is Subtype_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_subtype_declaration;
}
function isObject_declarationContext(x: any): x is Object_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_object_declaration;
}
function isNumber_declarationContext(x: any): x is Number_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_number_declaration;
}
function isSubprogram_declarationContext(x: any): x is Subprogram_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_subprogram_declaration;
}
function isSubprogram_bodyContext(x: any): x is Subprogram_bodyContext {
    return x?.ruleIndex === AdaParser.RULE_subprogram_body;
}
function isPackage_declarationContext(x: any): x is Package_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_package_declaration;
}
function isPackage_bodyContext(x: any): x is Package_bodyContext {
    return x?.ruleIndex === AdaParser.RULE_package_body;
}
function isException_declarationContext(x: any): x is Exception_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_exception_declaration;
}
function isTask_type_declarationContext(x: any): x is Task_type_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_task_type_declaration;
}
function isSingle_task_declarationContext(x: any): x is Single_task_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_single_task_declaration;
}
function isProtected_type_declarationContext(x: any): x is Protected_type_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_protected_type_declaration;
}
function isSingle_protected_declarationContext(x: any): x is Single_protected_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_single_protected_declaration;
}
function isEntry_declarationContext(x: any): x is Entry_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_entry_declaration;
}
function isComponent_declarationContext(x: any): x is Component_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_component_declaration;
}
function isIncomplete_type_declarationContext(x: any): x is Incomplete_type_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_incomplete_type_declaration;
}
function isPrivate_type_declarationContext(x: any): x is Private_type_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_private_type_declaration;
}
function isPrivate_extension_declarationContext(x: any): x is Private_extension_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_private_extension_declaration;
}
function isGeneric_instantiationContext(x: any): x is Generic_instantiationContext {
    return x?.ruleIndex === AdaParser.RULE_generic_instantiation;
}
function isObject_renaming_declarationContext(x: any): x is Object_renaming_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_object_renaming_declaration;
}
function isException_renaming_declarationContext(x: any): x is Exception_renaming_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_exception_renaming_declaration;
}
function isPackage_renaming_declarationContext(x: any): x is Package_renaming_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_package_renaming_declaration;
}
function isFormal_complete_type_declarationContext(x: any): x is Formal_complete_type_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_formal_complete_type_declaration;
}
function isFormal_incomplete_type_declarationContext(x: any): x is Formal_incomplete_type_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_formal_incomplete_type_declaration;
}
function isFormal_object_declarationContext(x: any): x is Formal_object_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_formal_object_declaration;
}
function isFormal_package_declarationContext(x: any): x is Formal_package_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_formal_package_declaration;
}
function isParameter_specificationContext(x: any): x is Parameter_specificationContext {
    return x?.ruleIndex === AdaParser.RULE_parameter_specification;
}
function isLoop_parameter_specificationContext(x: any): x is Loop_parameter_specificationContext {
    return x?.ruleIndex === AdaParser.RULE_loop_parameter_specification;
}
function isIterator_specificationContext(x: any): x is Iterator_specificationContext {
    return x?.ruleIndex === AdaParser.RULE_iterator_specification;
}
function isEnumeration_literal_specificationContext(x: any): x is Enumeration_literal_specificationContext {
    return x?.ruleIndex === AdaParser.RULE_enumeration_literal_specification;
}
function isChoice_parameter_specificationContext(x: any): x is Choice_parameter_specificationContext {
    return x?.ruleIndex === AdaParser.RULE_choice_parameter_specification;
}
function isEntry_index_specificationContext(x: any): x is Entry_index_specificationContext {
    return x?.ruleIndex === AdaParser.RULE_entry_index_specification;
}
function isExtended_return_object_declarationContext(x: any): x is Extended_return_object_declarationContext {
    return x?.ruleIndex === AdaParser.RULE_extended_return_object_declaration;
}
function isEntry_bodyContext(x: any): x is Entry_bodyContext {
    return x?.ruleIndex === AdaParser.RULE_entry_body;
}
function isTask_bodyContext(x: any): x is Task_bodyContext {
    return x?.ruleIndex === AdaParser.RULE_task_body;
}
function isProtected_bodyContext(x: any): x is Protected_bodyContext {
    return x?.ruleIndex === AdaParser.RULE_protected_body;
}
function isDiscriminant_specificationContext(x: any): x is Discriminant_specificationContext {
    return x?.ruleIndex === AdaParser.RULE_discriminant_specification;
}
function isAssignment_statementContext(x: any): x is Assignment_statementContext {
    return x?.ruleIndex === AdaParser.RULE_assignment_statement;
}
function isSimple_return_statementContext(x: any): x is Simple_return_statementContext {
    return x?.ruleIndex === AdaParser.RULE_simple_return_statement;
}
function isNonlimited_with_clauseContext(x: any): x is Nonlimited_with_clauseContext {
    return x?.ruleIndex === AdaParser.RULE_nonlimited_with_clause;
}
function isLimited_with_clauseContext(x: any): x is Limited_with_clauseContext {
    return x?.ruleIndex === AdaParser.RULE_limited_with_clause;
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
    private _searchPaths: string[] = [];
    private static _packageCache: Map<string, Symbol[]> = new Map();
    private static _parsingInProgress: Set<string> = new Set();
    public _currentFile: string = "";

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
        for (const arg of args) {
            if (arg.toLowerCase().startsWith("--i") && arg.length > 3) {
                this._searchPaths.push(arg.substring(3));
            }
        }
    }

    IsAggregate(): boolean {
        if (this._noSemantics.has("IsAggregate")) return true;
        const stream = this._input as CommonTokenStream;
        const lt1 = stream.LT(1);
        if (lt1 === null || lt1.type !== AdaLexer.LP) {
            if (this._debug) process.stderr.write("IsAggregate: LT(1) is not LP, returning false\n");
            return false;
        }
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
        let ctx: any = this._ctx;
        for (; ctx !== null; ctx = ctx.parentCtx) {
            if (isFull_type_declarationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null) {
                    const name = defId.getText();
                    let isComposite = false;
                    const typeDef = ctx.type_definition();
                    if (typeDef !== null) {
                        isComposite = typeDef.record_type_definition() !== null
                                   || typeDef.array_type_definition() !== null;
                    }
                    this._defineSymbol(name, TypeClassification.TypeName_, defId.start, isComposite);
                }
                return;
            }
            if (isSubtype_declarationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null) {
                    const name = defId.getText();
                    let isComposite = false;
                    const si = ctx.subtype_indication();
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
            if (isObject_declarationContext(ctx)) {
                const defIdList = ctx.defining_identifier_list();
                if (defIdList !== null) {
                    const defIds = defIdList.defining_identifier_list();
                    for (const di of defIds) {
                        this._defineSymbol(di.getText(), TypeClassification.ObjectName_, di.start);
                    }
                }
                return;
            }
            if (isNumber_declarationContext(ctx)) {
                const defIdList = ctx.defining_identifier_list();
                if (defIdList !== null) {
                    const defIds = defIdList.defining_identifier_list();
                    for (const di of defIds) {
                        this._defineSymbol(di.getText(), TypeClassification.ObjectName_, di.start);
                    }
                }
                return;
            }
            if (isSubprogram_declarationContext(ctx)) {
                this._defineSubprogramFromSpec(ctx.subprogram_specification());
                return;
            }
            if (isSubprogram_bodyContext(ctx)) {
                this._defineSubprogramFromSpec(ctx.subprogram_specification());
                return;
            }
            if (isPackage_declarationContext(ctx)) {
                const pkgSpec = ctx.package_specification();
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
            if (isPackage_bodyContext(ctx)) {
                const dpun = ctx.defining_program_unit_name();
                if (dpun !== null) {
                    const defId = dpun.defining_identifier();
                    if (defId !== null)
                        this._defineSymbol(defId.getText(), TypeClassification.PackageName_, defId.start);
                }
                return;
            }
            if (isException_declarationContext(ctx)) {
                const defIdList = ctx.defining_identifier_list();
                if (defIdList !== null) {
                    const defIds = defIdList.defining_identifier_list();
                    for (const di of defIds) {
                        this._defineSymbol(di.getText(), TypeClassification.ExceptionName_, di.start);
                    }
                }
                return;
            }
            if (isTask_type_declarationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start);
                return;
            }
            if (isSingle_task_declarationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (isProtected_type_declarationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start);
                return;
            }
            if (isSingle_protected_declarationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (isEntry_declarationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.SubprogramName_, defId.start);
                return;
            }
            if (isComponent_declarationContext(ctx)) {
                const defIdList = ctx.defining_identifier_list();
                if (defIdList !== null) {
                    const defIds = defIdList.defining_identifier_list();
                    for (const di of defIds) {
                        this._defineSymbol(di.getText(), TypeClassification.ComponentName_, di.start);
                    }
                }
                return;
            }
            if (isIncomplete_type_declarationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start);
                return;
            }
            if (isPrivate_type_declarationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start);
                return;
            }
            if (isPrivate_extension_declarationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start, true);
                return;
            }
            if (isGeneric_instantiationContext(ctx)) {
                const dpun = ctx.defining_program_unit_name();
                if (dpun !== null) {
                    const defId = dpun.defining_identifier();
                    if (defId !== null) {
                        let tc = TypeClassification.PackageName_;
                        if (ctx.PROCEDURE() !== null) tc = TypeClassification.SubprogramName_;
                        else if (ctx.FUNCTION() !== null) tc = TypeClassification.SubprogramName_;
                        this._defineSymbol(defId.getText(), tc, defId.start);
                    }
                }
                const dd = ctx.defining_designator();
                if (dd !== null) {
                    const dpun2 = dd.defining_program_unit_name();
                    if (dpun2 !== null) {
                        const defId = dpun2.defining_identifier();
                        if (defId !== null)
                            this._defineSymbol(defId.getText(), TypeClassification.SubprogramName_, defId.start);
                    }
                }
                return;
            }
            if (isObject_renaming_declarationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (isException_renaming_declarationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ExceptionName_, defId.start);
                return;
            }
            if (isPackage_renaming_declarationContext(ctx)) {
                const dpun = ctx.defining_program_unit_name();
                if (dpun !== null) {
                    const defId = dpun.defining_identifier();
                    if (defId !== null)
                        this._defineSymbol(defId.getText(), TypeClassification.PackageName_, defId.start);
                }
                return;
            }
            if (isFormal_complete_type_declarationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start);
                return;
            }
            if (isFormal_incomplete_type_declarationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.TypeName_, defId.start);
                return;
            }
            if (isFormal_object_declarationContext(ctx)) {
                const defIdList = ctx.defining_identifier_list();
                if (defIdList !== null) {
                    const defIds = defIdList.defining_identifier_list();
                    for (const di of defIds) {
                        this._defineSymbol(di.getText(), TypeClassification.ObjectName_, di.start);
                    }
                }
                return;
            }
            if (isFormal_package_declarationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.PackageName_, defId.start);
                return;
            }
            if (isParameter_specificationContext(ctx)) {
                const defIdList = ctx.defining_identifier_list();
                if (defIdList !== null) {
                    const defIds = defIdList.defining_identifier_list();
                    for (const di of defIds) {
                        this._defineSymbol(di.getText(), TypeClassification.ObjectName_, di.start);
                    }
                }
                return;
            }
            if (isLoop_parameter_specificationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (isIterator_specificationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (isEnumeration_literal_specificationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.EnumerationLiteral_, defId.start);
                return;
            }
            if (isChoice_parameter_specificationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (isEntry_index_specificationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (isExtended_return_object_declarationContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (isEntry_bodyContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.SubprogramName_, defId.start);
                return;
            }
            if (isTask_bodyContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (isProtected_bodyContext(ctx)) {
                const defId = ctx.defining_identifier();
                if (defId !== null)
                    this._defineSymbol(defId.getText(), TypeClassification.ObjectName_, defId.start);
                return;
            }
            if (isDiscriminant_specificationContext(ctx)) {
                const defIdList = ctx.defining_identifier_list();
                if (defIdList !== null) {
                    const defIds = defIdList.defining_identifier_list();
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
        let ctx: any = this._ctx;
        for (; ctx !== null; ctx = ctx.parentCtx) {
            if (isObject_declarationContext(ctx)) {
                const si = ctx.subtype_indication();
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
            if (isAssignment_statementContext(ctx)) {
                const n = ctx.name();
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
            if (isComponent_declarationContext(ctx)) {
                const compDef = ctx.component_definition();
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
            if (isNumber_declarationContext(ctx)) {
                this._expectedTypeStack.push(null);
                if (this._debug) process.stderr.write("PushExpectedType: null (scalar) from number_declaration\n");
                return;
            }
            if (isParameter_specificationContext(ctx)) {
                const sm = ctx.subtype_mark();
                if (sm !== null) {
                    const resolved = this._st.resolve(sm.getText());
                    this._expectedTypeStack.push(resolved);
                    if (this._debug) process.stderr.write("PushExpectedType: " + (resolved?.name ?? "null") + " from parameter_specification\n");
                    return;
                }
                this._expectedTypeStack.push(null);
                return;
            }
            if (isSimple_return_statementContext(ctx)) {
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

    ImportWithClause(): void {
        if (this._noSemantics.has("IsTypeName") && this._noSemantics.has("IsAggregate")) return;

        // Auto-detect current file from token stream
        if (!this._currentFile) {
            const stream = this._input as CommonTokenStream;
            const sourceName = (stream as any)?.tokenSource?.inputStream?.name;
            if (sourceName && sourceName !== "unknown" && fs.existsSync(sourceName)) {
                this._currentFile = path.resolve(sourceName);
            }
        }

        const context = this._ctx;
        let names: any[] | null = null;
        if (isNonlimited_with_clauseContext(context)) {
            names = context.name_list();
        } else if (isLimited_with_clauseContext(context)) {
            names = context.name_list();
        }
        if (!names || names.length === 0) return;

        for (const nameCtx of names) {
            const packageName = nameCtx.getText();
            if (this._debug) process.stderr.write(`ImportWithClause: processing 'with ${packageName}'\n`);

            const fileName = this._packageNameToFileName(packageName);
            const cacheKey = packageName.toLowerCase();

            if (AdaParserBase._packageCache.has(cacheKey)) {
                if (this._debug) process.stderr.write(`ImportWithClause: using cached symbols for ${packageName}\n`);
                for (const sym of AdaParserBase._packageCache.get(cacheKey)!) {
                    const copy = new Symbol(sym.name, new Set(sym.classification));
                    copy.isComposite = sym.isComposite;
                    copy.definedFile = sym.definedFile;
                    copy.definedLine = sym.definedLine;
                    copy.definedColumn = sym.definedColumn;
                    (this as any)._st.define(copy);
                }
                continue;
            }

            const adsPath = this._findAdsFile(fileName);
            if (adsPath === null) {
                if (this._debug) process.stderr.write(`ImportWithClause: could not find ${fileName}\n`);
                continue;
            }

            const fullPath = path.resolve(adsPath).toLowerCase();
            if (AdaParserBase._parsingInProgress.has(fullPath)) {
                if (this._debug) process.stderr.write(`ImportWithClause: skipping ${fileName} (cycle detected)\n`);
                continue;
            }

            const symbols = this._parseAdsFile(adsPath);
            if (symbols !== null) {
                AdaParserBase._packageCache.set(cacheKey, symbols);
                for (const sym of symbols) {
                    const copy = new Symbol(sym.name, new Set(sym.classification));
                    copy.isComposite = sym.isComposite;
                    copy.definedFile = sym.definedFile;
                    copy.definedLine = sym.definedLine;
                    copy.definedColumn = sym.definedColumn;
                    (this as any)._st.define(copy);
                    if (this._debug) process.stderr.write(`ImportWithClause: imported symbol ${sym.name} from ${packageName}\n`);
                }
            }
        }
    }

    private _packageNameToFileName(packageName: string): string {
        return packageName.toLowerCase().replace(/\./g, '-') + ".ads";
    }

    private _findAdsFile(fileName: string): string | null {
        if (this._currentFile) {
            const dir = path.dirname(this._currentFile);
            const candidate = path.join(dir, fileName);
            if (fs.existsSync(candidate)) return candidate;
        }
        for (const searchPath of this._searchPaths) {
            const candidate = path.join(searchPath, fileName);
            if (fs.existsSync(candidate)) return candidate;
        }
        return null;
    }

    private _parseAdsFile(adsPath: string): Symbol[] | null {
        const fullPath = path.resolve(adsPath).toLowerCase();
        AdaParserBase._parsingInProgress.add(fullPath);
        try {
            if (this._debug) process.stderr.write(`ImportWithClause: parsing ${adsPath}\n`);
            const input = new CharStream(fs.readFileSync(adsPath, 'utf-8'));
            const lexer = new AdaLexer(input);
            lexer.removeErrorListeners();
            const tokenStream = new CommonTokenStream(lexer);
            const parser = new AdaParser(tokenStream);
            parser.removeErrorListeners();
            (parser as any)._currentFile = path.resolve(adsPath);
            parser.compilation();
            return (parser as any)._st.getExportedSymbols();
        } catch (ex: any) {
            if (this._debug) process.stderr.write(`ImportWithClause: error parsing ${adsPath}: ${ex.message}\n`);
            return null;
        } finally {
            AdaParserBase._parsingInProgress.delete(fullPath);
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
