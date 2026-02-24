import antlr4 from 'antlr4';
import { SymbolTable } from "./SymbolTable.js";
import { Symbol } from "./Symbol.js";
import { TypeClassification } from "./TypeClassification.js";
import CLexer from "./CLexer.js";
import CParser from "./CParser.js";

function isDeclarationContext(x) {
    return x?.ruleIndex === CParser.RULE_declaration;
}

function isFunctionDefinitionContext(x) {
    return x?.ruleIndex === CParser.RULE_functionDefinition;
}

const ALL_SEMANTIC_FUNCTIONS = [
    "IsAlignmentSpecifier", "IsAtomicTypeSpecifier", "IsAttributeDeclaration",
    "IsAttributeSpecifier", "IsAttributeSpecifierSequence", "IsDeclaration",
    "IsDeclarationSpecifier", "IsTypeSpecifierQualifier", "IsEnumSpecifier",
    "IsFunctionSpecifier", "IsStatement", "IsStaticAssertDeclaration",
    "IsStorageClassSpecifier", "IsStructOrUnionSpecifier", "IsTypedefName",
    "IsTypeofSpecifier", "IsTypeQualifier", "IsTypeSpecifier", "IsCast",
    "IsNullStructDeclarationListExtension",
    "IsGnuAttributeBeforeDeclarator",
    "IsSizeofTypeName"
];

function parseNoSemantics(args) {
    const result = new Set();
    for (const a of args) {
        const lower = a.toLowerCase();
        if (lower.startsWith("--no-semantics")) {
            const eqIndex = a.indexOf('=');
            if (eqIndex === -1) {
                for (const func of ALL_SEMANTIC_FUNCTIONS) {
                    result.add(func);
                }
            } else {
                const value = a.substring(eqIndex + 1);
                const funcs = value.split(',');
                for (const func of funcs) {
                    result.add(func.trim());
                }
            }
        }
    }
    return result;
}

export default class CParserBase extends antlr4.Parser {
    constructor(input) {
        super(input);
        const args = process.argv;
        this.noSemantics = parseNoSemantics(args);
        this._debug = args.some(a => a.toLowerCase().includes("--debug"));
        this.outputSymbolTable = args.some(a => a.toLowerCase().includes("--output-symbol-table"));
        this.outputAppliedOccurrences = args.some(a => a.toLowerCase().includes("--output-applied-occurrences"));
        this._st = new SymbolTable();
    }

    IsAlignmentSpecifier(k = 1) {
        if (this.noSemantics.has("IsAlignmentSpecifier")) return true;
        const lt1 = this._input.LT(k);
        const text = lt1.text;
        if (this._debug) process.stdout.write("IsAlignmentSpecifier " + lt1);
        const resolved = this._resolveWithOutput(lt1);
        let result = false;
        if (resolved === null) {
            result = false;
        } else if (resolved.classification.has(TypeClassification.AlignmentSpecifier_)) {
            result = true;
        } else {
            result = false;
        }
        if (this._debug) console.log(" " + result);
        return result;
    }

    IsAtomicTypeSpecifier(k = 1) {
        if (this.noSemantics.has("IsAtomicTypeSpecifier")) return true;
        const lt1 = this._input.LT(k);
        const text = lt1.text;
        if (this._debug) process.stdout.write("IsAtomicTypeSpecifier " + lt1);
        const resolved = this._resolveWithOutput(lt1);
        let result = false;
        if (resolved === null) {
            result = false;
        } else if (resolved.classification.has(TypeClassification.AtomicTypeSpecifier_)) {
            result = true;
        } else {
            result = false;
        }
        if (this._debug) console.log(" " + result);
        return result;
    }

    IsAttributeDeclaration() {
        if (this.noSemantics.has("IsAttributeDeclaration")) return true;
        return this.IsAttributeSpecifierSequence();
    }

    IsAttributeSpecifier() {
        if (this.noSemantics.has("IsAttributeSpecifier")) return true;
        const lt1 = this._input.LT(1);
        if (this._debug) process.stdout.write("IsAttributeSpecifier " + lt1);
        const result = lt1.type === CLexer.LeftBracket;
        if (this._debug) console.log(" " + result);
        return result;
    }

    IsAttributeSpecifierSequence() {
        if (this.noSemantics.has("IsAttributeSpecifierSequence")) return true;
        return this.IsAttributeSpecifier();
    }

    IsDeclaration() {
        if (this.noSemantics.has("IsDeclaration")) return true;
        if (this._debug) console.log("IsDeclaration");
        const result = this.IsDeclarationSpecifiers()
            || this.IsAttributeSpecifierSequence()
            || this.IsStaticAssertDeclaration()
            || this.IsAttributeDeclaration();
        if (this._debug) console.log("IsDeclaration " + result);
        return result;
    }

    IsDeclarationSpecifier() {
        if (this.noSemantics.has("IsDeclarationSpecifier")) return true;
        const lt1 = this._input.LT(1);
        const text = lt1.text;
        if (this._debug) console.log("IsDeclarationSpecifier " + lt1);
        const result = this.IsStorageClassSpecifier()
            || this.IsTypeSpecifier()
            || this.IsTypeQualifier()
            || (this.IsFunctionSpecifier() && !this.IsGnuAttributeBeforeDeclarator())
            || this.IsAlignmentSpecifier();
        if (this._debug) console.log("IsDeclarationSpecifier " + result + " for " + lt1);
        return result;
    }

    IsTypeSpecifierQualifier(k = 1) {
        if (this.noSemantics.has("IsTypeSpecifierQualifier")) return true;
        if (this._debug) console.log("IsDeclarationSpecifier");
        const result = this.IsTypeSpecifier(k)
            || this.IsTypeQualifier(k)
            || this.IsAlignmentSpecifier(k);
        if (this._debug) console.log("IsDeclarationSpecifier " + result);
        return result;
    }

    IsDeclarationSpecifiers() {
        return this.IsDeclarationSpecifier();
    }

    IsEnumSpecifier(k = 1) {
        if (this.noSemantics.has("IsEnumSpecifier")) return true;
        const lt1 = this._input.LT(k);
        if (this._debug) process.stdout.write("IsEnumSpecifier " + lt1);
        const result = lt1.type === CLexer.Enum;
        if (this._debug) console.log(" " + result);
        return result;
    }

    IsFunctionSpecifier() {
        if (this.noSemantics.has("IsFunctionSpecifier")) return true;
        const lt1 = this._input.LT(1);
        const text = lt1.text;
        if (this._debug) process.stdout.write("IsFunctionSpecifier " + lt1);
        const resolved = this._resolveWithOutput(lt1);
        let result = false;
        if (resolved === null) {
            result = false;
        } else if (resolved.classification.has(TypeClassification.FunctionSpecifier_)) {
            result = true;
        } else {
            result = false;
        }
        if (this._debug) console.log("IsFunctionSpecifier " + result);
        return result;
    }

    IsGnuAttributeBeforeDeclarator(k = 1) {
        if (this.noSemantics.has("IsGnuAttributeBeforeDeclarator")) return true;
        const ts = this._input;
        let i = k;
        if (ts.LT(i).type !== CLexer.Attribute) return false;
        i++;
        let depth = 0;
        while (true) {
            const t = ts.LT(i++);
            if (t.type < 0) return false; // EOF
            if (t.type === CLexer.LeftParen) depth++;
            else if (t.type === CLexer.RightParen) { depth--; if (depth === 0) break; }
        }
        const next = ts.LT(i).type;
        return next === CLexer.Identifier || next === CLexer.Star || next === CLexer.LeftParen;
    }

    IsStatement() {
        if (this.noSemantics.has("IsStatement")) return true;
        const t1 = this._input.LT(1);
        const t2 = this._input.LT(2);
        if (this._debug) console.log("IsStatement1 " + t1);
        if (this._debug) console.log("IsStatement2 " + t2);
        if (t1.type === CLexer.Identifier && t2.type === CLexer.Colon) {
            if (this._debug) process.stdout.write("IsStatement3 true");
            return true;
        }
        const result = !this.IsDeclaration();
        if (this._debug) process.stdout.write("IsStatement " + result);
        return result;
    }

    IsStaticAssertDeclaration() {
        if (this.noSemantics.has("IsStaticAssertDeclaration")) return true;
        const token = this._input.LT(1);
        if (this._debug) process.stdout.write("IsStaticAssertDeclaration " + token);
        const result = token.type === CLexer.Static_assert;
        if (this._debug) console.log(" " + result);
        return result;
    }

    IsStorageClassSpecifier() {
        if (this.noSemantics.has("IsStorageClassSpecifier")) return true;
        const lt1 = this._input.LT(1);
        const text = lt1.text;
        if (this._debug) process.stdout.write("IsStorageClassSpecifier " + lt1);
        const resolved = this._resolveWithOutput(lt1);
        let result = false;
        if (resolved === null) {
            result = false;
        } else if (resolved.classification.has(TypeClassification.StorageClassSpecifier_)) {
            result = true;
        } else {
            result = false;
        }
        if (this._debug) console.log(" " + result);
        return result;
    }

    IsStructOrUnionSpecifier(k = 1) {
        if (this.noSemantics.has("IsStructOrUnionSpecifier")) return true;
        const token = this._input.LT(k);
        if (this._debug) process.stdout.write("IsStructOrUnionSpecifier " + token);
        const result = token.type === CLexer.Struct ||
            token.type === CLexer.Union;
        if (this._debug) console.log(" " + result);
        return result;
    }

    IsTypedefName(k = 1) {
        if (this.noSemantics.has("IsTypedefName")) return true;
        const lt1 = this._input.LT(k);
        const text = lt1.text;
        if (this._debug) process.stdout.write("IsTypedefName " + lt1);
        const resolved = this._resolveWithOutput(lt1);
        let result = false;
        if (resolved === null) {
            result = false;
        } else if (resolved.classification.has(TypeClassification.Variable_)) {
            result = false;
        } else if (resolved.classification.has(TypeClassification.Function_)) {
            result = false;
        } else {
            result = true;
        }
        if (this._debug) console.log(" " + result);
        return result;
    }

    IsTypeofSpecifier(k = 1) {
        if (this.noSemantics.has("IsTypeofSpecifier")) return true;
        const token = this._input.LT(k);
        if (this._debug) process.stdout.write("IsTypeofSpecifier " + token);
        const result = token.type === CLexer.Typeof ||
            token.type === CLexer.Typeof_unqual;
        if (this._debug) console.log(" " + result);
        return result;
    }

    IsTypeQualifier(k = 1) {
        if (this.noSemantics.has("IsTypeQualifier")) return true;
        const lt1 = this._input.LT(k);
        const text = lt1.text;
        if (this._debug) process.stdout.write("IsTypeQualifier " + lt1);
        const resolved = this._resolveWithOutput(lt1);
        let result = false;
        if (resolved === null) {
            result = false;
        } else if (resolved.classification.has(TypeClassification.TypeQualifier_)) {
            result = true;
        } else {
            result = false;
        }
        if (this._debug) console.log(" " + result);
        return result;
    }

    IsTypeSpecifier(k = 1) {
        if (this.noSemantics.has("IsTypeSpecifier")) return true;
        const lt1 = this._input.LT(k);
        const text = lt1.text;
        if (this._debug) process.stdout.write("IsTypeSpecifier " + lt1);
        const resolved = this._resolveWithOutput(lt1);
        let result = false;
        if (resolved === null) {
            result = false;
        } else if (resolved.classification.has(TypeClassification.TypeSpecifier_)) {
            result = true;
        } else {
            result = false;
        }

        if (result) {
            if (this._debug) console.log(" " + result);
            return result;
        }
        result = this.IsAtomicTypeSpecifier(k) || this.IsStructOrUnionSpecifier(k) || this.IsEnumSpecifier(k)
            || this.IsTypedefName(k) || this.IsTypeofSpecifier(k);
        if (this._debug) console.log(" " + result);
        return result;
    }

    _myGetDeclarationId(y) {
        const token = this._myGetDeclarationToken(y);
        return token !== null ? token.text : null;
    }

    _myGetDeclarationToken(y) {
        if (y === null || y === undefined) return null;

        const directDeclarator = y.directDeclarator();
        if (directDeclarator !== null && directDeclarator !== undefined) {
            const more = directDeclarator.declarator();
            const token = this._myGetDeclarationToken(more);
            if (token !== null) return token;
            if (directDeclarator.Identifier() !== null && directDeclarator.Identifier() !== undefined) {
                return directDeclarator.Identifier().symbol;
            }
        }

        return null;
    }

    EnterDeclaration() {
        if (this._debug) console.log("EnterDeclaration");
        let context = this._ctx;
        for (; context !== null; context = context.parentCtx) {
            if (context instanceof CParser.DeclarationContext) {
                if (!isDeclarationContext(context)) {
                    continue;
                }
                const declaration_context = context;
                const declaration_specifiers = declaration_context.declarationSpecifiers();
                const declaration_specifier = declaration_specifiers?.declarationSpecifier() ?? null;

                let is_typedef = false;
                if (declaration_specifier !== null) {
                    for (const ds of declaration_specifier) {
                        if (ds.storageClassSpecifier()?.Typedef() !== null && ds.storageClassSpecifier()?.Typedef() !== undefined) {
                            is_typedef = true;
                            break;
                        }
                    }
                    for (const ds of declaration_specifier) {
                        if (ds.storageClassSpecifier()?.Typedef() !== null && ds.storageClassSpecifier()?.Typedef() !== undefined) {
                            is_typedef = true;
                            break;
                        }
                    }
                    for (const ds of declaration_specifier) {
                        if (ds.storageClassSpecifier()?.Typedef() !== null && ds.storageClassSpecifier()?.Typedef() !== undefined) {
                            is_typedef = true;
                            break;
                        }
                    }
                    for (const ds of declaration_specifier) {
                        if (ds.typeSpecifier() !== null && ds.typeSpecifier() !== undefined) {
                            const sous = ds.typeSpecifier().structOrUnionSpecifier();
                            if (sous !== null && sous !== undefined) {
                                const id = sous.Identifier();
                                if (id !== null && id !== undefined) {
                                    const idToken = id.symbol;
                                    const text = idToken.text;
                                    const loc = this._getSourceLocation(idToken);
                                    const symbol = new Symbol();
                                    symbol.name = text;
                                    symbol.classification = new Set([TypeClassification.TypeSpecifier_]);
                                    symbol.definedFile = loc.file;
                                    symbol.definedLine = loc.line;
                                    symbol.definedColumn = loc.column;
                                    this._st.define(symbol);
                                    if (this._debug) console.log("New symbol Declaration1 Declarator " + symbol);
                                }
                            }
                        }
                    }
                }

                const init_declaration_list = declaration_context.initDeclaratorList();
                const init_declarators = init_declaration_list?.initDeclarator() ?? null;

                if (init_declarators !== null) {
                    for (const id of init_declarators) {
                        const y = id?.declarator() ?? null;
                        const idToken = this._myGetDeclarationToken(y);
                        if (idToken !== null && idToken !== undefined) {
                            const text = idToken.text;
                            const loc = this._getSourceLocation(idToken);
                            if (is_typedef) {
                                const symbol = new Symbol();
                                symbol.name = text;
                                symbol.classification = new Set([TypeClassification.TypeSpecifier_]);
                                symbol.definedFile = loc.file;
                                symbol.definedLine = loc.line;
                                symbol.definedColumn = loc.column;
                                this._st.define(symbol);
                                if (this._debug) console.log("New symbol Declaration2 Declarator " + symbol);
                            } else {
                                const symbol = new Symbol();
                                symbol.name = text;
                                symbol.classification = new Set([TypeClassification.Variable_]);
                                symbol.definedFile = loc.file;
                                symbol.definedLine = loc.line;
                                symbol.definedColumn = loc.column;
                                this._st.define(symbol);
                                if (this._debug) console.log("New symbol Declaration3 Declarator " + symbol);
                            }
                        }
                    }
                }
            }
            if (context instanceof CParser.FunctionDefinitionContext) {
                if (!isFunctionDefinitionContext(context)) {
                    continue;
                }
                const fd = context;
                const de = fd.declarator();
                if (de === null || de === undefined) continue;
                const dd = de?.directDeclarator();
                if (dd === null || dd === undefined) continue;
                if (dd !== null && dd.Identifier() !== null && dd.Identifier() !== undefined) {
                    const idToken = dd.Identifier().symbol;
                    const text = idToken.text;
                    const loc = this._getSourceLocation(idToken);
                    const symbol = new Symbol();
                    symbol.name = text;
                    symbol.classification = new Set([TypeClassification.Function_]);
                    symbol.definedFile = loc.file;
                    symbol.definedLine = loc.line;
                    symbol.definedColumn = loc.column;
                    this._st.define(symbol);
                    if (this._debug) console.log("New symbol Declarationf Declarator " + symbol);
                    return;
                }
            }
        }
    }

    IsNullStructDeclarationListExtension() {
        if (this.noSemantics.has("IsNullStructDeclarationListExtension")) return true;
        return true;
    }

    EnterScope() {
        if (this._debug) console.log("EnterScope");
        this._st.pushBlockScope();
    }

    ExitScope() {
        if (this._debug) console.log("ExitScope");
        this._st.popBlockScope();
    }

    LookupSymbol() {
        const token = this._input.LT(-1);
        if (token === null) return;
        const text = token.text;
        const resolved = this._st.resolve(text);
        if (this.outputAppliedOccurrences && resolved !== null) {
            const appliedLoc = this._getSourceLocation(token);
            process.stderr.write(`Applied occurrence: ${text} at ${appliedLoc.file}:${appliedLoc.line}:${appliedLoc.column} -> Defined at ${resolved.definedFile}:${resolved.definedLine}:${resolved.definedColumn}\n`);
        }
    }

    OutputSymbolTable() {
        if (this.outputSymbolTable) {
            process.stderr.write(this._st.toString() + "\n");
        }
    }

    _resolveWithOutput(token) {
        if (token === null || token === undefined) return null;
        const text = token.text;
        const resolved = this._st.resolve(text);
        if (this.outputAppliedOccurrences && resolved !== null) {
            const appliedLoc = this._getSourceLocation(token);
            process.stderr.write(`Applied occurrence: ${text} at ${appliedLoc.file}:${appliedLoc.line}:${appliedLoc.column} -> Defined at ${resolved.definedFile}:${resolved.definedLine}:${resolved.definedColumn}\n`);
        }
        return resolved;
    }

    _getSourceLocation(token) {
        if (token === null || token === undefined) {
            return { file: "", line: 0, column: 0 };
        }

        let fileName = "<unknown>";
        const line = token.line;
        const column = token.column;
        let lineAdjusted = line;

        const ts = this._input;
        const ind = token.tokenIndex;

        for (let j = ind; j >= 0; j--) {
            const t = ts.get(j);
            if (t === null) break;
            if (t.type === CLexer.LineDirective) {
                const txt = t.text;
                const parts = txt.split(/\s+/);
                if (parts.length >= 3) {
                    const dirLine = parseInt(parts[1], 10);
                    if (!isNaN(dirLine)) {
                        const lineDirective = t.line;
                        const lineDiff = line - lineDirective;
                        lineAdjusted = lineDiff + dirLine - 1;
                        fileName = parts[2].trim();
                        if (fileName.startsWith("\"") && fileName.endsWith("\"")) {
                            fileName = fileName.substring(1, fileName.length - 1);
                        }
                    }
                }
                break;
            }
        }

        return { file: fileName, line: lineAdjusted, column: column };
    }

    IsInitDeclaratorList() {
        // Cannot be initDeclaratorList if the first thing is a type.
        // Types need to go to preceding declarationSpecifiers.
        if (this.noSemantics.has("IsInitDeclaratorList")) return true;
        const lt1 = this._input.LT(1);
        const text = lt1.text;
        if (this._debug) process.stdout.write("IsInitDeclaratorList " + lt1);
        const resolved = this._resolveWithOutput(lt1);
        let result = false;
        if (resolved === null) {
            result = true;
        } else if (resolved.classification.has(TypeClassification.TypeQualifier_) || resolved.classification.has(TypeClassification.TypeSpecifier_)) {
            result = false;
        } else {
            result = true;
        }
        if (this._debug) console.log(" " + result);
        return result;
    }

    IsSomethingOfTypeName() {
        if (this.noSemantics.has("IsSizeofTypeName")) return true;
        const ts = this._input;
        if (!(ts.LT(1).type === CLexer.Sizeof ||
              ts.LT(1).type === CLexer.Countof ||
              ts.LT(1).type === CLexer.Alignof ||
              ts.LT(1).type === CLexer.Maxof ||
              ts.LT(1).type === CLexer.Minof)) return false;
        if (ts.LT(2).type !== CLexer.LeftParen) return false;
        if (this.IsTypeName(3)) return true;
        return false;
    }

    IsTypeName(k = 1) {
        return this.IsSpecifierQualifierList(k);
    }

    IsSpecifierQualifierList(k = 1) {
        if (this.IsGnuAttributeBeforeDeclarator(k)) return true;
        if (this.IsTypeSpecifierQualifier(k)) return true;
        return false;
    }

    IsCast() {
        let result = false;
        if (this.noSemantics.has("IsCast")) return true;
        const t1 = this._input.LT(1);
        const t2 = this._input.LT(2);
        if (this._debug) console.log("IsCast1 " + t1);
        if (this._debug) console.log("IsCast2 " + t2);
        if (t1.type !== CLexer.LeftParen) {
            if (this._debug) process.stdout.write("IsCast " + result);
        } else if (t2.type !== CLexer.Identifier) {
            result = true;
        } else {
            const resolved = this._resolveWithOutput(t2);
            if (resolved === null) {
                result = false;
            } else if (resolved.classification.has(TypeClassification.TypeSpecifier_)) {
                result = true;
            } else {
                result = false;
            }
        }
        if (this._debug) process.stdout.write("IsStatement " + result);
        return result;
    }
}
