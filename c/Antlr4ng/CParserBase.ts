import { Parser, TokenStream, CommonTokenStream, ParserRuleContext } from "antlr4ng";
import { SymbolTable } from "./SymbolTable.js";
import { Symbol } from "./Symbol.js";
import { TypeClassification } from "./TypeClassification.js";
import { CLexer } from "./CLexer.js";
import {
    CParser,
    DeclarationContext,
    FunctionDefinitionContext,
    DeclaratorContext
} from "./CParser.js";


function isDeclarationContext(x: any): x is DeclarationContext {
  return x?.ruleIndex === CParser.RULE_declaration;
}


function isFunctionDefinitionContext(x: any): x is FunctionDefinitionContext {
  return x?.ruleIndex === CParser.RULE_functionDefinition;
}

// List of all semantic function names
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

function parseNoSemantics(args: string[]): Set<string> {
    const result = new Set<string>();
    for (const a of args) {
        const lower = a.toLowerCase();
        if (lower.startsWith("--no-semantics")) {
            const eqIndex = a.indexOf('=');
            if (eqIndex === -1) {
                // --no-semantics without value: disable all semantic functions
                for (const func of ALL_SEMANTIC_FUNCTIONS) {
                    result.add(func);
                }
            } else {
                // --no-semantics=Func1,Func2,...
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

export abstract class CParserBase extends Parser {
    private _st: SymbolTable;
    private debug: boolean = false;
    private outputSymbolTable: boolean = false;
    private outputAppliedOccurrences: boolean = false;
    private noSemantics: Set<string> = new Set<string>();

    constructor(input: TokenStream) {
        super(input);
        // Get options from command line args
        const args = process.argv;
        this.noSemantics = parseNoSemantics(args);
        this.debug = args.some(a => a.toLowerCase().includes("--debug"));
        this.outputSymbolTable = args.some(a => a.toLowerCase().includes("--output-symbol-table"));
        this.outputAppliedOccurrences = args.some(a => a.toLowerCase().includes("--output-applied-occurrences"));
        this._st = new SymbolTable();
    }

    public IsAlignmentSpecifier(k: number = 1): boolean {
        if (this.noSemantics.has("IsAlignmentSpecifier")) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(k);
        const text = lt1!.text!;
        if (this.debug) process.stdout.write("IsAlignmentSpecifier " + lt1);
        const resolved = this.resolveWithOutput(lt1);
        let result = false;
        if (resolved === null) {
            result = false;
        } else if (resolved.classification.has(TypeClassification.AlignmentSpecifier_)) {
            result = true;
        } else {
            result = false;
        }
        if (this.debug) console.log(" " + result);
        return result;
    }

    public IsAtomicTypeSpecifier(k: number = 1): boolean {
        if (this.noSemantics.has("IsAtomicTypeSpecifier")) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(k);
        const text = lt1!.text!;
        if (this.debug) process.stdout.write("IsAtomicTypeSpecifier " + lt1);
        const resolved = this.resolveWithOutput(lt1);
        let result = false;
        if (resolved === null) {
            result = false;
        } else if (resolved.classification.has(TypeClassification.AtomicTypeSpecifier_)) {
            result = true;
        } else {
            result = false;
        }
        if (this.debug) console.log(" " + result);
        return result;
    }

    public IsAttributeDeclaration(): boolean {
        if (this.noSemantics.has("IsAttributeDeclaration")) return true;
        return this.IsAttributeSpecifierSequence();
    }

    public IsAttributeSpecifier(): boolean {
        if (this.noSemantics.has("IsAttributeSpecifier")) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(1);
        if (this.debug) process.stdout.write("IsAttributeSpecifier " + lt1);
        const result = lt1!.type === CLexer.LeftBracket;
        if (this.debug) console.log(" " + result);
        return result;
    }

    public IsAttributeSpecifierSequence(): boolean {
        if (this.noSemantics.has("IsAttributeSpecifierSequence")) return true;
        return this.IsAttributeSpecifier();
    }

    public IsDeclaration(): boolean {
        if (this.noSemantics.has("IsDeclaration")) return true;
        if (this.debug) console.log("IsDeclaration");
        const result = this.IsDeclarationSpecifiers()
            || this.IsAttributeSpecifierSequence()
            || this.IsStaticAssertDeclaration()
            || this.IsAttributeDeclaration();
        if (this.debug) console.log("IsDeclaration " + result);
        return result;
    }

    public IsDeclarationSpecifier(): boolean {
        if (this.noSemantics.has("IsDeclarationSpecifier")) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(1);
        const text = lt1!.text!;
        if (this.debug) console.log("IsDeclarationSpecifier " + lt1);
        const result = this.IsStorageClassSpecifier()
            || this.IsTypeSpecifier()
            || this.IsTypeQualifier()
            || (this.IsFunctionSpecifier() && !this.IsGnuAttributeBeforeDeclarator())
            || this.IsAlignmentSpecifier();
        if (this.debug) console.log("IsDeclarationSpecifier " + result + " for " + lt1);
        return result;
    }

    public IsTypeSpecifierQualifier(k: number = 1): boolean {
        if (this.noSemantics.has("IsTypeSpecifierQualifier")) return true;
        if (this.debug) console.log("IsDeclarationSpecifier");
        const result = this.IsTypeSpecifier(k)
            || this.IsTypeQualifier(k)
            || this.IsAlignmentSpecifier(k);
        if (this.debug) console.log("IsDeclarationSpecifier " + result);
        return result;
    }

    public IsDeclarationSpecifiers(): boolean {
        return this.IsDeclarationSpecifier();
    }

    public IsEnumSpecifier(k: number = 1): boolean {
        if (this.noSemantics.has("IsEnumSpecifier")) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(k);
        if (this.debug) process.stdout.write("IsEnumSpecifier " + lt1);
        const result = lt1!.type === CLexer.Enum;
        if (this.debug) console.log(" " + result);
        return result;
    }

    public IsFunctionSpecifier(): boolean {
        if (this.noSemantics.has("IsFunctionSpecifier")) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(1);
        const text = lt1!.text!;
        if (this.debug) process.stdout.write("IsFunctionSpecifier " + lt1);
        const resolved = this.resolveWithOutput(lt1);
        let result = false;
        if (resolved === null) {
            result = false;
        } else if (resolved.classification.has(TypeClassification.FunctionSpecifier_)) {
            result = true;
        } else {
            result = false;
        }
        if (this.debug) console.log("IsFunctionSpecifier " + result);
        return result;
    }

    public IsGnuAttributeBeforeDeclarator(k: number = 1): boolean {
        if (this.noSemantics.has("IsGnuAttributeBeforeDeclarator")) return false;
        const ts = this.inputStream as CommonTokenStream;
        let i = k;
        if (ts.LT(i)!.type !== CLexer.Attribute) return false;
        i++;
        let depth = 0;
        while (true) {
            const t = ts.LT(i++)!;
            if (t.type < 0) return false; // EOF
            if (t.type === CLexer.LeftParen) depth++;
            else if (t.type === CLexer.RightParen) { depth--; if (depth === 0) break; }
        }
        const next = ts.LT(i)!.type;
        return next === CLexer.Identifier || next === CLexer.Star || next === CLexer.LeftParen;
    }

    public IsStatement(): boolean {
        if (this.noSemantics.has("IsStatement")) return true;
        const t1 = (this.inputStream as CommonTokenStream).LT(1);
        const t2 = (this.inputStream as CommonTokenStream).LT(2);
        if (this.debug) console.log("IsStatement1 " + t1);
        if (this.debug) console.log("IsStatement2 " + t2);
        if (t1!.type === CLexer.Identifier && t2!.type === CLexer.Colon) {
            if (this.debug) process.stdout.write("IsStatement3 true");
            return true;
        }
        const result = !this.IsDeclaration();
        if (this.debug) process.stdout.write("IsStatement " + result);
        return result;
    }

    public IsStaticAssertDeclaration(): boolean {
        if (this.noSemantics.has("IsStaticAssertDeclaration")) return true;
        const token = (this.inputStream as CommonTokenStream).LT(1);
        if (this.debug) process.stdout.write("IsStaticAssertDeclaration " + token);
        const result = token!.type === CLexer.Static_assert;
        if (this.debug) console.log(" " + result);
        return result;
    }

    public IsStorageClassSpecifier(): boolean {
        if (this.noSemantics.has("IsStorageClassSpecifier")) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(1);
        const text = lt1!.text!;
        if (this.debug) process.stdout.write("IsStorageClassSpecifier " + lt1);
        const resolved = this.resolveWithOutput(lt1);
        let result = false;
        if (resolved === null) {
            result = false;
        } else if (resolved.classification.has(TypeClassification.StorageClassSpecifier_)) {
            result = true;
        } else {
            result = false;
        }
        if (this.debug) console.log(" " + result);
        return result;
    }

    public IsStructOrUnionSpecifier(k: number = 1): boolean {
        if (this.noSemantics.has("IsStructOrUnionSpecifier")) return true;
        const token = (this.inputStream as CommonTokenStream).LT(k);
        if (this.debug) process.stdout.write("IsStructOrUnionSpecifier " + token);
        const result = token!.type === CLexer.Struct ||
            token!.type === CLexer.Union;
        if (this.debug) console.log(" " + result);
        return result;
    }

    public IsTypedefName(k: number = 1): boolean {
        if (this.noSemantics.has("IsTypedefName")) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(k);
        const text = lt1!.text!;
        if (this.debug) process.stdout.write("IsTypedefName " + lt1);
        const resolved = this.resolveWithOutput(lt1);
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
        if (this.debug) console.log(" " + result);
        return result;
    }

    public IsTypeofSpecifier(k: number = 1): boolean {
        if (this.noSemantics.has("IsTypeofSpecifier")) return true;
        const token = (this.inputStream as CommonTokenStream).LT(k);
        if (this.debug) process.stdout.write("IsTypeofSpecifier " + token);
        const result = token!.type === CLexer.Typeof ||
            token!.type === CLexer.Typeof_unqual;
        if (this.debug) console.log(" " + result);
        return result;
    }

    public IsTypeQualifier(k: number = 1): boolean {
        if (this.noSemantics.has("IsTypeQualifier")) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(k);
        const text = lt1!.text!;
        if (this.debug) process.stdout.write("IsTypeQualifier " + lt1);
        const resolved = this.resolveWithOutput(lt1);
        let result = false;
        if (resolved === null) {
            result = false;
        } else if (resolved.classification.has(TypeClassification.TypeQualifier_)) {
            result = true;
        } else {
            result = false;
        }
        if (this.debug) console.log(" " + result);
        return result;
    }

    public IsTypeSpecifier(k: number = 1): boolean {
        if (this.noSemantics.has("IsTypeSpecifier")) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(k);
        const text = lt1!.text!;
        if (this.debug) process.stdout.write("IsTypeSpecifier " + lt1);
        const resolved = this.resolveWithOutput(lt1);
        let result = false;
        if (resolved === null) {
            result = false;
        } else if (resolved.classification.has(TypeClassification.TypeSpecifier_)) {
            result = true;
        } else {
            result = false;
        }

        if (result) {
            if (this.debug) console.log(" " + result);
            return result;
        }
        result = this.IsAtomicTypeSpecifier(k) || this.IsStructOrUnionSpecifier(k) || this.IsEnumSpecifier(k)
            || this.IsTypedefName(k) || this.IsTypeofSpecifier(k);
        if (this.debug) console.log(" " + result);
        return result;
    }

    public EnterDeclaration(): void {
        if (this.debug) console.log("EnterDeclaration");
        let context: ParserRuleContext | null = this.context;
        for (; context !== null; context = context.parent) {
            if (context instanceof DeclarationContext) {
		if ( ! isDeclarationContext(context)) {
		    continue;
		}
                const declaration_context = context as DeclarationContext;
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
                        if (ds.typeSpecifier() !== null && ds.typeSpecifier() != undefined) {
			    const sous = ds.typeSpecifier().structOrUnionSpecifier();
			    if (sous !== null && sous !== undefined) {
				const id = sous.Identifier();
				if (id !== null && id !== undefined) {
				    const idToken = id.symbol;
				    const text = idToken.text;
				    const loc = this.getSourceLocation(idToken);
				    const symbol = new Symbol();
				    symbol.name = text;
				    symbol.classification = new Set([TypeClassification.TypeSpecifier_]);
				    symbol.definedFile = loc.file;
				    symbol.definedLine = loc.line;
				    symbol.definedColumn = loc.column;
				    this._st.define(symbol);
				    if (this.debug) console.log("New symbol Declaration1 Declarator " + symbol);
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
                        const idToken = this.getDeclarationToken(y);
                        if (idToken !== null && idToken != undefined) {
                            const text = idToken.text;
                            const loc = this.getSourceLocation(idToken);
                            if (is_typedef) {
                                const symbol = new Symbol();
                                symbol.name = text;
                                symbol.classification = new Set([TypeClassification.TypeSpecifier_]);
                                symbol.definedFile = loc.file;
                                symbol.definedLine = loc.line;
                                symbol.definedColumn = loc.column;
                                this._st.define(symbol);
                                if (this.debug) console.log("New symbol Declaration2 Declarator " + symbol);
                            } else {
                                const symbol = new Symbol();
                                symbol.name = text;
                                symbol.classification = new Set([TypeClassification.Variable_]);
                                symbol.definedFile = loc.file;
                                symbol.definedLine = loc.line;
                                symbol.definedColumn = loc.column;
                                this._st.define(symbol);
                                if (this.debug) console.log("New symbol Declaration3 Declarator " + symbol);
                            }
                        }
                    }
                }
            }
            if (context instanceof FunctionDefinitionContext) {
                const fd = context as FunctionDefinitionContext;
		if ( ! isFunctionDefinitionContext(context)) {
		    continue;
		}
                const de = fd.declarator();
		if (de === null || de === undefined) continue;
                const dd = de?.directDeclarator();
		if (dd === null || dd === undefined) continue;
                if (dd !== null && dd.Identifier() !== null && dd.Identifier() !== undefined) {
                    const idToken = dd.Identifier()!.symbol;
                    const text = idToken.text;
                    const loc = this.getSourceLocation(idToken);
                    const symbol = new Symbol();
                    symbol.name = text;
                    symbol.classification = new Set([TypeClassification.Function_]);
                    symbol.definedFile = loc.file;
                    symbol.definedLine = loc.line;
                    symbol.definedColumn = loc.column;
                    this._st.define(symbol);
                    if (this.debug) console.log("New symbol Declarationf Declarator " + symbol);
                    return;
                }
            }
        }
    }

    private getDeclarationId(y: DeclaratorContext | null): string | null {
        const token = this.getDeclarationToken(y);
        return token !== null ? token.text : null;
    }

    private getDeclarationToken(y: DeclaratorContext | null): any | null {
        // Go down the tree and find a declarator with Identifier.
        if (y === null) return null;

        // Check if this declarator has a direct declarator with an identifier
        const directDeclarator = y.directDeclarator();
        if (directDeclarator !== null) {
            const more = directDeclarator.declarator();
            const token = this.getDeclarationToken(more);
            if (token !== null) return token;
            if (directDeclarator.Identifier() !== null) {
                return directDeclarator.Identifier()!.symbol;
            }
        }

        return null;
    }

    // Define to return "true" because "gcc -c -std=c2x" accepts an empty
    // struct-declaration-list.
    public IsNullStructDeclarationListExtension(): boolean {
        if (this.noSemantics.has("IsNullStructDeclarationListExtension")) return true;
        return true;
    }

    public EnterScope(): void {
        if (this.debug) console.log("EnterScope");
        this._st.pushBlockScope();
    }

    public ExitScope(): void {
        if (this.debug) console.log("ExitScope");
        this._st.popBlockScope();
    }

    public LookupSymbol(): void {
        // Get the token that was just parsed (the Identifier)
        const token = (this.inputStream as CommonTokenStream).LT(-1);
        if (token === null) return;
        const text = token.text!;
        const resolved = this._st.resolve(text);
        if (this.outputAppliedOccurrences && resolved !== null) {
            const appliedLoc = this.getSourceLocation(token);
            process.stderr.write(`Applied occurrence: ${text} at ${appliedLoc.file}:${appliedLoc.line}:${appliedLoc.column} -> Defined at ${resolved.definedFile}:${resolved.definedLine}:${resolved.definedColumn}\n`);
        }
    }

    public OutputSymbolTable(): void {
        if (this.outputSymbolTable) {
            process.stderr.write(this._st.toString() + "\n");
        }
    }

    private resolveWithOutput(token: any): Symbol | null {
        if (token === null || token === undefined) return null;
        const text = token.text;
        const resolved = this._st.resolve(text);
        if (this.outputAppliedOccurrences && resolved !== null) {
            const appliedLoc = this.getSourceLocation(token);
            process.stderr.write(`Applied occurrence: ${text} at ${appliedLoc.file}:${appliedLoc.line}:${appliedLoc.column} -> Defined at ${resolved.definedFile}:${resolved.definedLine}:${resolved.definedColumn}\n`);
        }
        return resolved;
    }

    // Helper method to get source location from a token, accounting for #line directives
    private getSourceLocation(token: any): { file: string; line: number; column: number } {
        if (token === null || token === undefined) {
            return { file: "", line: 0, column: 0 };
        }

        let fileName = "<unknown>";
        const line = token.line;
        const column = token.column;
        let lineAdjusted = line;

        const ts = this.inputStream as CommonTokenStream;
        const ind = token.tokenIndex;

        // Search back from token index to find last LineDirective
        for (let j = ind; j >= 0; j--) {
            const t = ts.get(j);
            if (t === null) break;
            if (t.type === CLexer.LineDirective) {
                // Found it
                const txt = t.text!;
                const parts = txt.split(/\s+/);
                if (parts.length >= 3) {
                    const dirLine = parseInt(parts[1], 10);
                    if (!isNaN(dirLine)) {
                        const lineDirective = t.line;
                        const lineDiff = line - lineDirective;
                        lineAdjusted = lineDiff + dirLine - 1;
                        fileName = parts[2].trim();
                        // Remove quotes if present
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

    public IsSomethingOfTypeName(): boolean {
        if (this.noSemantics.has("IsSizeofTypeName")) return false;
        const ts = this.inputStream as CommonTokenStream;
        if (!(ts.LT(1)!.type === CLexer.Sizeof ||
              ts.LT(1)!.type === CLexer.Countof ||
              ts.LT(1)!.type === CLexer.Alignof ||
              ts.LT(1)!.type === CLexer.Maxof ||
              ts.LT(1)!.type === CLexer.Minof)) return false;
        if (ts.LT(2)!.type !== CLexer.LeftParen) return false;
        if (this.IsTypeName(3)) return true;
        return false;
    }

    public IsTypeName(k: number = 1): boolean {
        return this.IsSpecifierQualifierList(k);
    }

    public IsSpecifierQualifierList(k: number = 1): boolean {
        if (this.IsGnuAttributeBeforeDeclarator(k)) return true;
        if (this.IsTypeSpecifierQualifier(k)) return true;
        return false;
    }

    public IsCast(): boolean {
        let result = false;
        // Look for a cast.
        if (this.noSemantics.has("IsCast")) return true;
        const t1 = (this.inputStream as CommonTokenStream).LT(1);
        const t2 = (this.inputStream as CommonTokenStream).LT(2);
        if (this.debug) console.log("IsCast1 " + t1);
        if (this.debug) console.log("IsCast2 " + t2);
        if (t1!.type !== CLexer.LeftParen) {
            if (this.debug) process.stdout.write("IsCast " + result);
        } else if (t2!.type !== CLexer.Identifier) {
            // Assume typecast until otherwise.
            result = true;
        } else {
            // Check id.
            const resolved = this.resolveWithOutput(t2);
            if (resolved === null) {
                // It's not in symbol table.
                result = false;
            } else if (resolved.classification.has(TypeClassification.TypeSpecifier_)) {
                result = true;
            } else {
                result = false;
            }
        }
        if (this.debug) process.stdout.write("IsStatement " + result);
        return result;
    }
}
