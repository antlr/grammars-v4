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

export abstract class CParserBase extends Parser {
    private _st: SymbolTable;
    private debug: boolean = false;
    private noSemantics: boolean = false;

    constructor(input: TokenStream) {
        super(input);
        // Get options from command line args
        const args = process.argv;
        this.noSemantics = args.some(a => a.toLowerCase().includes("-no-semantics"));
        this.debug = args.some(a => a.toLowerCase().includes("-debug"));
        this._st = new SymbolTable();
    }

    public IsAlignmentSpecifier(): boolean {
        if (this.noSemantics) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(1);
        const text = lt1!.text!;
        if (this.debug) process.stdout.write("IsAlignmentSpecifier " + lt1);
        const resolved = this._st.resolve(text);
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

    public IsAtomicTypeSpecifier(): boolean {
        if (this.noSemantics) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(1);
        const text = lt1!.text!;
        if (this.debug) process.stdout.write("IsAtomicTypeSpecifier " + lt1);
        const resolved = this._st.resolve(text);
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
        if (this.noSemantics) return true;
        return this.IsAttributeSpecifierSequence();
    }

    public IsAttributeSpecifier(): boolean {
        if (this.noSemantics) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(1);
        if (this.debug) process.stdout.write("IsAttributeSpecifier " + lt1);
        const result = lt1!.type === CLexer.LeftBracket;
        if (this.debug) console.log(" " + result);
        return result;
    }

    public IsAttributeSpecifierSequence(): boolean {
        if (this.noSemantics) return true;
        return this.IsAttributeSpecifier();
    }

    public IsDeclaration(): boolean {
        if (this.noSemantics) return true;
        if (this.debug) console.log("IsDeclaration");
        const result = this.IsDeclarationSpecifiers()
            || this.IsAttributeSpecifierSequence()
            || this.IsStaticAssertDeclaration()
            || this.IsAttributeDeclaration();
        if (this.debug) console.log("IsDeclaration " + result);
        return result;
    }

    public IsDeclarationSpecifier(): boolean {
        if (this.noSemantics) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(1);
        const text = lt1!.text!;
        if (this.debug) console.log("IsDeclarationSpecifier " + lt1);
        const result = this.IsStorageClassSpecifier()
            || this.IsTypeSpecifier()
            || this.IsTypeQualifier()
            || this.IsFunctionSpecifier()
            || this.IsAlignmentSpecifier();
        if (this.debug) console.log("IsDeclarationSpecifier " + result + " for " + lt1);
        return result;
    }

    public IsTypeSpecifierQualifier(): boolean {
        if (this.noSemantics) return true;
        if (this.debug) console.log("IsDeclarationSpecifier");
        const result = this.IsTypeSpecifier()
            || this.IsTypeQualifier()
            || this.IsAlignmentSpecifier();
        if (this.debug) console.log("IsDeclarationSpecifier " + result);
        return result;
    }

    public IsDeclarationSpecifiers(): boolean {
        return this.IsDeclarationSpecifier();
    }

    public IsEnumSpecifier(): boolean {
        if (this.noSemantics) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(1);
        if (this.debug) process.stdout.write("IsEnumSpecifier " + lt1);
        const result = lt1!.type === CLexer.Enum;
        if (this.debug) console.log(" " + result);
        return result;
    }

    public IsFunctionSpecifier(): boolean {
        if (this.noSemantics) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(1);
        const text = lt1!.text!;
        if (this.debug) process.stdout.write("IsFunctionSpecifier " + lt1);
        const resolved = this._st.resolve(text);
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

    public IsStatement(): boolean {
        if (this.noSemantics) return true;
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
        if (this.noSemantics) return true;
        const token = (this.inputStream as CommonTokenStream).LT(1);
        if (this.debug) process.stdout.write("IsStaticAssertDeclaration " + token);
        const result = token!.type === CLexer.Static_assert;
        if (this.debug) console.log(" " + result);
        return result;
    }

    public IsStorageClassSpecifier(): boolean {
        if (this.noSemantics) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(1);
        const text = lt1!.text!;
        if (this.debug) process.stdout.write("IsStorageClassSpecifier " + lt1);
        const resolved = this._st.resolve(text);
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

    public IsStructOrUnionSpecifier(): boolean {
        if (this.noSemantics) return true;
        const token = (this.inputStream as CommonTokenStream).LT(1);
        if (this.debug) process.stdout.write("IsStructOrUnionSpecifier " + token);
        const result = token!.type === CLexer.Struct ||
            token!.type === CLexer.Union;
        if (this.debug) console.log(" " + result);
        return result;
    }

    public IsTypedefName(): boolean {
        if (this.noSemantics) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(1);
        const text = lt1!.text!;
        if (this.debug) process.stdout.write("IsTypedefName " + lt1);
        const resolved = this._st.resolve(text);
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

    public IsTypeofSpecifier(): boolean {
        if (this.noSemantics) return true;
        const token = (this.inputStream as CommonTokenStream).LT(1);
        if (this.debug) process.stdout.write("IsTypeofSpecifier " + token);
        const result = token!.type === CLexer.Typeof ||
            token!.type === CLexer.Typeof_unqual;
        if (this.debug) console.log(" " + result);
        return result;
    }

    public IsTypeQualifier(): boolean {
        if (this.noSemantics) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(1);
        const text = lt1!.text!;
        if (this.debug) process.stdout.write("IsTypeQualifier " + lt1);
        const resolved = this._st.resolve(text);
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

    public IsTypeSpecifier(): boolean {
        if (this.noSemantics) return true;
        const lt1 = (this.inputStream as CommonTokenStream).LT(1);
        const text = lt1!.text!;
        if (this.debug) process.stdout.write("IsTypeSpecifier " + lt1);
        const resolved = this._st.resolve(text);
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
        result = this.IsAtomicTypeSpecifier() || this.IsStructOrUnionSpecifier() || this.IsEnumSpecifier()
            || this.IsTypedefName() || this.IsTypeofSpecifier();
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
                const declarationContext = context as DeclarationContext;
                const declarationSpecifiers = declarationContext.declarationSpecifiers();
                const declarationSpecifier = declarationSpecifiers?.declarationSpecifier() ?? null;
                const initDeclarationList = declarationContext.initDeclaratorList();
                const initDeclarators = initDeclarationList?.initDeclarator() ?? null;

                if (initDeclarators !== null) {
                    let isTypedef = false;
                    if (declarationSpecifier !== null) {
                        for (const ds of declarationSpecifier) {
                            if (ds.storageClassSpecifier()?.Typedef() !== null && ds.storageClassSpecifier()?.Typedef() !== undefined) {
                                isTypedef = true;
                                break;
                            }
                        }
                    }
                    for (const id of initDeclarators) {
                        const y = id?.declarator() ?? null;
                        const identifier = this.getDeclarationId(y);
                        if (identifier !== null && identifier != undefined) {
                            const text = identifier;
                            if (isTypedef) {
                                const symbol = new Symbol();
                                symbol.name = text;
                                symbol.classification = new Set([TypeClassification.TypeSpecifier_]);
                                this._st.define(symbol);
                                if (this.debug) console.log("New symbol Declaration2 Declarator " + symbol);
                            } else {
                                const symbol = new Symbol();
                                symbol.name = text;
                                symbol.classification = new Set([TypeClassification.Variable_]);
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
                    const text = dd.Identifier()!.getText();
                    const symbol = new Symbol();
                    symbol.name = text;
                    symbol.classification = new Set([TypeClassification.Function_]);
                    this._st.define(symbol);
                    if (this.debug) console.log("New symbol Declarationf Declarator " + symbol);
                    return;
                }
            }
        }
    }

    private getDeclarationId(y: DeclaratorContext | null): string | null {
        // Go down the tree and find a declarator with Identifier.
        if (y === null) return null;

        // Check if this declarator has a direct declarator with an identifier
        const directDeclarator = y.directDeclarator();
        if (directDeclarator !== null) {
            const more = directDeclarator.declarator();
            const xxx = this.getDeclarationId(more);
            if (xxx !== null) return xxx;
            if (directDeclarator.Identifier() !== null) {
                return directDeclarator.Identifier()!.getText();
            }
        }

        return null;
    }

    // Define to return "true" because "gcc -c -std=c2x" accepts an empty
    // struct-declaration-list.
    public IsNullStructDeclarationListExtension(): boolean {
        return true;
    }

    public IsCast(): boolean {
        let result = false;
        // Look for a cast.
        if (this.noSemantics) return true;
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
            const text = t2!.text!;
            const resolved = this._st.resolve(text);
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
