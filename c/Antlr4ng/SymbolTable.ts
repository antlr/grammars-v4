import { Symbol } from "./Symbol.js";
import { TypeClassification } from "./TypeClassification.js";

export class SymbolTable {
    private scopeStack: Symbol[] = [];

    constructor() {
        const globalScope = this.createSymbol("global", TypeClassification.Global_);
        this.scopeStack.push(globalScope);

        this.define(this.createSymbol("auto", TypeClassification.StorageClassSpecifier_));
        this.define(this.createSymbol("constexpr", TypeClassification.StorageClassSpecifier_));
        this.define(this.createSymbol("extern", TypeClassification.StorageClassSpecifier_));
        this.define(this.createSymbol("register", TypeClassification.StorageClassSpecifier_));
        this.define(this.createSymbol("static", TypeClassification.StorageClassSpecifier_));
        this.define(this.createSymbol("thread_local", TypeClassification.StorageClassSpecifier_));
        this.define(this.createSymbol("_Thread_local", TypeClassification.StorageClassSpecifier_));
        this.define(this.createSymbol("typedef", TypeClassification.StorageClassSpecifier_));

        this.define(this.createSymbol("enum", TypeClassification.EnumSpecifier_));

        this.define(this.createSymbol("struct", TypeClassification.StorageClassSpecifier_));
        this.define(this.createSymbol("union", TypeClassification.StorageClassSpecifier_));

        this.define(this.createSymbol("const", TypeClassification.TypeQualifier_));
        this.define(this.createSymbol("restrict", TypeClassification.TypeQualifier_));
        this.define(this.createSymbol("volatile", TypeClassification.TypeQualifier_));
        this.define(this.createSymbolMulti("_Atomic", TypeClassification.TypeQualifier_, TypeClassification.AtomicTypeSpecifier_));

        this.define(this.createSymbol("void", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("char", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("short", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("int", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("long", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("float", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("double", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("signed", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("unsigned", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("_BitInt", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("bool", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("_Bool", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("_Complex", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("_Decimal32", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("_Decimal64", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("_Decimal128", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__m128", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__m128d", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__m128i", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__extension__", TypeClassification.TypeSpecifier_));

        this.define(this.createSymbol("__builtin_va_list", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_has_attribute", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_speculation_safe_value", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_types_compatible_p", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_choose_expr", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_tgmath", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_constant_p", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_is_constant_evaluated", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_bit_cast", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_expect", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_expect_with_probability", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_trap", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_assoc_barrier", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_assume_aligned", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_LINE", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_FUNCTION", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_FILE", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin___clear_cache", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_prefetch", TypeClassification.Function_));
        this.define(this.createSymbol("__builtin_classify_type", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_extend_pointer", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_goacc_parlevel_id", TypeClassification.TypeSpecifier_));
        this.define(this.createSymbol("__builtin_goacc_parlevel_size", TypeClassification.TypeSpecifier_));

        this.define(this.createSymbol("inline", TypeClassification.FunctionSpecifier_));
        this.define(this.createSymbol("_Noreturn", TypeClassification.FunctionSpecifier_));
        this.define(this.createSymbol("__inline__", TypeClassification.FunctionSpecifier_));

        this.define(this.createSymbol("__cdecl", TypeClassification.FunctionSpecifier_)); // MS
        this.define(this.createSymbol("__clrcall", TypeClassification.FunctionSpecifier_)); // MS
        this.define(this.createSymbol("__stdcall", TypeClassification.FunctionSpecifier_)); // MS
        this.define(this.createSymbol("__fastcall", TypeClassification.FunctionSpecifier_)); // MS
        this.define(this.createSymbol("__thiscall", TypeClassification.FunctionSpecifier_)); // MS
        this.define(this.createSymbol("__vectorcall", TypeClassification.FunctionSpecifier_)); // MS

        this.define(this.createSymbol("_purecall", TypeClassification.TypeSpecifier_)); // gcc
        this.define(this.createSymbol("_purecall_handler", TypeClassification.TypeSpecifier_)); // gcc
        this.define(this.createSymbol("_onexit_t", TypeClassification.TypeSpecifier_)); // gcc
        this.define(this.createSymbol("_locale_t", TypeClassification.TypeSpecifier_)); // gcc
        this.define(this.createSymbol("_invalid_parameter_handler", TypeClassification.TypeSpecifier_)); // gcc
        this.define(this.createSymbol("__inline", TypeClassification.TypeSpecifier_)); // gcc

        this.define(this.createSymbol("__int8", TypeClassification.TypeSpecifier_)); // gcc
        this.define(this.createSymbol("__int16", TypeClassification.TypeSpecifier_)); // gcc
        this.define(this.createSymbol("__int32", TypeClassification.TypeSpecifier_)); // gcc
        this.define(this.createSymbol("__int64", TypeClassification.TypeSpecifier_)); // gcc
        this.define(this.createSymbol("__int128", TypeClassification.TypeSpecifier_)); // gcc
        this.define(this.createSymbol("_Float16", TypeClassification.TypeSpecifier_)); // gcc
        this.define(this.createSymbol("_Float32", TypeClassification.TypeSpecifier_)); // gcc
        this.define(this.createSymbol("_Float64", TypeClassification.TypeSpecifier_)); // gcc
        this.define(this.createSymbol("_Float128", TypeClassification.TypeSpecifier_)); // gcc
        this.define(this.createSymbol("__v8hf", TypeClassification.TypeSpecifier_)); // gcc
        this.define(this.createSymbol("__bf16", TypeClassification.TypeSpecifier_)); // gcc
        this.define(this.createSymbol("__v16bf", TypeClassification.TypeSpecifier_)); // gcc

        this.define(this.createSymbol("__declspec", TypeClassification.FunctionSpecifier_)); // MS
        this.define(this.createSymbol("__attribute__", TypeClassification.FunctionSpecifier_)); // GCC

        this.define(this.createSymbol("alignas", TypeClassification.AlignmentSpecifier_));
        this.define(this.createSymbol("align", TypeClassification.AlignmentSpecifier_));
    }

    private createSymbol(name: string, classification: TypeClassification): Symbol {
        const symbol = new Symbol();
        symbol.name = name;
        symbol.classification = new Set([classification]);
        return symbol;
    }

    private createSymbolMulti(name: string, ...classifications: TypeClassification[]): Symbol {
        const symbol = new Symbol();
        symbol.name = name;
        symbol.classification = new Set(classifications);
        return symbol;
    }

    public enterScope(newScope: Symbol): void {
        const current = this.scopeStack[this.scopeStack.length - 1];
        if (newScope === current) return;
        this.scopeStack.push(newScope);
    }

    public exitScope(): void {
        this.scopeStack.pop();
        if (this.scopeStack.length === 0) {
            throw new Error("Cannot exit global scope");
        }
    }

    public currentScope(): Symbol | null {
        if (this.scopeStack.length === 0) return null;
        return this.scopeStack[this.scopeStack.length - 1];
    }

    public define(symbol: Symbol): boolean {
        const currentScope = this.currentScope();
        if (!currentScope) return false;
        return this.defineInScope(currentScope, symbol);
    }

    public defineInScope(currentScope: Symbol, symbol: Symbol): boolean {
        if (currentScope.members.has(symbol.name)) {
            return false; // Symbol already defined in the current scope
        }
        symbol.parent = currentScope;
        currentScope.members.set(symbol.name, symbol);
        return true;
    }

    public resolve(name: string, startScope: Symbol | null = null): Symbol | null {
        if (startScope === null) {
            for (let i = this.scopeStack.length - 1; i >= 0; i--) {
                const scope = this.scopeStack[i];
                const symbol = scope.members.get(name);
                if (symbol) {
                    return symbol;
                }
            }
            return null; // Symbol not found
        } else {
            return startScope.members.get(name) || null;
        }
    }

    public toString(): string {
        let result = "";
        for (const scope of this.scopeStack) {
            for (const [key, value] of scope.members) {
                result += value.toString() + "\n";
            }
        }
        return result;
    }
}
