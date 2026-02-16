import { Symbol } from "./Symbol.js";
import { TypeClassification } from "./TypeClassification.js";

export class SymbolTable {
    constructor() {
        this.scopeStack = [];
        this.blockCounter = 0;

        const globalScope = this._createSymbol("global", TypeClassification.Global_);
        this.scopeStack.push(globalScope);

        this.define(this._createSymbol("auto", TypeClassification.StorageClassSpecifier_));
        this.define(this._createSymbol("constexpr", TypeClassification.StorageClassSpecifier_));
        this.define(this._createSymbol("extern", TypeClassification.StorageClassSpecifier_));
        this.define(this._createSymbol("register", TypeClassification.StorageClassSpecifier_));
        this.define(this._createSymbol("static", TypeClassification.StorageClassSpecifier_));
        this.define(this._createSymbol("thread_local", TypeClassification.StorageClassSpecifier_));
        this.define(this._createSymbol("_Thread_local", TypeClassification.StorageClassSpecifier_));
        this.define(this._createSymbol("typedef", TypeClassification.StorageClassSpecifier_));

        this.define(this._createSymbol("enum", TypeClassification.EnumSpecifier_));

        this.define(this._createSymbol("struct", TypeClassification.StorageClassSpecifier_));
        this.define(this._createSymbol("union", TypeClassification.StorageClassSpecifier_));

        this.define(this._createSymbol("const", TypeClassification.TypeQualifier_));
        this.define(this._createSymbol("restrict", TypeClassification.TypeQualifier_));
        this.define(this._createSymbol("volatile", TypeClassification.TypeQualifier_));
        this.define(this._createSymbolMulti("_Atomic", TypeClassification.TypeQualifier_, TypeClassification.AtomicTypeSpecifier_));

        this.define(this._createSymbol("void", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("char", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("short", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("int", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("long", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("float", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("double", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("signed", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("unsigned", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("_BitInt", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("bool", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("_Bool", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("_Complex", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("_Decimal32", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("_Decimal64", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("_Decimal128", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__m128", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__m128d", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__m128i", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__extension__", TypeClassification.TypeSpecifier_));

        this.define(this._createSymbol("__builtin_va_list", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_has_attribute", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_speculation_safe_value", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_types_compatible_p", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_choose_expr", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_tgmath", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_constant_p", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_is_constant_evaluated", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_bit_cast", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_expect", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_expect_with_probability", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_trap", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_assoc_barrier", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_assume_aligned", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_LINE", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_FUNCTION", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_FILE", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin___clear_cache", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_prefetch", TypeClassification.Function_));
        this.define(this._createSymbol("__builtin_classify_type", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_extend_pointer", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_goacc_parlevel_id", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__builtin_goacc_parlevel_size", TypeClassification.TypeSpecifier_));

        this.define(this._createSymbol("inline", TypeClassification.FunctionSpecifier_));
        this.define(this._createSymbol("_Noreturn", TypeClassification.FunctionSpecifier_));
        this.define(this._createSymbol("__inline__", TypeClassification.FunctionSpecifier_));

        this.define(this._createSymbol("__cdecl", TypeClassification.FunctionSpecifier_));
        this.define(this._createSymbol("__clrcall", TypeClassification.FunctionSpecifier_));
        this.define(this._createSymbol("__stdcall", TypeClassification.FunctionSpecifier_));
        this.define(this._createSymbol("__fastcall", TypeClassification.FunctionSpecifier_));
        this.define(this._createSymbol("__thiscall", TypeClassification.FunctionSpecifier_));
        this.define(this._createSymbol("__vectorcall", TypeClassification.FunctionSpecifier_));

        this.define(this._createSymbol("_purecall", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("_purecall_handler", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("_onexit_t", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("_locale_t", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("_invalid_parameter_handler", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__inline", TypeClassification.TypeSpecifier_));

        this.define(this._createSymbol("__int8", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__int16", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__int32", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__int64", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__int128", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("_Float16", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("_Float32", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("_Float64", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("_Float128", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__v8hf", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__bf16", TypeClassification.TypeSpecifier_));
        this.define(this._createSymbol("__v16bf", TypeClassification.TypeSpecifier_));

        this.define(this._createSymbol("__declspec", TypeClassification.FunctionSpecifier_));
        this.define(this._createSymbol("__attribute__", TypeClassification.FunctionSpecifier_));

        this.define(this._createSymbol("alignas", TypeClassification.AlignmentSpecifier_));
        this.define(this._createSymbol("align", TypeClassification.AlignmentSpecifier_));
    }

    _createSymbol(name, classification) {
        const symbol = new Symbol();
        symbol.name = name;
        symbol.classification = new Set([classification]);
        symbol.predefined = true;
        return symbol;
    }

    _createSymbolMulti(name, ...classifications) {
        const symbol = new Symbol();
        symbol.name = name;
        symbol.classification = new Set(classifications);
        symbol.predefined = true;
        return symbol;
    }

    enterScope(newScope) {
        const current = this.scopeStack[this.scopeStack.length - 1];
        if (newScope === current) return;
        this.scopeStack.push(newScope);
    }

    exitScope() {
        this.scopeStack.pop();
        if (this.scopeStack.length === 0) {
            throw new Error("Cannot exit global scope");
        }
    }

    currentScope() {
        if (this.scopeStack.length === 0) return null;
        return this.scopeStack[this.scopeStack.length - 1];
    }

    define(symbol) {
        const currentScope = this.currentScope();
        if (!currentScope) return false;
        return this.defineInScope(currentScope, symbol);
    }

    defineInScope(currentScope, symbol) {
        if (currentScope.members.has(symbol.name)) {
            return false;
        }
        symbol.parent = currentScope;
        currentScope.members.set(symbol.name, symbol);
        return true;
    }

    resolve(name, startScope = null) {
        if (startScope === null) {
            for (let i = this.scopeStack.length - 1; i >= 0; i--) {
                const scope = this.scopeStack[i];
                const symbol = scope.members.get(name);
                if (symbol) {
                    return symbol;
                }
            }
            return null;
        } else {
            return startScope.members.get(name) || null;
        }
    }

    pushBlockScope() {
        const blockScope = new Symbol();
        blockScope.name = "block" + (++this.blockCounter);
        blockScope.classification = new Set([TypeClassification.Block_]);
        blockScope.predefined = true;
        this.enterScope(blockScope);
        return blockScope;
    }

    popBlockScope() {
        this.exitScope();
    }

    toString() {
        let result = "";
        if (this.scopeStack.length > 0) {
            result = this._toStringHelper(this.scopeStack[0], 0);
        }
        return result;
    }

    _toStringHelper(scope, depth) {
        let result = "";
        const indent = "  ".repeat(depth);
        for (const [key, sym] of scope.members) {
            if (!sym.predefined) {
                result += indent + sym.toString() + "\n";
            }
            if (sym.classification.has(TypeClassification.Block_) ||
                sym.classification.has(TypeClassification.Function_)) {
                result += this._toStringHelper(sym, depth + 1);
            }
        }
        return result;
    }
}

export default SymbolTable;
