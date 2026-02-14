from Symbol import Symbol
from TypeClassification import TypeClassification

class SymbolTable:
    def __init__(self):
        self.scopeStack = []
        self.blockCounter = 0

        globalScope = self._createSymbol("global", TypeClassification.Global_)
        self.scopeStack.append(globalScope)

        self.define(self._createSymbol("auto", TypeClassification.StorageClassSpecifier_))
        self.define(self._createSymbol("constexpr", TypeClassification.StorageClassSpecifier_))
        self.define(self._createSymbol("extern", TypeClassification.StorageClassSpecifier_))
        self.define(self._createSymbol("register", TypeClassification.StorageClassSpecifier_))
        self.define(self._createSymbol("static", TypeClassification.StorageClassSpecifier_))
        self.define(self._createSymbol("thread_local", TypeClassification.StorageClassSpecifier_))
        self.define(self._createSymbol("_Thread_local", TypeClassification.StorageClassSpecifier_))
        self.define(self._createSymbol("typedef", TypeClassification.StorageClassSpecifier_))

        self.define(self._createSymbol("enum", TypeClassification.EnumSpecifier_))

        self.define(self._createSymbol("struct", TypeClassification.StorageClassSpecifier_))
        self.define(self._createSymbol("union", TypeClassification.StorageClassSpecifier_))

        self.define(self._createSymbol("const", TypeClassification.TypeQualifier_))
        self.define(self._createSymbol("restrict", TypeClassification.TypeQualifier_))
        self.define(self._createSymbol("volatile", TypeClassification.TypeQualifier_))
        self.define(self._createSymbolMulti("_Atomic", TypeClassification.TypeQualifier_, TypeClassification.AtomicTypeSpecifier_))

        self.define(self._createSymbol("void", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("char", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("short", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("int", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("long", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("float", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("double", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("signed", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("unsigned", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("_BitInt", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("bool", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("_Bool", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("_Complex", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("_Decimal32", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("_Decimal64", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("_Decimal128", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__m128", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__m128d", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__m128i", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__extension__", TypeClassification.TypeSpecifier_))

        self.define(self._createSymbol("__builtin_va_list", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_has_attribute", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_speculation_safe_value", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_types_compatible_p", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_choose_expr", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_tgmath", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_constant_p", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_is_constant_evaluated", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_bit_cast", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_expect", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_expect_with_probability", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_trap", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_assoc_barrier", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_assume_aligned", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_LINE", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_FUNCTION", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_FILE", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin___clear_cache", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_prefetch", TypeClassification.Function_))
        self.define(self._createSymbol("__builtin_classify_type", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_extend_pointer", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_goacc_parlevel_id", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__builtin_goacc_parlevel_size", TypeClassification.TypeSpecifier_))

        self.define(self._createSymbol("inline", TypeClassification.FunctionSpecifier_))
        self.define(self._createSymbol("_Noreturn", TypeClassification.FunctionSpecifier_))
        self.define(self._createSymbol("__inline__", TypeClassification.FunctionSpecifier_))

        self.define(self._createSymbol("__cdecl", TypeClassification.FunctionSpecifier_))
        self.define(self._createSymbol("__clrcall", TypeClassification.FunctionSpecifier_))
        self.define(self._createSymbol("__stdcall", TypeClassification.FunctionSpecifier_))
        self.define(self._createSymbol("__fastcall", TypeClassification.FunctionSpecifier_))
        self.define(self._createSymbol("__thiscall", TypeClassification.FunctionSpecifier_))
        self.define(self._createSymbol("__vectorcall", TypeClassification.FunctionSpecifier_))

        self.define(self._createSymbol("_purecall", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("_purecall_handler", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("_onexit_t", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("_locale_t", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("_invalid_parameter_handler", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__inline", TypeClassification.TypeSpecifier_))

        self.define(self._createSymbol("__int8", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__int16", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__int32", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__int64", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__int128", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("_Float16", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("_Float32", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("_Float64", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("_Float128", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__v8hf", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__bf16", TypeClassification.TypeSpecifier_))
        self.define(self._createSymbol("__v16bf", TypeClassification.TypeSpecifier_))

        self.define(self._createSymbol("__declspec", TypeClassification.FunctionSpecifier_))
        self.define(self._createSymbol("__attribute__", TypeClassification.FunctionSpecifier_))

        self.define(self._createSymbol("alignas", TypeClassification.AlignmentSpecifier_))
        self.define(self._createSymbol("align", TypeClassification.AlignmentSpecifier_))

    def _createSymbol(self, name, classification):
        symbol = Symbol()
        symbol.name = name
        symbol.classification = {classification}
        symbol.predefined = True
        return symbol

    def _createSymbolMulti(self, name, *classifications):
        symbol = Symbol()
        symbol.name = name
        symbol.classification = set(classifications)
        symbol.predefined = True
        return symbol

    def enterScope(self, newScope):
        if len(self.scopeStack) > 0 and newScope is self.scopeStack[-1]:
            return
        self.scopeStack.append(newScope)

    def exitScope(self):
        self.scopeStack.pop()
        if len(self.scopeStack) == 0:
            raise Exception("Cannot exit global scope")

    def currentScope(self):
        if len(self.scopeStack) == 0:
            return None
        return self.scopeStack[-1]

    def define(self, symbol):
        cs = self.currentScope()
        if cs is None:
            return False
        return self.defineInScope(cs, symbol)

    def defineInScope(self, currentScope, symbol):
        if symbol.name in currentScope.members:
            return False
        symbol.parent = currentScope
        currentScope.members[symbol.name] = symbol
        return True

    def resolve(self, name, startScope=None):
        if startScope is None:
            for i in range(len(self.scopeStack) - 1, -1, -1):
                scope = self.scopeStack[i]
                if name in scope.members:
                    return scope.members[name]
            return None
        else:
            return startScope.members.get(name, None)

    def pushBlockScope(self):
        self.blockCounter += 1
        blockScope = Symbol()
        blockScope.name = "block" + str(self.blockCounter)
        blockScope.classification = {TypeClassification.Block_}
        blockScope.predefined = True
        self.enterScope(blockScope)
        return blockScope

    def popBlockScope(self):
        self.exitScope()

    def __str__(self):
        result = ""
        if len(self.scopeStack) > 0:
            result = self._toStringHelper(self.scopeStack[0], 0)
        return result

    def _toStringHelper(self, scope, depth):
        result = ""
        indent = "  " * depth
        for key, sym in scope.members.items():
            if not sym.predefined:
                result += indent + str(sym) + "\n"
            if TypeClassification.Block_ in sym.classification or \
               TypeClassification.Function_ in sym.classification:
                result += self._toStringHelper(sym, depth + 1)
        return result
