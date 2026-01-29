import 'Symbol.dart';
import 'TypeClassification.dart';

class SymbolTable {
  final List<Symbol> _scopeStack = [];
  int _blockCounter = 0;

  SymbolTable() {
    var globalScope = _createSymbol("global", TypeClassification.global);
    _scopeStack.add(globalScope);

    define(_createSymbol("auto", TypeClassification.storageClassSpecifier));
    define(_createSymbol("constexpr", TypeClassification.storageClassSpecifier));
    define(_createSymbol("extern", TypeClassification.storageClassSpecifier));
    define(_createSymbol("register", TypeClassification.storageClassSpecifier));
    define(_createSymbol("static", TypeClassification.storageClassSpecifier));
    define(_createSymbol("thread_local", TypeClassification.storageClassSpecifier));
    define(_createSymbol("_Thread_local", TypeClassification.storageClassSpecifier));
    define(_createSymbol("typedef", TypeClassification.storageClassSpecifier));

    define(_createSymbol("enum", TypeClassification.enumSpecifier));

    define(_createSymbol("struct", TypeClassification.storageClassSpecifier));
    define(_createSymbol("union", TypeClassification.storageClassSpecifier));

    define(_createSymbol("const", TypeClassification.typeQualifier));
    define(_createSymbol("restrict", TypeClassification.typeQualifier));
    define(_createSymbol("volatile", TypeClassification.typeQualifier));
    define(_createSymbolMulti("_Atomic", [TypeClassification.typeQualifier, TypeClassification.atomicTypeSpecifier]));

    define(_createSymbol("void", TypeClassification.typeSpecifier));
    define(_createSymbol("char", TypeClassification.typeSpecifier));
    define(_createSymbol("short", TypeClassification.typeSpecifier));
    define(_createSymbol("int", TypeClassification.typeSpecifier));
    define(_createSymbol("long", TypeClassification.typeSpecifier));
    define(_createSymbol("float", TypeClassification.typeSpecifier));
    define(_createSymbol("double", TypeClassification.typeSpecifier));
    define(_createSymbol("signed", TypeClassification.typeSpecifier));
    define(_createSymbol("unsigned", TypeClassification.typeSpecifier));
    define(_createSymbol("_BitInt", TypeClassification.typeSpecifier));
    define(_createSymbol("bool", TypeClassification.typeSpecifier));
    define(_createSymbol("_Bool", TypeClassification.typeSpecifier));
    define(_createSymbol("_Complex", TypeClassification.typeSpecifier));
    define(_createSymbol("_Decimal32", TypeClassification.typeSpecifier));
    define(_createSymbol("_Decimal64", TypeClassification.typeSpecifier));
    define(_createSymbol("_Decimal128", TypeClassification.typeSpecifier));
    define(_createSymbol("__m128", TypeClassification.typeSpecifier));
    define(_createSymbol("__m128d", TypeClassification.typeSpecifier));
    define(_createSymbol("__m128i", TypeClassification.typeSpecifier));
    define(_createSymbol("__extension__", TypeClassification.typeSpecifier));

    define(_createSymbol("__builtin_va_list", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_has_attribute", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_speculation_safe_value", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_types_compatible_p", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_choose_expr", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_tgmath", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_constant_p", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_is_constant_evaluated", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_bit_cast", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_expect", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_expect_with_probability", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_trap", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_assoc_barrier", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_assume_aligned", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_LINE", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_FUNCTION", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_FILE", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin___clear_cache", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_prefetch", TypeClassification.function_));
    define(_createSymbol("__builtin_classify_type", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_extend_pointer", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_goacc_parlevel_id", TypeClassification.typeSpecifier));
    define(_createSymbol("__builtin_goacc_parlevel_size", TypeClassification.typeSpecifier));

    define(_createSymbol("inline", TypeClassification.functionSpecifier));
    define(_createSymbol("_Noreturn", TypeClassification.functionSpecifier));
    define(_createSymbol("__inline__", TypeClassification.functionSpecifier));

    define(_createSymbol("__cdecl", TypeClassification.functionSpecifier)); // MS
    define(_createSymbol("__clrcall", TypeClassification.functionSpecifier)); // MS
    define(_createSymbol("__stdcall", TypeClassification.functionSpecifier)); // MS
    define(_createSymbol("__fastcall", TypeClassification.functionSpecifier)); // MS
    define(_createSymbol("__thiscall", TypeClassification.functionSpecifier)); // MS
    define(_createSymbol("__vectorcall", TypeClassification.functionSpecifier)); // MS

    define(_createSymbol("_purecall", TypeClassification.typeSpecifier)); // gcc
    define(_createSymbol("_purecall_handler", TypeClassification.typeSpecifier)); // gcc
    define(_createSymbol("_onexit_t", TypeClassification.typeSpecifier)); // gcc
    define(_createSymbol("_locale_t", TypeClassification.typeSpecifier)); // gcc
    define(_createSymbol("_invalid_parameter_handler", TypeClassification.typeSpecifier)); // gcc
    define(_createSymbol("__inline", TypeClassification.typeSpecifier)); // gcc

    define(_createSymbol("__int8", TypeClassification.typeSpecifier)); // gcc
    define(_createSymbol("__int16", TypeClassification.typeSpecifier)); // gcc
    define(_createSymbol("__int32", TypeClassification.typeSpecifier)); // gcc
    define(_createSymbol("__int64", TypeClassification.typeSpecifier)); // gcc
    define(_createSymbol("__int128", TypeClassification.typeSpecifier)); // gcc
    define(_createSymbol("_Float16", TypeClassification.typeSpecifier)); // gcc
    define(_createSymbol("_Float32", TypeClassification.typeSpecifier)); // gcc
    define(_createSymbol("_Float64", TypeClassification.typeSpecifier)); // gcc
    define(_createSymbol("_Float128", TypeClassification.typeSpecifier)); // gcc
    define(_createSymbol("__v8hf", TypeClassification.typeSpecifier)); // gcc
    define(_createSymbol("__bf16", TypeClassification.typeSpecifier)); // gcc
    define(_createSymbol("__v16bf", TypeClassification.typeSpecifier)); // gcc

    define(_createSymbol("__declspec", TypeClassification.functionSpecifier)); // MS
    define(_createSymbol("__attribute__", TypeClassification.functionSpecifier)); // GCC

    define(_createSymbol("alignas", TypeClassification.alignmentSpecifier));
    define(_createSymbol("align", TypeClassification.alignmentSpecifier));
  }

  Symbol _createSymbol(String name, TypeClassification classification) {
    var symbol = Symbol();
    symbol.name = name;
    symbol.classification = {classification};
    symbol.predefined = true;
    return symbol;
  }

  Symbol _createSymbolMulti(String name, List<TypeClassification> classifications) {
    var symbol = Symbol();
    symbol.name = name;
    symbol.classification = Set.from(classifications);
    symbol.predefined = true;
    return symbol;
  }

  void enterScope(Symbol newScope) {
    if (_scopeStack.isNotEmpty && newScope == _scopeStack.last) return;
    _scopeStack.add(newScope);
  }

  void exitScope() {
    _scopeStack.removeLast();
    if (_scopeStack.isEmpty) {
      throw Exception("Cannot exit global scope");
    }
  }

  Symbol? currentScope() {
    if (_scopeStack.isEmpty) return null;
    return _scopeStack.last;
  }

  bool define(Symbol symbol) {
    var scope = currentScope();
    if (scope == null) return false;
    return defineInScope(scope, symbol);
  }

  bool defineInScope(Symbol scope, Symbol symbol) {
    if (scope.members.containsKey(symbol.name)) {
      return false; // Symbol already defined in the current scope
    }
    symbol.parent = scope;
    scope.members[symbol.name] = symbol;
    return true;
  }

  Symbol? resolve(String name, [Symbol? startScope]) {
    if (startScope == null) {
      // Iterate from innermost (top of stack) to outermost (bottom of stack)
      for (var i = _scopeStack.length - 1; i >= 0; i--) {
        var scope = _scopeStack[i];
        var symbol = scope.members[name];
        if (symbol != null) {
          return symbol;
        }
      }
      return null; // Symbol not found
    } else {
      return startScope.members[name];
    }
  }

  Symbol pushBlockScope() {
    var blockScope = Symbol();
    blockScope.name = "block${++_blockCounter}";
    blockScope.classification = {TypeClassification.block};
    blockScope.predefined = true;
    enterScope(blockScope);
    return blockScope;
  }

  void popBlockScope() {
    exitScope();
  }

  @override
  String toString() {
    var result = StringBuffer();
    if (_scopeStack.isNotEmpty) {
      _toStringHelper(result, _scopeStack[0], 0);
    }
    return result.toString();
  }

  void _toStringHelper(StringBuffer sb, Symbol scope, int depth) {
    var indent = '  ' * depth;
    for (var entry in scope.members.entries) {
      var sym = entry.value;
      if (!sym.predefined) {
        sb.writeln('$indent${sym.toString()}');
      }
      // Recursively print nested scopes
      if (sym.classification.contains(TypeClassification.block) ||
          sym.classification.contains(TypeClassification.function_)) {
        _toStringHelper(sb, sym, depth + 1);
      }
    }
  }
}
