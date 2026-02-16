import 'Symbol.dart';
import 'TypeClassification.dart';

class SymbolTable {
  final List<Symbol> _scopeStack = [];
  int _blockCounter = 0;

  SymbolTable() {
    var globalScope = Symbol()
      ..name = "global"
      ..classification = {TypeClassification.global_};
    _scopeStack.add(globalScope);

    for (var name in ["integer", "natural", "positive", "float", "long_float",
        "character", "wide_character", "wide_wide_character", "boolean", "duration"]) {
      _defineType(name, false);
    }
    for (var name in ["string", "wide_string", "wide_wide_string"]) {
      _defineType(name, true);
    }
    for (var name in ["constraint_error", "program_error", "storage_error",
        "tasking_error", "numeric_error"]) {
      define(Symbol()..name = name..classification = {TypeClassification.exceptionName_}..predefined = true);
    }
    for (var name in ["true", "false"]) {
      define(Symbol()..name = name..classification = {TypeClassification.enumerationLiteral_}..predefined = true);
    }
  }

  void _defineType(String name, bool isComposite) {
    define(Symbol()..name = name..classification = {TypeClassification.typeName_}..predefined = true..isComposite = isComposite);
  }

  void enterScope(Symbol newScope) {
    if (_scopeStack.isNotEmpty && identical(newScope, _scopeStack.last)) return;
    _scopeStack.add(newScope);
  }

  void exitScope() {
    _scopeStack.removeLast();
    if (_scopeStack.isEmpty) throw StateError("SymbolTable: scope stack underflow");
  }

  Symbol? currentScope() {
    if (_scopeStack.isEmpty) return null;
    return _scopeStack.last;
  }

  bool define(Symbol symbol) {
    var current = currentScope();
    if (current == null) return false;
    return defineInScope(current, symbol);
  }

  bool defineInScope(Symbol currentScope, Symbol symbol) {
    symbol.name = symbol.name.toLowerCase();
    if (currentScope.members.containsKey(symbol.name)) return false;
    symbol.parent = currentScope;
    currentScope.members[symbol.name] = symbol;
    return true;
  }

  Symbol? resolve(String name, [Symbol? startScope]) {
    name = name.toLowerCase();
    if (startScope == null) {
      for (var i = _scopeStack.length - 1; i >= 0; i--) {
        var sym = _scopeStack[i].members[name];
        if (sym != null) return sym;
      }
      return null;
    } else {
      return startScope.members[name];
    }
  }

  Symbol pushBlockScope() {
    _blockCounter++;
    var blockScope = Symbol()
      ..name = "block$_blockCounter"
      ..classification = {TypeClassification.block_}
      ..predefined = true;
    enterScope(blockScope);
    return blockScope;
  }

  void popBlockScope() {
    exitScope();
  }

  @override
  String toString() {
    var sb = StringBuffer();
    if (_scopeStack.isNotEmpty) {
      _toStringHelper(sb, _scopeStack.first, 0);
    }
    return sb.toString();
  }

  void _toStringHelper(StringBuffer sb, Symbol scope, int depth) {
    var indent = '  ' * depth;
    for (var sym in scope.members.values) {
      if (!sym.predefined) {
        sb.writeln('$indent$sym');
      }
      if (sym.classification.contains(TypeClassification.block_) ||
          sym.classification.contains(TypeClassification.subprogramName_) ||
          sym.classification.contains(TypeClassification.packageName_)) {
        _toStringHelper(sb, sym, depth + 1);
      }
    }
  }
}
