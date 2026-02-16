from Symbol import Symbol
from TypeClassification import TypeClassification


class SymbolTable:
    def __init__(self):
        self._scope_stack = []
        self._block_counter = 0
        global_scope = Symbol()
        global_scope.name = "global"
        global_scope.classification = {TypeClassification.Global_}
        self._scope_stack.append(global_scope)

        # Predefined scalar types
        for name in ["integer", "natural", "positive", "float", "long_float",
                      "character", "wide_character", "wide_wide_character",
                      "boolean", "duration"]:
            self._define_type(name, False)

        # Predefined composite types
        for name in ["string", "wide_string", "wide_wide_string"]:
            self._define_type(name, True)

        # Predefined exceptions
        for name in ["constraint_error", "program_error", "storage_error",
                      "tasking_error", "numeric_error"]:
            sym = Symbol()
            sym.name = name
            sym.classification = {TypeClassification.ExceptionName_}
            sym.predefined = True
            self.define(sym)

        # Predefined enumeration literals
        for name in ["true", "false"]:
            sym = Symbol()
            sym.name = name
            sym.classification = {TypeClassification.EnumerationLiteral_}
            sym.predefined = True
            self.define(sym)

    def _define_type(self, name, is_composite):
        sym = Symbol()
        sym.name = name
        sym.classification = {TypeClassification.TypeName_}
        sym.predefined = True
        sym.is_composite = is_composite
        self.define(sym)

    def enter_scope(self, new_scope):
        if self._scope_stack and new_scope is self._scope_stack[-1]:
            return
        self._scope_stack.append(new_scope)

    def exit_scope(self):
        self._scope_stack.pop()
        if not self._scope_stack:
            raise RuntimeError("SymbolTable: scope stack underflow")

    def current_scope(self):
        if not self._scope_stack:
            return None
        return self._scope_stack[-1]

    def define(self, symbol):
        current = self.current_scope()
        return self.define_in_scope(current, symbol)

    def define_in_scope(self, current_scope, symbol):
        symbol.name = symbol.name.lower()
        if symbol.name in current_scope.members:
            return False
        symbol.parent = current_scope
        current_scope.members[symbol.name] = symbol
        return True

    def resolve(self, name, start_scope=None):
        name = name.lower()
        if start_scope is None:
            for scope in reversed(self._scope_stack):
                if name in scope.members:
                    return scope.members[name]
            return None
        else:
            return start_scope.members.get(name)

    def push_block_scope(self):
        self._block_counter += 1
        block_scope = Symbol()
        block_scope.name = "block" + str(self._block_counter)
        block_scope.classification = {TypeClassification.Block_}
        block_scope.predefined = True
        self.enter_scope(block_scope)
        return block_scope

    def pop_block_scope(self):
        self.exit_scope()

    def __str__(self):
        if self._scope_stack:
            return self._to_string_helper(self._scope_stack[0], 0)
        return ""

    def _to_string_helper(self, scope, depth):
        result = ""
        indent = "  " * depth
        for sym in scope.members.values():
            if not sym.predefined:
                result += indent + str(sym) + "\n"
            if (TypeClassification.Block_ in sym.classification or
                TypeClassification.SubprogramName_ in sym.classification or
                TypeClassification.PackageName_ in sym.classification):
                result += self._to_string_helper(sym, depth + 1)
        return result
