import java.util.*;

public class SymbolTable {
    private Deque<Symbol> scopeStack = new ArrayDeque<>();
    private int blockCounter = 0;

    public SymbolTable() {
        Symbol globalScope = new Symbol();
        globalScope.setName("global");
        globalScope.setClassification(new HashSet<>(Arrays.asList(TypeClassification.Global_)));
        scopeStack.push(globalScope);

        defineType("integer", false);
        defineType("natural", false);
        defineType("positive", false);
        defineType("float", false);
        defineType("long_float", false);
        defineType("character", false);
        defineType("wide_character", false);
        defineType("wide_wide_character", false);
        defineType("boolean", false);
        defineType("duration", false);

        defineType("string", true);
        defineType("wide_string", true);
        defineType("wide_wide_string", true);

        definePredefined("constraint_error", TypeClassification.ExceptionName_);
        definePredefined("program_error", TypeClassification.ExceptionName_);
        definePredefined("storage_error", TypeClassification.ExceptionName_);
        definePredefined("tasking_error", TypeClassification.ExceptionName_);
        definePredefined("numeric_error", TypeClassification.ExceptionName_);

        definePredefined("true", TypeClassification.EnumerationLiteral_);
        definePredefined("false", TypeClassification.EnumerationLiteral_);
    }

    private void defineType(String name, boolean isComposite) {
        Symbol sym = new Symbol();
        sym.setName(name);
        sym.setClassification(new HashSet<>(Arrays.asList(TypeClassification.TypeName_)));
        sym.setPredefined(true);
        sym.setComposite(isComposite);
        define(sym);
    }

    private void definePredefined(String name, TypeClassification tc) {
        Symbol sym = new Symbol();
        sym.setName(name);
        sym.setClassification(new HashSet<>(Arrays.asList(tc)));
        sym.setPredefined(true);
        define(sym);
    }

    public void enterScope(Symbol newScope) {
        Symbol current = scopeStack.peek();
        if (newScope == current) return;
        scopeStack.push(newScope);
    }

    public void exitScope() {
        scopeStack.pop();
        if (scopeStack.isEmpty())
            throw new RuntimeException("SymbolTable: scope stack underflow");
    }

    public Symbol currentScope() {
        if (scopeStack.isEmpty()) return null;
        return scopeStack.peek();
    }

    public boolean define(Symbol symbol) {
        Symbol currentScope = currentScope();
        return defineInScope(currentScope, symbol);
    }

    public boolean defineInScope(Symbol currentScope, Symbol symbol) {
        symbol.setName(symbol.getName().toLowerCase());
        if (currentScope.getMembers().containsKey(symbol.getName())) {
            return false;
        }
        symbol.setParent(currentScope);
        currentScope.getMembers().put(symbol.getName(), symbol);
        return true;
    }

    public Symbol resolve(String name) {
        return resolve(name, null);
    }

    public Symbol resolve(String name, Symbol startScope) {
        name = name.toLowerCase();
        if (startScope == null) {
            for (Symbol scope : scopeStack) {
                Symbol sym = scope.getMembers().get(name);
                if (sym != null) return sym;
            }
            return null;
        } else {
            return startScope.getMembers().get(name);
        }
    }

    public Symbol pushBlockScope() {
        Symbol blockScope = new Symbol();
        blockScope.setName("block" + (++blockCounter));
        blockScope.setClassification(new HashSet<>(Arrays.asList(TypeClassification.Block_)));
        blockScope.setPredefined(true);
        enterScope(blockScope);
        return blockScope;
    }

    public void popBlockScope() {
        exitScope();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        Symbol[] scopes = scopeStack.toArray(new Symbol[0]);
        if (scopes.length > 0) {
            toStringHelper(sb, scopes[scopes.length - 1], 0);
        }
        return sb.toString();
    }

    private void toStringHelper(StringBuilder sb, Symbol scope, int depth) {
        String indent = " ".repeat(depth * 2);
        for (Map.Entry<String, Symbol> entry : scope.getMembers().entrySet()) {
            Symbol sym = entry.getValue();
            if (!sym.isPredefined()) {
                sb.append(indent).append(sym.toString()).append("\n");
            }
            if (sym.getClassification().contains(TypeClassification.Block_) ||
                sym.getClassification().contains(TypeClassification.SubprogramName_) ||
                sym.getClassification().contains(TypeClassification.PackageName_)) {
                toStringHelper(sb, sym, depth + 1);
            }
        }
    }
}
