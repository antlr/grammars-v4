import java.util.Deque;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;

public class SymbolTable {
    private Deque<Symbol> scopeStack = new LinkedList<>();

    // Note: predeclared identifiers here.

    public SymbolTable() {
        Symbol globalScope = new Symbol();
        globalScope.setName("global");
        globalScope.setClassification(TypeClassification.Package_);
        scopeStack.push(globalScope);
    }

    public void enterScope(Symbol newScope) {
        var current = scopeStack.peek();
        if (newScope == current) return;
        scopeStack.push(newScope);
    }

    public void exitScope() {
        Symbol current = scopeStack.peek();
        scopeStack.pop();
    }

    public Symbol currentScope() {
        var current_scope = scopeStack.peek();
        return current_scope;
    }

    public boolean define(Symbol symbol) {
        Symbol currentScope = scopeStack.peek();
        return this.defineInScope(currentScope, symbol);
    }

    public boolean defineInScope(Symbol currentScope, Symbol symbol) {
        Map<String, Symbol> members = currentScope.getMembers();
        if (members.containsKey(symbol.getName())) {
            return false; // Symbol already defined in the current scope
        }
        symbol.setParent(currentScope);
        members.put(symbol.getName(), symbol);
        return true;
    }

    public Symbol resolve(String name, Symbol start_scope) {
        if (start_scope == null)
        {
            for (Symbol scope : scopeStack) {
                Map<String, Symbol> members = scope.getMembers();
                if (members.containsKey(name)) {
                    return members.get(name);
                }
            }
            return null; // Symbol not found
        }
        else
        {
            Map<String, Symbol> members = start_scope.getMembers();
            if (members.containsKey(name)) {
                return members.get(name);
            }
            return null; // Symbol not found
        }
    }
}
