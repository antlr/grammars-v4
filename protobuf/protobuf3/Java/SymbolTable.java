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
	Symbol parent = scopeStack.peek();
	parent.getMembers().put(newScope.getName(), newScope);
	newScope.setParent(parent);
	scopeStack.push(newScope);
    }

    public void exitScope() {
	Symbol current = scopeStack.peek();
	current.setParent(null);
	scopeStack.pop();
    }

    public Symbol currentScope() {
	return scopeStack.peek();
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

    public Symbol resolve(String name) {
	for (Symbol scope : scopeStack) {
	    Map<String, Symbol> members = scope.getMembers();
	    if (members.containsKey(name)) {
		return members.get(name);
	    }
	}
	return null; // Symbol not found
    }
}
