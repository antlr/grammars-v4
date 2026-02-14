#pragma once

#include "Symbol.h"
#include "TypeClassification.h"
#include <stack>
#include <vector>
#include <memory>
#include <string>
#include <sstream>
#include <initializer_list>

class SymbolTable {
public:
    SymbolTable();

    void enterScope(std::shared_ptr<Symbol> newScope);
    void exitScope();
    std::shared_ptr<Symbol> currentScope() const;

    bool define(std::shared_ptr<Symbol> symbol);
    bool defineInScope(std::shared_ptr<Symbol> currentScope, std::shared_ptr<Symbol> symbol);

    std::shared_ptr<Symbol> resolve(const std::string& name, std::shared_ptr<Symbol> startScope = nullptr) const;

    std::shared_ptr<Symbol> pushBlockScope();
    void popBlockScope();

    std::string toString() const;

private:
    static std::shared_ptr<Symbol> createSymbol(const std::string& name, std::initializer_list<TypeClassification> classifications);

    void toStringHelper(std::ostringstream& sb, const std::shared_ptr<Symbol>& scope, int depth) const;

    // Use a vector as a stack so we can iterate from bottom to top
    std::vector<std::shared_ptr<Symbol>> scopeStack_;
    int blockCounter_ = 0;
};
