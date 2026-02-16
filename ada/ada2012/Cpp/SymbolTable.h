#pragma once
#include <vector>
#include <string>
#include <memory>
#include "Symbol.h"
#include "TypeClassification.h"

class SymbolTable {
public:
    SymbolTable();
    bool define(Symbol* symbol);
    Symbol* resolve(const std::string& name);
    Symbol* pushBlockScope();
    void popBlockScope();
    Symbol* currentScope();
    std::string toString() const;

private:
    std::vector<Symbol*> scopeStack;
    std::vector<std::unique_ptr<Symbol>> ownedSymbols;
    int blockCounter = 0;

    void defineType(const std::string& name, bool isComposite);
    void definePredefined(const std::string& name, TypeClassification tc);
    Symbol* createSymbol();
    void toStringHelper(std::string& sb, Symbol* scope, int depth) const;
    static std::string toLower(const std::string& s);
};
