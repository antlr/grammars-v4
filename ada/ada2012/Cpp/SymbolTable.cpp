#include "SymbolTable.h"
#include <algorithm>
#include <stdexcept>

std::string SymbolTable::toLower(const std::string& s) {
    std::string result = s;
    std::transform(result.begin(), result.end(), result.begin(), ::tolower);
    return result;
}

Symbol* SymbolTable::createSymbol() {
    auto sym = std::make_unique<Symbol>();
    Symbol* ptr = sym.get();
    ownedSymbols.push_back(std::move(sym));
    return ptr;
}

SymbolTable::SymbolTable() {
    auto globalScope = createSymbol();
    globalScope->name = "global";
    globalScope->classification.insert(TypeClassification::Global_);
    scopeStack.push_back(globalScope);

    for (auto& name : {"integer", "natural", "positive", "float", "long_float",
                       "character", "wide_character", "wide_wide_character", "boolean", "duration"}) {
        defineType(name, false);
    }
    for (auto& name : {"string", "wide_string", "wide_wide_string"}) {
        defineType(name, true);
    }
    for (auto& name : {"constraint_error", "program_error", "storage_error", "tasking_error", "numeric_error"}) {
        definePredefined(name, TypeClassification::ExceptionName_);
    }
    definePredefined("true", TypeClassification::EnumerationLiteral_);
    definePredefined("false", TypeClassification::EnumerationLiteral_);
}

void SymbolTable::defineType(const std::string& name, bool isComposite) {
    auto sym = createSymbol();
    sym->name = name;
    sym->classification.insert(TypeClassification::TypeName_);
    sym->predefined = true;
    sym->isComposite = isComposite;
    define(sym);
}

void SymbolTable::definePredefined(const std::string& name, TypeClassification tc) {
    auto sym = createSymbol();
    sym->name = name;
    sym->classification.insert(tc);
    sym->predefined = true;
    define(sym);
}

Symbol* SymbolTable::currentScope() {
    if (scopeStack.empty()) return nullptr;
    return scopeStack.back();
}

bool SymbolTable::define(Symbol* symbol) {
    auto current = currentScope();
    if (!current) return false;
    symbol->name = toLower(symbol->name);
    if (current->members.count(symbol->name)) return false;
    symbol->parent = current;
    current->members[symbol->name] = symbol;
    return true;
}

Symbol* SymbolTable::resolve(const std::string& name) {
    std::string lower = toLower(name);
    for (int i = static_cast<int>(scopeStack.size()) - 1; i >= 0; i--) {
        auto it = scopeStack[i]->members.find(lower);
        if (it != scopeStack[i]->members.end()) return it->second;
    }
    return nullptr;
}

Symbol* SymbolTable::pushBlockScope() {
    blockCounter++;
    auto blockScope = createSymbol();
    blockScope->name = "block" + std::to_string(blockCounter);
    blockScope->classification.insert(TypeClassification::Block_);
    blockScope->predefined = true;
    scopeStack.push_back(blockScope);
    return blockScope;
}

void SymbolTable::popBlockScope() {
    if (scopeStack.size() <= 1) throw std::runtime_error("SymbolTable: scope stack underflow");
    scopeStack.pop_back();
}

std::string SymbolTable::toString() const {
    std::string sb;
    if (!scopeStack.empty()) {
        toStringHelper(sb, scopeStack.front(), 0);
    }
    return sb;
}

void SymbolTable::toStringHelper(std::string& sb, Symbol* scope, int depth) const {
    std::string indent(depth * 2, ' ');
    for (auto& entry : scope->members) {
        auto sym = entry.second;
        if (!sym->predefined) {
            sb += indent + sym->toString() + "\n";
        }
        if (sym->classification.count(TypeClassification::Block_) ||
            sym->classification.count(TypeClassification::SubprogramName_) ||
            sym->classification.count(TypeClassification::PackageName_)) {
            toStringHelper(sb, sym, depth + 1);
        }
    }
}
