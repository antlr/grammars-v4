#include "SymbolTable.h"
#include <stdexcept>

std::shared_ptr<Symbol> SymbolTable::createSymbol(const std::string& name, std::initializer_list<TypeClassification> classifications) {
    auto symbol = std::make_shared<Symbol>();
    symbol->setName(name);
    std::unordered_set<TypeClassification> classSet(classifications.begin(), classifications.end());
    symbol->setClassification(classSet);
    symbol->setPredefined(true);
    return symbol;
}

SymbolTable::SymbolTable() {
    auto globalScope = createSymbol("global", {TypeClassification::Global_});
    scopeStack_.push_back(globalScope);

    define(createSymbol("auto", {TypeClassification::StorageClassSpecifier_}));
    define(createSymbol("constexpr", {TypeClassification::StorageClassSpecifier_}));
    define(createSymbol("extern", {TypeClassification::StorageClassSpecifier_}));
    define(createSymbol("register", {TypeClassification::StorageClassSpecifier_}));
    define(createSymbol("static", {TypeClassification::StorageClassSpecifier_}));
    define(createSymbol("thread_local", {TypeClassification::StorageClassSpecifier_}));
    define(createSymbol("_Thread_local", {TypeClassification::StorageClassSpecifier_}));
    define(createSymbol("typedef", {TypeClassification::StorageClassSpecifier_}));

    define(createSymbol("enum", {TypeClassification::EnumSpecifier_}));

    define(createSymbol("struct", {TypeClassification::StorageClassSpecifier_}));
    define(createSymbol("union", {TypeClassification::StorageClassSpecifier_}));

    define(createSymbol("const", {TypeClassification::TypeQualifier_}));
    define(createSymbol("restrict", {TypeClassification::TypeQualifier_}));
    define(createSymbol("volatile", {TypeClassification::TypeQualifier_}));
    define(createSymbol("_Atomic", {TypeClassification::TypeQualifier_, TypeClassification::AtomicTypeSpecifier_}));

    define(createSymbol("void", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("char", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("short", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("int", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("long", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("float", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("double", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("signed", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("unsigned", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("_BitInt", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("bool", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("_Bool", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("_Complex", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("_Decimal32", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("_Decimal64", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("_Decimal128", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__m128", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__m128d", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__m128i", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__extension__", {TypeClassification::TypeSpecifier_}));

    define(createSymbol("__builtin_va_list", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_has_attribute", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_speculation_safe_value", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_types_compatible_p", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_choose_expr", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_tgmath", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_constant_p", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_is_constant_evaluated", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_bit_cast", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_expect", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_expect_with_probability", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_trap", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_assoc_barrier", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_assume_aligned", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_LINE", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_FUNCTION", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_FILE", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin___clear_cache", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_prefetch", {TypeClassification::Function_}));
    define(createSymbol("__builtin_classify_type", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_extend_pointer", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_goacc_parlevel_id", {TypeClassification::TypeSpecifier_}));
    define(createSymbol("__builtin_goacc_parlevel_size", {TypeClassification::TypeSpecifier_}));

    define(createSymbol("inline", {TypeClassification::FunctionSpecifier_}));
    define(createSymbol("_Noreturn", {TypeClassification::FunctionSpecifier_}));
    define(createSymbol("__inline__", {TypeClassification::FunctionSpecifier_}));

    define(createSymbol("__cdecl", {TypeClassification::FunctionSpecifier_}));     // MS
    define(createSymbol("__clrcall", {TypeClassification::FunctionSpecifier_}));    // MS
    define(createSymbol("__stdcall", {TypeClassification::FunctionSpecifier_}));    // MS
    define(createSymbol("__fastcall", {TypeClassification::FunctionSpecifier_}));   // MS
    define(createSymbol("__thiscall", {TypeClassification::FunctionSpecifier_}));   // MS
    define(createSymbol("__vectorcall", {TypeClassification::FunctionSpecifier_})); // MS

    define(createSymbol("_purecall", {TypeClassification::TypeSpecifier_}));                // gcc
    define(createSymbol("_purecall_handler", {TypeClassification::TypeSpecifier_}));        // gcc
    define(createSymbol("_onexit_t", {TypeClassification::TypeSpecifier_}));                // gcc
    define(createSymbol("_locale_t", {TypeClassification::TypeSpecifier_}));                // gcc
    define(createSymbol("_invalid_parameter_handler", {TypeClassification::TypeSpecifier_})); // gcc
    define(createSymbol("__inline", {TypeClassification::TypeSpecifier_}));                 // gcc

    define(createSymbol("__int8", {TypeClassification::TypeSpecifier_}));    // gcc
    define(createSymbol("__int16", {TypeClassification::TypeSpecifier_}));   // gcc
    define(createSymbol("__int32", {TypeClassification::TypeSpecifier_}));   // gcc
    define(createSymbol("__int64", {TypeClassification::TypeSpecifier_}));   // gcc
    define(createSymbol("__int128", {TypeClassification::TypeSpecifier_}));  // gcc
    define(createSymbol("_Float16", {TypeClassification::TypeSpecifier_}));  // gcc
    define(createSymbol("_Float32", {TypeClassification::TypeSpecifier_}));  // gcc
    define(createSymbol("_Float64", {TypeClassification::TypeSpecifier_}));  // gcc
    define(createSymbol("_Float128", {TypeClassification::TypeSpecifier_})); // gcc
    define(createSymbol("__v8hf", {TypeClassification::TypeSpecifier_}));    // gcc
    define(createSymbol("__bf16", {TypeClassification::TypeSpecifier_}));    // gcc
    define(createSymbol("__v16bf", {TypeClassification::TypeSpecifier_}));   // gcc

    define(createSymbol("__declspec", {TypeClassification::FunctionSpecifier_}));    // MS
    define(createSymbol("__attribute__", {TypeClassification::FunctionSpecifier_})); // GCC

    define(createSymbol("alignas", {TypeClassification::AlignmentSpecifier_}));
    define(createSymbol("align", {TypeClassification::AlignmentSpecifier_}));
}

void SymbolTable::enterScope(std::shared_ptr<Symbol> newScope) {
    auto current = scopeStack_.back();
    if (newScope == current) return;
    scopeStack_.push_back(newScope);
}

void SymbolTable::exitScope() {
    scopeStack_.pop_back();
    if (scopeStack_.empty()) {
        throw std::runtime_error("Cannot exit global scope");
    }
}

std::shared_ptr<Symbol> SymbolTable::currentScope() const {
    if (scopeStack_.empty()) return nullptr;
    return scopeStack_.back();
}

bool SymbolTable::define(std::shared_ptr<Symbol> symbol) {
    auto cs = currentScope();
    return defineInScope(cs, symbol);
}

bool SymbolTable::defineInScope(std::shared_ptr<Symbol> cs, std::shared_ptr<Symbol> symbol) {
    if (cs->getMembers().count(symbol->getName())) {
        return false; // Symbol already defined in the current scope
    }
    symbol->setParent(cs.get());
    cs->getMembers()[symbol->getName()] = symbol;
    return true;
}

std::shared_ptr<Symbol> SymbolTable::resolve(const std::string& name, std::shared_ptr<Symbol> startScope) const {
    if (!startScope) {
        // Iterate from innermost (top of stack) to outermost (bottom of stack)
        for (int i = static_cast<int>(scopeStack_.size()) - 1; i >= 0; i--) {
            auto& scope = scopeStack_[i];
            auto it = scope->getMembers().find(name);
            if (it != scope->getMembers().end()) {
                return it->second;
            }
        }
        return nullptr; // Symbol not found
    } else {
        auto it = startScope->getMembers().find(name);
        if (it != startScope->getMembers().end()) {
            return it->second;
        }
        return nullptr;
    }
}

std::shared_ptr<Symbol> SymbolTable::pushBlockScope() {
    auto blockScope = std::make_shared<Symbol>();
    blockScope->setName("block" + std::to_string(++blockCounter_));
    std::unordered_set<TypeClassification> classSet;
    classSet.insert(TypeClassification::Block_);
    blockScope->setClassification(classSet);
    blockScope->setPredefined(true);
    enterScope(blockScope);
    return blockScope;
}

void SymbolTable::popBlockScope() {
    exitScope();
}

std::string SymbolTable::toString() const {
    std::ostringstream sb;
    toStringHelper(sb, scopeStack_[0], 0);
    return sb.str();
}

void SymbolTable::toStringHelper(std::ostringstream& sb, const std::shared_ptr<Symbol>& scope, int depth) const {
    std::string indent(depth * 2, ' ');
    for (auto& [key, sym] : scope->getMembers()) {
        if (!sym->isPredefined()) {
            sb << indent << sym->toString() << "\n";
        }
        // Recursively print nested scopes
        if (sym->getClassification().count(TypeClassification::Block_) ||
            sym->getClassification().count(TypeClassification::Function_)) {
            toStringHelper(sb, sym, depth + 1);
        }
    }
}
