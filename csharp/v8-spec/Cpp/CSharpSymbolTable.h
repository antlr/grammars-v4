#pragma once
#include "antlr4-runtime.h"
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <memory>
#include <utility>

// ─── Kinds ──────────────────────────────────────────────────────────────────

enum class CSharpSymbolKind { Type, Variable, Namespace, TypeParameter, Alias };
enum class CSharpTypeKind   { Class, Struct, Interface, Enum, Delegate };
enum class CSharpScopeKind  { Global, Namespace, Type, Method, Block };

// ─── Symbol ─────────────────────────────────────────────────────────────────

struct CSharpSymbol
{
    std::string      name;
    CSharpSymbolKind kind     = CSharpSymbolKind::Type;
    CSharpTypeKind   typeKind = CSharpTypeKind::Class;  // valid when kind == Type
    std::string      typeRef;   // valid when kind == Variable
    std::string      target;    // valid when kind == Alias
    int              arity   = 0; // valid when kind == Type
};

// ─── Scope ──────────────────────────────────────────────────────────────────

class CSharpScope
{
public:
    CSharpScopeKind  kind;
    CSharpScope     *parent;
    std::string      name;

    CSharpScope(CSharpScopeKind k, CSharpScope *p, const std::string &n)
        : kind(k), parent(p), name(n) {}

    void declare(const CSharpSymbol &sym) { symbols_[sym.name] = sym; }

    CSharpSymbol *lookup(const std::string &n)
    {
        auto it = symbols_.find(n);
        return it != symbols_.end() ? &it->second : nullptr;
    }

    CSharpSymbol *lookupChain(const std::string &n)
    {
        if (CSharpSymbol *s = lookup(n)) return s;
        return parent ? parent->lookupChain(n) : nullptr;
    }

private:
    std::unordered_map<std::string, CSharpSymbol> symbols_;
};

// ─── Symbol Table ────────────────────────────────────────────────────────────

class CSharpSymbolTable
{
public:
    CSharpSymbolTable();

    CSharpScope *currentScope() const { return scopeStack_.back().get(); }
    CSharpScope *globalScope()  const { return globalScope_; }

    bool isTypeName(const std::string &name) const;
    bool isGenericType(const std::string &name, int arity) const;

    void enterScope(CSharpScopeKind kind, const std::string &name = "");
    void exitScope();

    void declareType(const std::string &name, CSharpTypeKind kind, int arity = 0);
    void declareTypeParam(const std::string &name);
    void declareVariable(const std::string &name, const std::string &typeRef);
    void declareAlias(const std::string &alias, const std::string &target);
    void importNamespace(const std::string &ns);

    void preScan(antlr4::TokenStream *tokens);

private:
    std::unordered_set<std::string>                              knownTypeNames_;
    std::unordered_map<std::string, std::unordered_set<int>>    genericArities_;
    std::vector<std::unique_ptr<CSharpScope>>                   scopeStack_;
    CSharpScope                                                 *globalScope_ = nullptr;

    void registerType(const std::string &name, int arity,
                      const std::vector<std::string> &typeParams);
    static bool   isReservedKeyword(const std::string &name);
    static bool   isIdentLike(const std::string &t);
    static size_t nextDefault(antlr4::TokenStream *tokens, size_t pos, size_t n);
    static size_t skipAngled(antlr4::TokenStream *tokens, size_t pos, size_t n);
    static std::pair<int, std::vector<std::string>> countTypeParams(
        antlr4::TokenStream *tokens, size_t pos, size_t n);
    void populateBuiltins();
};
