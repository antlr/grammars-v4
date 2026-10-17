#include "CSharpSymbolTable.h"
#include <algorithm>

// ─── CSharpSymbolTable ────────────────────────────────────────────────────────

CSharpSymbolTable::CSharpSymbolTable()
{
    // Push the global scope
    scopeStack_.push_back(std::make_unique<CSharpScope>(CSharpScopeKind::Global, nullptr, "<global>"));
    globalScope_ = scopeStack_.back().get();
    populateBuiltins();
}

// ── Core predicates ─────────────────────────────────────────────────────────

bool CSharpSymbolTable::isTypeName(const std::string &name) const
{
    CSharpSymbol *sym = currentScope()->lookupChain(name);
    if (sym)
        return sym->kind == CSharpSymbolKind::Type
            || sym->kind == CSharpSymbolKind::TypeParameter
            || sym->kind == CSharpSymbolKind::Alias;
    return knownTypeNames_.count(name) > 0;
}

bool CSharpSymbolTable::isGenericType(const std::string &name, int arity) const
{
    auto it = genericArities_.find(name);
    if (it == genericArities_.end()) return false;
    return it->second.count(arity) > 0;
}

// ── Scope management ─────────────────────────────────────────────────────────

void CSharpSymbolTable::enterScope(CSharpScopeKind kind, const std::string &name)
{
    CSharpScope *parent = scopeStack_.back().get();
    scopeStack_.push_back(std::make_unique<CSharpScope>(kind, parent, name));
}

void CSharpSymbolTable::exitScope()
{
    if (scopeStack_.size() > 1)
        scopeStack_.pop_back();
}

// ── Declarations ─────────────────────────────────────────────────────────────

void CSharpSymbolTable::declareType(const std::string &name, CSharpTypeKind kind, int arity)
{
    CSharpSymbol sym;
    sym.name     = name;
    sym.kind     = CSharpSymbolKind::Type;
    sym.typeKind = kind;
    sym.arity    = arity;
    currentScope()->declare(sym);
    knownTypeNames_.insert(name);
    if (arity > 0)
        genericArities_[name].insert(arity);
}

void CSharpSymbolTable::declareTypeParam(const std::string &name)
{
    CSharpSymbol sym;
    sym.name = name;
    sym.kind = CSharpSymbolKind::TypeParameter;
    currentScope()->declare(sym);
    knownTypeNames_.insert(name);
}

void CSharpSymbolTable::declareVariable(const std::string &name, const std::string &typeRef)
{
    CSharpSymbol sym;
    sym.name    = name;
    sym.kind    = CSharpSymbolKind::Variable;
    sym.typeRef = typeRef;
    currentScope()->declare(sym);
}

void CSharpSymbolTable::declareAlias(const std::string &alias, const std::string &target)
{
    CSharpSymbol sym;
    sym.name   = alias;
    sym.kind   = CSharpSymbolKind::Alias;
    sym.target = target;
    currentScope()->declare(sym);
    knownTypeNames_.insert(alias);
}

void CSharpSymbolTable::importNamespace(const std::string &ns)
{
    CSharpSymbol sym;
    sym.name = ns;
    sym.kind = CSharpSymbolKind::Namespace;
    currentScope()->declare(sym);
}

// ── Pre-scan ─────────────────────────────────────────────────────────────────

void CSharpSymbolTable::preScan(antlr4::TokenStream *tokens)
{
    auto *bts = dynamic_cast<antlr4::BufferedTokenStream *>(tokens);
    if (bts) bts->fill();

    size_t n = static_cast<size_t>(tokens->size());
    for (size_t i = 0; i < n; i++)
    {
        antlr4::Token *tok = tokens->get(i);
        if (tok->getChannel() != 0) continue;

        const std::string &text = tok->getText();

        if (text == "class" || text == "struct"
            || text == "interface" || text == "enum")
        {
            size_t j = nextDefault(tokens, i + 1, n);
            while (j < n && (tokens->get(j)->getText() == "partial"
                             || tokens->get(j)->getText() == "ref"))
                j = nextDefault(tokens, j + 1, n);

            if (j < n && isIdentLike(tokens->get(j)->getText()))
            {
                std::string typeName = tokens->get(j)->getText();
                std::vector<std::string> typeParams;
                auto [arity, tp] = countTypeParams(tokens, j + 1, n);
                registerType(typeName, arity, tp);
            }
        }
        else if (text == "delegate")
        {
            size_t j = nextDefault(tokens, i + 1, n);
            while (j < n && (tokens->get(j)->getText() == "ref"
                             || tokens->get(j)->getText() == "readonly"))
                j = nextDefault(tokens, j + 1, n);
            if (j >= n) continue;

            // skip return type
            j = nextDefault(tokens, j + 1, n);
            if (j < n && tokens->get(j)->getText() == "<")
                j = skipAngled(tokens, j, n);
            while (j < n && (tokens->get(j)->getText() == "["
                             || tokens->get(j)->getText() == "]"
                             || tokens->get(j)->getText() == ","))
                j = nextDefault(tokens, j + 1, n);
            if (j < n && tokens->get(j)->getText() == "?")
                j = nextDefault(tokens, j + 1, n);

            if (j < n && isIdentLike(tokens->get(j)->getText()))
            {
                std::string typeName = tokens->get(j)->getText();
                auto [arity, tp] = countTypeParams(tokens, j + 1, n);
                registerType(typeName, arity, tp);
            }
        }
    }
}

// ── Private helpers ──────────────────────────────────────────────────────────

void CSharpSymbolTable::registerType(const std::string &name, int arity,
                                     const std::vector<std::string> &typeParams)
{
    if (!isReservedKeyword(name))
        knownTypeNames_.insert(name);
    if (arity > 0 && !isReservedKeyword(name))
        genericArities_[name].insert(arity);
    for (const auto &tp : typeParams)
        if (isIdentLike(tp) && !isReservedKeyword(tp))
            knownTypeNames_.insert(tp);
}

/*static*/ bool CSharpSymbolTable::isIdentLike(const std::string &t)
{
    if (t.empty()) return false;
    char c = t[0];
    return std::isalpha((unsigned char)c) || c == '_' || c == '@';
}

/*static*/ size_t CSharpSymbolTable::nextDefault(antlr4::TokenStream *tokens, size_t pos, size_t n)
{
    while (pos < n && tokens->get(pos)->getChannel() != 0)
        pos++;
    return pos;
}

/*static*/ size_t CSharpSymbolTable::skipAngled(antlr4::TokenStream *tokens, size_t pos, size_t n)
{
    int depth = 1;
    size_t i = nextDefault(tokens, pos + 1, n);
    while (i < n && depth > 0)
    {
        const std::string &t = tokens->get(i)->getText();
        if (t == "<") depth++;
        else if (t == ">") depth--;
        i = nextDefault(tokens, i + 1, n);
    }
    return i;
}

/*static*/ std::pair<int, std::vector<std::string>>
CSharpSymbolTable::countTypeParams(antlr4::TokenStream *tokens, size_t pos, size_t n)
{
    std::vector<std::string> names;
    size_t p = nextDefault(tokens, pos, n);
    if (p >= n || tokens->get(p)->getText() != "<")
        return {0, names};

    int depth = 1, arity = 1;
    size_t i = nextDefault(tokens, p + 1, n);
    while (i < n && depth > 0)
    {
        const std::string &t = tokens->get(i)->getText();
        if (t == "<")
            depth++;
        else if (t == ">")
            depth--;
        else if (t == "," && depth == 1)
            arity++;
        else if (depth == 1 && isIdentLike(t))
            names.push_back(t);
        i = nextDefault(tokens, i + 1, n);
    }
    return {depth == 0 ? arity : 0, names};
}

/*static*/ bool CSharpSymbolTable::isReservedKeyword(const std::string &name)
{
    static const std::unordered_set<std::string> kw = {
        "abstract", "as",        "base",       "bool",      "break",
        "byte",     "case",      "catch",       "char",      "checked",
        "class",    "const",     "continue",    "decimal",   "default",
        "delegate", "do",        "double",      "else",      "enum",
        "event",    "explicit",  "extern",      "false",     "finally",
        "fixed",    "float",     "for",         "foreach",   "goto",
        "if",       "implicit",  "in",          "int",       "interface",
        "internal", "is",        "lock",        "long",      "namespace",
        "new",      "null",      "object",      "operator",  "out",
        "override", "params",    "private",     "protected", "public",
        "readonly", "ref",       "return",      "sbyte",     "sealed",
        "short",    "sizeof",    "stackalloc",  "static",    "string",
        "struct",   "switch",    "this",        "throw",     "true",
        "try",      "typeof",    "uint",        "ulong",     "unchecked",
        "unsafe",   "ushort",    "using",       "virtual",   "void",
        "volatile", "while",
    };
    return kw.count(name) > 0;
}

void CSharpSymbolTable::populateBuiltins()
{
    // C# predefined types
    for (const auto &t : std::vector<std::string>{
        "bool", "byte", "sbyte", "char", "decimal", "double", "float",
        "int",  "uint", "long",  "ulong", "short",  "ushort",
        "object", "string", "void", "dynamic",
    })
        knownTypeNames_.insert(t);

    // Common BCL short names
    for (const auto &t : std::vector<std::string>{
        "Boolean", "Byte", "SByte", "Char", "Decimal", "Double", "Single",
        "Int16", "Int32", "Int64", "UInt16", "UInt32", "UInt64",
        "IntPtr", "UIntPtr", "Object", "String", "Void",
        "Guid", "DateTime", "DateTimeOffset", "TimeSpan", "Uri",
        "Exception", "Type", "Enum", "Delegate", "MulticastDelegate",
        "Attribute", "Math", "Convert", "Console", "Environment",
    })
        knownTypeNames_.insert(t);

    // Common generic BCL types
    struct GenEntry { const char *name; int arity; };
    static const GenEntry generics[] = {
        {"List",1},{"IList",1},{"IEnumerable",1},{"IEnumerator",1},
        {"ICollection",1},{"IReadOnlyList",1},{"IReadOnlyCollection",1},
        {"IReadOnlyDictionary",2},{"Dictionary",2},{"IDictionary",2},
        {"SortedDictionary",2},{"SortedList",2},{"HashSet",1},
        {"SortedSet",1},{"Queue",1},{"Stack",1},{"LinkedList",1},
        {"LinkedListNode",1},{"Func",1},{"Func",2},{"Func",3},{"Func",4},
        {"Action",1},{"Action",2},{"Action",3},{"Predicate",1},
        {"Comparison",1},{"Converter",2},{"Task",1},{"ValueTask",1},
        {"Nullable",1},{"IAsyncEnumerable",1},{"IAsyncEnumerator",1},
        {"Span",1},{"ReadOnlySpan",1},{"Memory",1},{"ReadOnlyMemory",1},
        {"KeyValuePair",2},{"Tuple",2},{"Tuple",3},{"Tuple",4},
        {"ImmutableArray",1},{"ImmutableList",1},{"ImmutableDictionary",2},
        {"ImmutableHashSet",1},{"Lazy",1},{"WeakReference",1},
        {"EventHandler",1},{"IEqualityComparer",1},{"IComparer",1},
        {"EqualityComparer",1},{"Comparer",1},{"ConcurrentDictionary",2},
        {"ConcurrentQueue",1},{"ConcurrentStack",1},{"ConcurrentBag",1},
    };
    for (const auto &g : generics)
    {
        knownTypeNames_.insert(g.name);
        genericArities_[g.name].insert(g.arity);
    }
}
