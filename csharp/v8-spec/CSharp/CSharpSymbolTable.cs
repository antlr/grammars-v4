using System;
using System.Collections.Generic;
using Antlr4.Runtime;

// ─── Kinds ─────────────────────────────────────────────────────────────────

public enum CSharpSymbolKind { Type, Variable, Namespace, TypeParameter, Alias }
public enum CSharpTypeKind   { Class, Struct, Interface, Enum, Delegate }
public enum CSharpScopeKind  { Global, Namespace, Type, Method, Block }

// ─── Symbols ───────────────────────────────────────────────────────────────

public abstract class CSharpSymbol
{
    public string           Name { get; }
    public CSharpSymbolKind Kind { get; }
    protected CSharpSymbol(string name, CSharpSymbolKind kind) { Name = name; Kind = kind; }
}

public sealed class CSharpTypeSymbol : CSharpSymbol
{
    public CSharpTypeKind TypeKind { get; }
    public int            Arity    { get; }
    public CSharpTypeSymbol(string name, CSharpTypeKind kind, int arity = 0)
        : base(name, CSharpSymbolKind.Type) { TypeKind = kind; Arity = arity; }
}

public sealed class CSharpVariableSymbol : CSharpSymbol
{
    public string TypeRef { get; }
    public CSharpVariableSymbol(string name, string typeRef)
        : base(name, CSharpSymbolKind.Variable) { TypeRef = typeRef; }
}

public sealed class CSharpAliasSymbol : CSharpSymbol
{
    public string TargetName { get; }
    public CSharpAliasSymbol(string alias, string target)
        : base(alias, CSharpSymbolKind.Alias) { TargetName = target; }
}

public sealed class CSharpTypeParamSymbol : CSharpSymbol
{
    public CSharpTypeParamSymbol(string name)
        : base(name, CSharpSymbolKind.TypeParameter) { }
}

public sealed class CSharpNamespaceSymbol : CSharpSymbol
{
    public CSharpNamespaceSymbol(string name)
        : base(name, CSharpSymbolKind.Namespace) { }
}

// ─── Scope ─────────────────────────────────────────────────────────────────

public sealed class CSharpScope
{
    public CSharpScopeKind Kind   { get; }
    public CSharpScope     Parent { get; }   // null only for GlobalScope
    public string          Name   { get; }   // may be null; use "" for anonymous scopes

    private readonly Dictionary<string, CSharpSymbol> _symbols =
        new Dictionary<string, CSharpSymbol>(StringComparer.Ordinal);

    public CSharpScope(CSharpScopeKind kind, CSharpScope parent, string name)
    { Kind = kind; Parent = parent; Name = name; }

    public void         Declare(CSharpSymbol s) => _symbols[s.Name] = s;
    public CSharpSymbol Lookup(string name)
    {
        CSharpSymbol s;
        return _symbols.TryGetValue(name, out s) ? s : null;
    }

    /// Walk the scope chain from innermost to outermost.
    public CSharpSymbol LookupChain(string name)
    {
        CSharpSymbol s = Lookup(name);
        if (s != null) return s;
        if (Parent != null) return Parent.LookupChain(name);
        return null;
    }
}

// ─── Symbol Table ──────────────────────────────────────────────────────────

/// <summary>
/// Single-file symbol table used by the C# parser to disambiguate casts vs.
/// parenthesised expressions, generic type arguments vs. less-than, and similar
/// context-sensitive constructs.
///
/// Usage:
///   1. Call PreScan(tokenStream) before the main parse to register all
///      type declarations in the file (handles forward references).
///   2. Grammar actions call DeclareType / DeclareTypeParam / DeclareAlias /
///      ImportNamespace / DeclareVariable and EnterScope / ExitScope as the
///      parser walks the input.
///   3. Grammar semantic predicates call IsTypeName(name) to decide whether a
///      parenthesised identifier should be parsed as a cast or an expression.
/// </summary>
public sealed class CSharpSymbolTable
{
    // Flat O(1) set of all names that are known types.  Entries are never
    // removed — the set only grows (conservative).  When a variable shadows a
    // type, IsTypeName() resolves via the scope chain first.
    private readonly HashSet<string> _knownTypeNames =
        new HashSet<string>(StringComparer.Ordinal);

    // Generic arity table: type name → set of known arities.
    // e.g. "Dictionary" → {2}, "Func" → {1, 2, 3, …}
    private readonly Dictionary<string, HashSet<int>> _genericArities =
        new Dictionary<string, HashSet<int>>(StringComparer.Ordinal);

    private readonly Stack<CSharpScope> _scopeStack = new Stack<CSharpScope>();

    public CSharpScope GlobalScope  { get; private set; }
    public CSharpScope CurrentScope { get { return _scopeStack.Peek(); } }

    public CSharpSymbolTable()
    {
        GlobalScope = new CSharpScope(CSharpScopeKind.Global, null, "<global>");
        _scopeStack.Push(GlobalScope);
        PopulateBuiltinTypes();
    }

    // ── Core predicates ───────────────────────────────────────────────────

    /// <summary>
    /// Returns true when <paramref name="name"/> is known to be a type in the
    /// current scope chain or in the global type-name set.  If the name is
    /// declared as a <em>variable</em> in the current scope chain it returns
    /// false even if the same name also designates a type (variable shadows type).
    /// Unknown names return false (conservative: prefer parenthesised over cast).
    /// </summary>
    public bool IsTypeName(string name)
    {
        // Scope-chain lookup: a variable declaration shadows the type.
        CSharpSymbol sym = CurrentScope.LookupChain(name);
        if (sym != null)
            return sym.Kind == CSharpSymbolKind.Type
                || sym.Kind == CSharpSymbolKind.TypeParameter
                || sym.Kind == CSharpSymbolKind.Alias;

        // Fall back to flat set (populated by PreScan + built-ins).
        return _knownTypeNames.Contains(name);
    }

    public bool IsGenericType(string name, int arity)
    {
        HashSet<int> set;
        return _genericArities.TryGetValue(name, out set) && set.Contains(arity);
    }

    // ── Scope management ──────────────────────────────────────────────────

    public void EnterScope(CSharpScopeKind kind, string name = "")
    {
        _scopeStack.Push(new CSharpScope(kind, _scopeStack.Peek(), name));
    }

    public void ExitScope()
    {
        if (_scopeStack.Count > 1)
            _scopeStack.Pop();
    }

    // ── Declarations ──────────────────────────────────────────────────────

    public void DeclareType(string name, CSharpTypeKind kind, int arity = 0)
    {
        CurrentScope.Declare(new CSharpTypeSymbol(name, kind, arity));
        _knownTypeNames.Add(name);
        if (arity > 0)
        {
            HashSet<int> ar;
            if (!_genericArities.TryGetValue(name, out ar))
                _genericArities[name] = ar = new HashSet<int>();
            ar.Add(arity);
        }
    }

    public void DeclareTypeParam(string name)
    {
        CurrentScope.Declare(new CSharpTypeParamSymbol(name));
        _knownTypeNames.Add(name);
    }

    public void DeclareVariable(string name, string typeRef)
        => CurrentScope.Declare(new CSharpVariableSymbol(name, typeRef));

    public void DeclareAlias(string alias, string targetName)
    {
        CurrentScope.Declare(new CSharpAliasSymbol(alias, targetName));
        _knownTypeNames.Add(alias);
    }

    public void ImportNamespace(string namespaceName)
        => CurrentScope.Declare(new CSharpNamespaceSymbol(namespaceName));

    // ── Pre-scan ──────────────────────────────────────────────────────────

    /// <summary>
    /// Fast linear scan over the full token stream that registers every type
    /// declaration (class / struct / interface / enum / delegate) and all
    /// type-parameter names found in the file.  Call this once, before the
    /// main parse, so that forward references work correctly when the
    /// <c>cast_expression</c> predicate is evaluated.
    /// </summary>
    public void PreScan(ITokenStream tokens)
    {
        // Ensure the whole stream is buffered.
        if (tokens is BufferedTokenStream bts)
            bts.Fill();

        int n = tokens.Size;
        for (int i = 0; i < n; i++)
        {
            IToken tok = tokens.Get(i);

            // Skip hidden-channel tokens (whitespace, comments, directives).
            // Channel 0 is the ANTLR4 default (on-channel) token channel.
            if (tok.Channel != 0)
                continue;

            switch (tok.Text)
            {
                case "class":
                case "struct":
                case "interface":
                case "enum":
                {
                    // Optional modifiers before the keyword have already been
                    // consumed (or come before it).  After the keyword, skip
                    // any 'partial' or 'ref' tokens that may precede the name.
                    int j = NextDefaultToken(tokens, i + 1, n);
                    while (j < n && (tokens.Get(j).Text == "partial" || tokens.Get(j).Text == "ref"))
                        j = NextDefaultToken(tokens, j + 1, n);

                    if (j < n && IsIdentLike(tokens.Get(j).Text))
                    {
                        string typeName = tokens.Get(j).Text;
                        int arity = CountTypeParams(tokens, j + 1, n,
                                                   out List<string> typeParams);
                        RegisterType(typeName, arity, typeParams);
                    }
                    break;
                }

                case "delegate":
                {
                    // Pattern: delegate [ref [readonly]] <return-type> <name>(<params>);
                    // Strategy: skip the return type (one or more tokens) and grab the
                    // first identifier that is followed by '(' or '<'.
                    int j = NextDefaultToken(tokens, i + 1, n);

                    // Skip optional 'ref' / 'ref readonly'
                    while (j < n && (tokens.Get(j).Text == "ref" || tokens.Get(j).Text == "readonly"))
                        j = NextDefaultToken(tokens, j + 1, n);

                    if (j >= n) break;

                    // Skip return type token (void / keyword / simple name)
                    j = NextDefaultToken(tokens, j + 1, n);

                    // Skip generic args of return type, e.g. Task<int>
                    if (j < n && tokens.Get(j).Text == "<")
                        j = SkipAngledBlock(tokens, j, n);

                    // Skip array rank specifiers on return type, e.g. int[]
                    while (j < n && (tokens.Get(j).Text == "[" || tokens.Get(j).Text == "]" || tokens.Get(j).Text == ","))
                        j = NextDefaultToken(tokens, j + 1, n);

                    // Skip nullable '?' on return type
                    if (j < n && tokens.Get(j).Text == "?")
                        j = NextDefaultToken(tokens, j + 1, n);

                    if (j < n && IsIdentLike(tokens.Get(j).Text))
                    {
                        string typeName = tokens.Get(j).Text;
                        int arity = CountTypeParams(tokens, j + 1, n,
                                                   out List<string> typeParams);
                        RegisterType(typeName, arity, typeParams);
                    }
                    break;
                }
            }
        }
    }

    // ── Private helpers ───────────────────────────────────────────────────

    private void RegisterType(string typeName, int arity, List<string> typeParams)
    {
        if (!IsReservedKeyword(typeName))
            _knownTypeNames.Add(typeName);

        if (arity > 0 && !IsReservedKeyword(typeName))
        {
            HashSet<int> ar;
            if (!_genericArities.TryGetValue(typeName, out ar))
                _genericArities[typeName] = ar = new HashSet<int>();
            ar.Add(arity);
        }

        foreach (string tp in typeParams)
            if (IsIdentLike(tp) && !IsReservedKeyword(tp))
                _knownTypeNames.Add(tp);
    }

    /// Advance to the next default-channel token at or after <paramref name="pos"/>.
    private static int NextDefaultToken(ITokenStream tokens, int pos, int n)
    {
        while (pos < n && tokens.Get(pos).Channel != 0)
            pos++;
        return pos;
    }

    /// Skip a balanced < … > block starting at <paramref name="pos"/> (the '<').
    /// Returns the index of the first token AFTER the closing '>'.
    private static int SkipAngledBlock(ITokenStream tokens, int pos, int n)
    {
        int depth = 1;
        int i = NextDefaultToken(tokens, pos + 1, n);
        while (i < n && depth > 0)
        {
            string t = tokens.Get(i).Text;
            if (t == "<") depth++;
            else if (t == ">") depth--;
            i = NextDefaultToken(tokens, i + 1, n);
        }
        return i;
    }

    /// Count type parameters in an optional < T, U, … > immediately at
    /// <paramref name="pos"/>.  Also collects the parameter names.
    private static int CountTypeParams(
        ITokenStream tokens, int pos, int n,
        out List<string> typeParamNames)
    {
        typeParamNames = new List<string>();
        int p = NextDefaultToken(tokens, pos, n);
        if (p >= n || tokens.Get(p).Text != "<") return 0;

        int depth = 1, arity = 1;
        int i = NextDefaultToken(tokens, p + 1, n);
        while (i < n && depth > 0)
        {
            string t = tokens.Get(i).Text;
            if (t == "<")
                depth++;
            else if (t == ">")
                depth--;
            else if (t == "," && depth == 1)
                arity++;
            else if (depth == 1 && IsIdentLike(t))
                typeParamNames.Add(t);
            i = NextDefaultToken(tokens, i + 1, n);
        }
        return depth == 0 ? arity : 0;
    }

    private static bool IsIdentLike(string t)
    {
        if (string.IsNullOrEmpty(t)) return false;
        char c = t[0];
        return char.IsLetter(c) || c == '_' || c == '@';
    }

    // C# reserved keywords that can NEVER be bare (non-@-escaped) identifiers.
    private static readonly HashSet<string> _reservedKeywords =
        new HashSet<string>(StringComparer.Ordinal)
        {
            "abstract", "as",        "base",       "bool",      "break",
            "byte",     "case",      "catch",      "char",      "checked",
            "class",    "const",     "continue",   "decimal",   "default",
            "delegate", "do",        "double",     "else",      "enum",
            "event",    "explicit",  "extern",     "false",     "finally",
            "fixed",    "float",     "for",        "foreach",   "goto",
            "if",       "implicit",  "in",         "int",       "interface",
            "internal", "is",        "lock",       "long",      "namespace",
            "new",      "null",      "object",     "operator",  "out",
            "override", "params",    "private",    "protected", "public",
            "readonly", "ref",       "return",     "sbyte",     "sealed",
            "short",    "sizeof",    "stackalloc", "static",    "string",
            "struct",   "switch",    "this",       "throw",     "true",
            "try",      "typeof",    "uint",       "ulong",     "unchecked",
            "unsafe",   "ushort",    "using",      "virtual",   "void",
            "volatile", "while",
        };

    private static bool IsReservedKeyword(string name) =>
        _reservedKeywords.Contains(name);

    private void PopulateBuiltinTypes()
    {
        // C# predefined types (§8.3–8.4 of ECMA-334)
        foreach (var t in new[] {
            "bool", "byte", "sbyte", "char", "decimal", "double", "float",
            "int",  "uint", "long",  "ulong", "short",  "ushort",
            "object", "string", "void", "dynamic",
        })
            _knownTypeNames.Add(t);

        // Common BCL short names used unqualified in real code
        foreach (var t in new[] {
            "Boolean", "Byte", "SByte", "Char", "Decimal", "Double", "Single",
            "Int16", "Int32", "Int64", "UInt16", "UInt32", "UInt64",
            "IntPtr", "UIntPtr", "Object", "String", "Void",
            "Guid", "DateTime", "DateTimeOffset", "TimeSpan", "Uri",
            "Exception", "Type", "Enum", "Delegate", "MulticastDelegate",
            "Attribute", "Math", "Convert", "Console", "Environment",
        })
            _knownTypeNames.Add(t);

        // Common generic BCL types with their arities
        var generics = new (string Name, int Arity)[] {
            ("List",                 1), ("IList",                1),
            ("IEnumerable",          1), ("IEnumerator",           1),
            ("ICollection",          1), ("IReadOnlyList",          1),
            ("IReadOnlyCollection",  1), ("IReadOnlyDictionary",    2),
            ("Dictionary",           2), ("IDictionary",            2),
            ("SortedDictionary",     2), ("SortedList",             2),
            ("HashSet",              1), ("SortedSet",              1),
            ("Queue",                1), ("Stack",                  1),
            ("LinkedList",           1), ("LinkedListNode",         1),
            ("Func",                 1), ("Func",                   2),
            ("Func",                 3), ("Func",                   4),
            ("Action",               1), ("Action",                 2),
            ("Action",               3), ("Predicate",              1),
            ("Comparison",           1), ("Converter",              2),
            ("Task",                 1), ("ValueTask",              1),
            ("Nullable",             1),
            ("IAsyncEnumerable",     1), ("IAsyncEnumerator",       1),
            ("Span",                 1), ("ReadOnlySpan",           1),
            ("Memory",               1), ("ReadOnlyMemory",         1),
            ("KeyValuePair",         2), ("Tuple",                  2),
            ("Tuple",                3), ("Tuple",                  4),
            ("ImmutableArray",       1), ("ImmutableList",          1),
            ("ImmutableDictionary",  2), ("ImmutableHashSet",       1),
            ("Lazy",                 1), ("WeakReference",          1),
            ("EventHandler",         1),
            ("IEqualityComparer",    1), ("IComparer",              1),
            ("EqualityComparer",     1), ("Comparer",               1),
            ("ConcurrentDictionary", 2), ("ConcurrentQueue",        1),
            ("ConcurrentStack",      1), ("ConcurrentBag",          1),
        };
        foreach (var (name, arity) in generics)
        {
            _knownTypeNames.Add(name);
            HashSet<int> set;
            if (!_genericArities.TryGetValue(name, out set))
                _genericArities[name] = set = new HashSet<int>();
            set.Add(arity);
        }
    }
}
