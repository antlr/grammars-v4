import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

public abstract class CSharpParserBase extends Parser
{
    protected CSharpParserBase(TokenStream input)
    {
        super(input);
    }

    // -------------------------------------------------------------------------
    // Look-ahead helpers
    // -------------------------------------------------------------------------

    public boolean LookAheadIs(int pos, int value)
    {
        return _input.LA(pos) == value;
    }

    public boolean LookAheadIsNot(int pos, int value)
    {
        return _input.LA(pos) != value;
    }

    // -------------------------------------------------------------------------
    // Error reporting
    // -------------------------------------------------------------------------

    private void notifySemanticError(int line, int charPositionInLine, String msg)
    {
        notifyErrorListeners(msg);
    }

    // -------------------------------------------------------------------------
    // ReduceTree — post-parse cleanup (for external tooling)
    // -------------------------------------------------------------------------

    public void ReduceTree(ParserRuleContext currentctx)
    {
        String[] names = getRuleNames();
        int ruleTypeArgumentList = -1;
        for (int i = 0; i < names.length; i++)
        {
            if ("type_argument_list".equals(names[i]))
            {
                ruleTypeArgumentList = i;
                break;
            }
        }
        final int tal = ruleTypeArgumentList;
        takeOutEmpties(currentctx, tal);

        String reduceProperty = System.getenv("ANTLR_REDUCE_TREE");
        if (!"yes".equalsIgnoreCase(reduceProperty)) return;

        boolean reduceAllChildren = "yes".equalsIgnoreCase(System.getenv("ANTLR_REDUCE_ALL_CHILDREN"));
        reducer(currentctx, reduceAllChildren);
    }

    private static void takeOutEmpties(ParserRuleContext node, int tal)
    {
        if (node == null || node.children == null) return;
        for (int ix = node.children.size() - 1; ix >= 0; ix--)
        {
            ParseTree child = node.children.get(ix);
            if (child instanceof ParserRuleContext)
            {
                ParserRuleContext childCtx = (ParserRuleContext) child;
                if (childCtx.getRuleIndex() == tal && childCtx.getChildCount() == 0)
                    node.children.remove(ix);
                else
                    takeOutEmpties(childCtx, tal);
            }
        }
    }

    private static void reducer(ParserRuleContext node, boolean reduceAllChildren)
    {
        if (node == null || node.children == null) return;
        for (ParseTree child : node.children)
            if (child instanceof ParserRuleContext) reducer((ParserRuleContext) child, reduceAllChildren);
        if (reduceAllChildren || node.getChildCount() == 1)
        {
            for (int i = 0; i < node.children.size(); i++)
            {
                ParseTree child = node.children.get(i);
                if (child instanceof ParserRuleContext)
                {
                    ParserRuleContext childNode = (ParserRuleContext) child;
                    if (childNode.getChildCount() == 1)
                    {
                        ParseTree grandchild = childNode.getChild(0);
                        if (grandchild instanceof ParserRuleContext)
                            node.children.set(i, grandchild);
                    }
                }
            }
        }
    }

    // -------------------------------------------------------------------------
    // insertNode — rewire parse tree (MLR un-inlining)
    // -------------------------------------------------------------------------

    private void insertNode(ParserRuleContext currentctx, String contextTypeName)
    {
        // try
        // {
        //     Class<?> contextType = null;
        //     for (Class<?> inner : getClass().getDeclaredClasses())
        //     {
        //         if (inner.getSimpleName().equals(contextTypeName))
        //         {
        //             contextType = inner;
        //             break;
        //         }
        //     }
        //     if (contextType == null) return;
        //
        //     Constructor<?> ctor = contextType.getConstructor(ParserRuleContext.class, int.class);
        //     ParserRuleContext insertedctx = (ParserRuleContext) ctor.newInstance(
        //         currentctx, currentctx.invokingState);
        // 
        //     insertedctx.children = (currentctx.children != null)
        //         ? new ArrayList<>(currentctx.children)
        //         : new ArrayList<>();
        //     if (currentctx.children == null)
        //         currentctx.children = new ArrayList<>();
        //     currentctx.children.clear();
        //     currentctx.children.add(insertedctx);
        // }
        // catch (Exception e)
        // {
        //     throw new RuntimeException("insertNode failed: " + e.getMessage(), e);
        // }
    }

    public void AsInvocationExpression(ParserRuleContext currentctx)         { insertNode(currentctx, "Invocation_expressionContext"); }
    public void AsElementAccess(ParserRuleContext currentctx)                { insertNode(currentctx, "Element_accessContext"); }
    public void AsMemberAccess(ParserRuleContext currentctx)                 { insertNode(currentctx, "Member_accessContext"); }
    public void AsNullConditionalMemberAccess(ParserRuleContext currentctx)  { insertNode(currentctx, "Null_conditional_member_accessContext"); }
    public void AsNullConditionalElementAccess(ParserRuleContext currentctx) { insertNode(currentctx, "Null_conditional_element_accessContext"); }
    public void AsPostIncrementExpression(ParserRuleContext currentctx)      { insertNode(currentctx, "Post_increment_expressionContext"); }
    public void AsPostDecrementExpression(ParserRuleContext currentctx)      { insertNode(currentctx, "Post_decrement_expressionContext"); }
    public void AsNullForgivingExpression(ParserRuleContext currentctx)      { insertNode(currentctx, "Null_forgiving_expressionContext"); }
    public void AsPointerMemberAccess(ParserRuleContext currentctx)          { insertNode(currentctx, "Pointer_member_accessContext"); }

    // -------------------------------------------------------------------------
    // ElementAccessSemanticCheck
    // -------------------------------------------------------------------------

    private static int ruleIdx(String[] names, String name)
    {
        for (int i = 0; i < names.length; i++) if (name.equals(names[i])) return i;
        return -1;
    }

    public void ElementAccessSemanticCheck(ParserRuleContext currentctx)
    {
        String[] names = getRuleNames();

        int rulePrimaryExpression            = ruleIdx(names, "primary_expression");
        int ruleElementAccess                = ruleIdx(names, "element_access");
        int rulePointerElementAccess         = ruleIdx(names, "pointer_element_access");
        int ruleNullConditionalElementAccess = ruleIdx(names, "null_conditional_element_access");
        int ruleArrayCreationExpression      = ruleIdx(names, "array_creation_expression");
        int ruleStackallocExpression         = ruleIdx(names, "stackalloc_expression");

        if (currentctx.getRuleIndex() != rulePrimaryExpression || currentctx.getChildCount() != 1)
            return;

        ParseTree childTree = currentctx.getChild(0);
        if (!(childTree instanceof ParserRuleContext)) return;
        ParserRuleContext childRule = (ParserRuleContext) childTree;

        int childRuleIndex  = childRule.getRuleIndex();
        int childChildCount = childRule.getChildCount();

        if (childRuleIndex == ruleElementAccess || childRuleIndex == rulePointerElementAccess)
        {
            if (childChildCount != 4) return;
        }
        else if (childRuleIndex == ruleNullConditionalElementAccess)
        {
            if (childChildCount != 5) return;
        }
        else return;

        ParseTree accessTargetTree = childRule.getChild(0);
        if (!(accessTargetTree instanceof ParserRuleContext)) return;
        ParserRuleContext accessTarget = (ParserRuleContext) accessTargetTree;

        if (accessTarget.getRuleIndex() != rulePrimaryExpression || accessTarget.getChildCount() == 0)
            return;

        ParseTree lhsTargetTree = accessTarget.getChild(0);
        if (!(lhsTargetTree instanceof ParserRuleContext)) return;
        ParserRuleContext lhsTarget = (ParserRuleContext) lhsTargetTree;

        if (lhsTarget.getRuleIndex() != ruleArrayCreationExpression
            && lhsTarget.getRuleIndex() != ruleStackallocExpression)
            return;

        Token lhsLast     = lhsTarget.getStop();
        int   lhsLastType = lhsLast != null ? lhsLast.getType() : -1;

        if (lhsLastType == CSharpLexer.TK_RBRACE) return; // initializer present

        if (lhsLastType != CSharpLexer.TK_RBRACK)
        {
            System.err.printf("%d:%d Error: Unexpected LHS last token %s (%d).%n",
                lhsLast.getLine(), lhsLast.getCharPositionInLine(),
                lhsLast.getText(), lhsLastType);
            return;
        }

        String childRuleName = names[childRuleIndex];
        String lhsRuleName   = names[lhsTarget.getRuleIndex()];
        String childPrefix   = "AEIOUaeiou".indexOf(childRuleName.charAt(0)) >= 0 ? "an" : "a";
        String lhsPrefix     = "AEIOUaeiou".indexOf(lhsRuleName.charAt(0))   >= 0 ? "an" : "a";

        notifySemanticError(lhsLast.getLine(), lhsLast.getCharPositionInLine(),
            "LHS of " + childPrefix + " " + childRuleName +
            " cannot be " + lhsPrefix + " " + lhsRuleName + " unless it has an initializer");
    }

    // =========================================================================
    // Symbol table
    // =========================================================================

    public final CSharpSymbolTable symTable = new CSharpSymbolTable();
    private String _pendingVarType = "?";

    public void EnterNamespaceScope() { symTable.enterScope(CSharpScopeKind.Namespace); }
    public void EnterTypeScope()      { symTable.enterScope(CSharpScopeKind.Type); }
    public void EnterBlockScope()     { symTable.enterScope(CSharpScopeKind.Block); }
    public void ExitCurrentScope()    { symTable.exitScope(); }

    public void OnTypeParameter()
    {
        ParserRuleContext ctx = (ParserRuleContext) _ctx;
        String name = ctx.getChild(ctx.getChildCount() - 1).getText();
        symTable.declareTypeParam(name);
    }

    public void OnUsingAliasDirective()
    {
        ParserRuleContext ctx = (ParserRuleContext) _ctx;
        String alias  = ctx.getChild(1).getText();
        String target = ctx.getChild(3).getText();
        symTable.declareAlias(alias, target);
    }

    public void OnUsingNamespaceDirective()
    {
        ParserRuleContext ctx = (ParserRuleContext) _ctx;
        String ns = ctx.getChild(1).getText();
        symTable.importNamespace(ns);
    }

    public void BeginVariableDeclaration()
    {
        ParserRuleContext ctx = (ParserRuleContext) _ctx;
        _pendingVarType = ctx.getChild(ctx.getChildCount() - 1).getText();
    }

    public void OnVariableDeclarator()
    {
        ParserRuleContext ctx = (ParserRuleContext) _ctx;
        String id = ctx.getChild(ctx.getChildCount() - 1).getText();
        symTable.declareVariable(id, _pendingVarType);
    }

    public boolean IsCastExpressionAhead()
    {
        Token tok = ((CommonTokenStream)_input).LT(2);
        if (tok == null) return true;
        CSharpScope.CSharpSymbol sym = symTable.currentScope().lookupChain(tok.getText());
        return sym == null || sym.kind != CSharpSymbolKind.Variable;
    }

    public boolean IsTypeParameterName()
    {
        Token tok = ((CommonTokenStream)_input).LT(1);
        if (tok == null) return false;
        CSharpScope.CSharpSymbol sym = symTable.currentScope().lookupChain(tok.getText());
        return sym != null && sym.kind == CSharpSymbolKind.TypeParameter;
    }

    public boolean IsValueTypeName()
    {
        Token tok = ((CommonTokenStream)_input).LT(1);
        if (tok == null) return false;
        int type = tok.getType();
        if (type == CSharpLexer.KW_BOOL  || type == CSharpLexer.KW_BYTE   ||
            type == CSharpLexer.KW_CHAR   || type == CSharpLexer.KW_DECIMAL ||
            type == CSharpLexer.KW_DOUBLE || type == CSharpLexer.KW_FLOAT   ||
            type == CSharpLexer.KW_INT    || type == CSharpLexer.KW_LONG    ||
            type == CSharpLexer.KW_SBYTE  || type == CSharpLexer.KW_SHORT   ||
            type == CSharpLexer.KW_UINT   || type == CSharpLexer.KW_ULONG   ||
            type == CSharpLexer.KW_USHORT || type == CSharpLexer.TK_LPAREN)
            return true;
        CSharpScope.CSharpSymbol sym = symTable.currentScope().lookupChain(tok.getText());
        if (sym == null || sym.kind != CSharpSymbolKind.Type) return false;
        return sym.typeKind == CSharpTypeKind.Struct || sym.typeKind == CSharpTypeKind.Enum;
    }

    public boolean IsReferenceTypeName()
    {
        Token tok = ((CommonTokenStream)_input).LT(1);
        if (tok == null) return true;
        int type = tok.getType();
        if (type == CSharpLexer.KW_DYNAMIC || type == CSharpLexer.KW_OBJECT ||
            type == CSharpLexer.KW_STRING   || type == CSharpLexer.TK_LBRACK)
            return true;
        CSharpScope.CSharpSymbol sym = symTable.currentScope().lookupChain(tok.getText());
        if (sym == null) return true;
        if (sym.kind == CSharpSymbolKind.TypeParameter) return false;
        if (sym.kind == CSharpSymbolKind.Type &&
            (sym.typeKind == CSharpTypeKind.Struct || sym.typeKind == CSharpTypeKind.Enum)) return false;
        return true;
    }

    public boolean IsDelegateTypeName()
    {
        Token tok = ((CommonTokenStream)_input).LT(1);
        if (tok == null) return false;
        CSharpScope.CSharpSymbol sym = symTable.currentScope().lookupChain(tok.getText());
        return sym != null && sym.kind == CSharpSymbolKind.Type && sym.typeKind == CSharpTypeKind.Delegate;
    }

    public boolean IsInterfaceTypeName()
    {
        Token tok = ((CommonTokenStream)_input).LT(1);
        if (tok == null) return false;
        CSharpScope.CSharpSymbol sym = symTable.currentScope().lookupChain(tok.getText());
        return sym != null && sym.kind == CSharpSymbolKind.Type && sym.typeKind == CSharpTypeKind.Interface;
    }

    public boolean IsClassTypeName()
    {
        Token tok = ((CommonTokenStream)_input).LT(1);
        if (tok == null) return true;
        int type = tok.getType();
        if (type == CSharpLexer.KW_OBJECT || type == CSharpLexer.KW_STRING) return true;
        CSharpScope.CSharpSymbol sym = symTable.currentScope().lookupChain(tok.getText());
        if (sym == null) return true;
        if (sym.kind != CSharpSymbolKind.Type) return true;
        return sym.typeKind != CSharpTypeKind.Interface && sym.typeKind != CSharpTypeKind.Delegate;
    }

    private boolean classBaseTypeCheck(boolean wantInterface)
    {
        Token tok = ((CommonTokenStream)_input).LT(2);
        if (tok == null) return !wantInterface;
        int type = tok.getType();
        if (type == CSharpLexer.KW_OBJECT || type == CSharpLexer.KW_STRING) return !wantInterface;
        CSharpScope.CSharpSymbol sym = symTable.currentScope().lookupChain(tok.getText());
        if (sym == null || sym.kind != CSharpSymbolKind.Type) return !wantInterface;
        boolean isIface = sym.typeKind == CSharpTypeKind.Interface;
        return wantInterface ? isIface : !isIface;
    }

    public boolean IsClassBaseInterfaceList() { return classBaseTypeCheck(true); }
    public boolean IsClassBaseClassType()     { return classBaseTypeCheck(false); }

    public boolean IsDeclarationPatternAhead()
    {
        int savedIndex = _input.index();
        CSharpParser par = new CSharpParser(_input);
        par.removeErrorListeners();
        par.setErrorHandler(new BailErrorStrategy());
        try
        {
            par.type_();
            Token next = ((CommonTokenStream)_input).LT(1);
            return next != null && (next.getType() == CSharpLexer.Simple_Identifier || "_".equals(next.getText()));
        }
        catch (Exception e) { return false; }
        finally { _input.seek(savedIndex); }
    }

    public boolean IsConstantPatternAhead() { return !IsDeclarationPatternAhead(); }

    public boolean IsImplicitlyTypedLocalVariable()
    {
        Token tok = ((CommonTokenStream)_input).LT(1);
        if (tok == null) return true;
        if (tok.getType() != CSharpLexer.KW_VAR) return false;
        CSharpScope.CSharpSymbol sym = symTable.currentScope().lookupChain("var");
        if (sym != null && sym.kind == CSharpSymbolKind.Type) return false;
        Token lt3 = ((CommonTokenStream)_input).LT(3);
        if (lt3 == null || !"=".equals(lt3.getText())) return false;
        Token lt4 = ((CommonTokenStream)_input).LT(4);
        if (lt4 != null && "{".equals(lt4.getText())) return false;
        return true;
    }

    public boolean IsExplicitlyTypedLocalVariable()
    {
        Token tok = ((CommonTokenStream)_input).LT(1);
        if (tok == null) return true;
        if (tok.getType() != CSharpLexer.KW_VAR) return true;
        CSharpScope.CSharpSymbol sym = symTable.currentScope().lookupChain("var");
        if (sym != null && sym.kind == CSharpSymbolKind.Type) return true;
        Token lt3 = ((CommonTokenStream)_input).LT(3);
        if (lt3 == null || !"=".equals(lt3.getText())) return true;
        Token lt4 = ((CommonTokenStream)_input).LT(4);
        if (lt4 != null && "{".equals(lt4.getText())) return true;
        return false;
    }

    public boolean IsExplicitlyTypedRefLocalVariable()
    {
        Token tok = ((CommonTokenStream)_input).LT(1);
        return tok != null && tok.getType() == CSharpLexer.KW_REF;
    }

    // Gates comma-separated declarators: false when type is 'var' (only one declarator allowed).
    protected boolean IsLocalVariableDeclaration()
    {
        if (!(this._ctx instanceof CSharpParser.Local_variable_declarationContext)) return true;
        CSharpParser.Local_variable_declarationContext local_var_decl =
            (CSharpParser.Local_variable_declarationContext) this._ctx;
        CSharpParser.Local_variable_typeContext local_variable_type = local_var_decl.local_variable_type();
        if (local_variable_type == null) return true;
        return !local_variable_type.getText().equals("var");
    }
}

// =============================================================================
// Symbol table support classes
// =============================================================================

enum CSharpSymbolKind { Type, Variable, Namespace, TypeParameter, Alias }
enum CSharpTypeKind   { Class, Struct, Interface, Enum, Delegate }
enum CSharpScopeKind  { Global, Namespace, Type, Method, Block }

class CSharpScope
{
    static class CSharpSymbol {
        final String name;
        final CSharpSymbolKind kind;
        CSharpTypeKind typeKind;
        String typeRef;
        String target;
        CSharpSymbol(String name, CSharpSymbolKind kind) { this.name = name; this.kind = kind; }
    }

    final CSharpScopeKind kind;
    final CSharpScope parent;
    final String name;
    private final Map<String,CSharpSymbol> _symbols = new HashMap<>();

    CSharpScope(CSharpScopeKind kind, CSharpScope parent, String name)
    { this.kind = kind; this.parent = parent; this.name = name; }

    void declare(CSharpSymbol s) { _symbols.put(s.name, s); }
    CSharpSymbol lookup(String n) { return _symbols.get(n); }
    CSharpSymbol lookupChain(String n)
    {
        CSharpSymbol s = lookup(n);
        if (s != null) return s;
        return parent != null ? parent.lookupChain(n) : null;
    }
}

class CSharpSymbolTable
{
    private final Set<String> _knownTypeNames = new HashSet<>();
    private final Map<String,Set<Integer>> _genericArities = new HashMap<>();
    private final Stack<CSharpScope> _scopeStack = new Stack<>();
    final CSharpScope globalScope;

    CSharpSymbolTable()
    {
        globalScope = new CSharpScope(CSharpScopeKind.Global, null, "<global>");
        _scopeStack.push(globalScope);
        populateBuiltins();
    }

    CSharpScope currentScope() { return _scopeStack.peek(); }

    void enterScope(CSharpScopeKind kind) { _scopeStack.push(new CSharpScope(kind, _scopeStack.peek(), "")); }
    void exitScope() { if (_scopeStack.size() > 1) _scopeStack.pop(); }

    void declareType(String name, CSharpTypeKind kind, int arity)
    {
        CSharpScope.CSharpSymbol sym = new CSharpScope.CSharpSymbol(name, CSharpSymbolKind.Type);
        sym.typeKind = kind;
        currentScope().declare(sym);
        _knownTypeNames.add(name);
        if (arity > 0) { _genericArities.computeIfAbsent(name, k -> new HashSet<>()).add(arity); }
    }

    void declareTypeParam(String name)
    {
        currentScope().declare(new CSharpScope.CSharpSymbol(name, CSharpSymbolKind.TypeParameter));
        _knownTypeNames.add(name);
    }

    void declareVariable(String name, String typeRef)
    {
        CSharpScope.CSharpSymbol sym = new CSharpScope.CSharpSymbol(name, CSharpSymbolKind.Variable);
        sym.typeRef = typeRef;
        currentScope().declare(sym);
    }

    void declareAlias(String alias, String target)
    {
        CSharpScope.CSharpSymbol sym = new CSharpScope.CSharpSymbol(alias, CSharpSymbolKind.Alias);
        sym.target = target;
        currentScope().declare(sym);
        _knownTypeNames.add(alias);
    }

    void importNamespace(String ns)
    {
        currentScope().declare(new CSharpScope.CSharpSymbol(ns, CSharpSymbolKind.Namespace));
    }

    boolean isTypeName(String name)
    {
        CSharpScope.CSharpSymbol sym = currentScope().lookupChain(name);
        if (sym != null)
            return sym.kind == CSharpSymbolKind.Type || sym.kind == CSharpSymbolKind.TypeParameter || sym.kind == CSharpSymbolKind.Alias;
        return _knownTypeNames.contains(name);
    }

    void preScan(TokenStream tokens)
    {
        if (tokens instanceof BufferedTokenStream) ((BufferedTokenStream)tokens).fill();
        int n = tokens.size();
        for (int i = 0; i < n; i++)
        {
            Token tok = tokens.get(i);
            if (tok.getChannel() != 0) continue;
            String txt = tok.getText();
            if ("class".equals(txt) || "struct".equals(txt) || "interface".equals(txt) || "enum".equals(txt))
            {
                int j = nextDefault(tokens, i + 1, n);
                while (j < n && ("partial".equals(tokens.get(j).getText()) || "ref".equals(tokens.get(j).getText())))
                    j = nextDefault(tokens, j + 1, n);
                if (j < n && isIdentLike(tokens.get(j).getText()))
                {
                    String typeName = tokens.get(j).getText();
                    List<String> typeParams = new ArrayList<>();
                    int arity = countTypeParams(tokens, j + 1, n, typeParams);
                    registerType(typeName, arity, typeParams);
                }
            }
            else if ("delegate".equals(txt))
            {
                int j = nextDefault(tokens, i + 1, n);
                while (j < n && ("ref".equals(tokens.get(j).getText()) || "readonly".equals(tokens.get(j).getText())))
                    j = nextDefault(tokens, j + 1, n);
                if (j >= n) continue;
                j = nextDefault(tokens, j + 1, n);
                if (j < n && "<".equals(tokens.get(j).getText())) j = skipAngled(tokens, j, n);
                while (j < n && ("[".equals(tokens.get(j).getText()) || "]".equals(tokens.get(j).getText()) || ",".equals(tokens.get(j).getText())))
                    j = nextDefault(tokens, j + 1, n);
                if (j < n && "?".equals(tokens.get(j).getText())) j = nextDefault(tokens, j + 1, n);
                if (j < n && isIdentLike(tokens.get(j).getText()))
                {
                    String typeName = tokens.get(j).getText();
                    List<String> typeParams = new ArrayList<>();
                    int arity = countTypeParams(tokens, j + 1, n, typeParams);
                    registerType(typeName, arity, typeParams);
                }
            }
        }
    }

    private void registerType(String name, int arity, List<String> typeParams)
    {
        if (!isReserved(name)) _knownTypeNames.add(name);
        if (arity > 0 && !isReserved(name)) _genericArities.computeIfAbsent(name, k -> new HashSet<>()).add(arity);
        for (String tp : typeParams) if (isIdentLike(tp) && !isReserved(tp)) _knownTypeNames.add(tp);
    }

    private static int nextDefault(TokenStream tokens, int pos, int n)
    { while (pos < n && tokens.get(pos).getChannel() != 0) pos++; return pos; }

    private static int skipAngled(TokenStream tokens, int pos, int n)
    {
        int depth = 1, i = nextDefault(tokens, pos + 1, n);
        while (i < n && depth > 0) { String t = tokens.get(i).getText(); if ("<".equals(t)) depth++; else if (">".equals(t)) depth--; i = nextDefault(tokens, i + 1, n); }
        return i;
    }

    private static int countTypeParams(TokenStream tokens, int pos, int n, List<String> names)
    {
        int p = nextDefault(tokens, pos, n);
        if (p >= n || !"<".equals(tokens.get(p).getText())) return 0;
        int depth = 1, arity = 1, i = nextDefault(tokens, p + 1, n);
        while (i < n && depth > 0) {
            String t = tokens.get(i).getText();
            if ("<".equals(t)) depth++; else if (">".equals(t)) depth--; else if (",".equals(t) && depth == 1) arity++; else if (depth == 1 && isIdentLike(t)) names.add(t);
            i = nextDefault(tokens, i + 1, n);
        }
        return depth == 0 ? arity : 0;
    }

    private static boolean isIdentLike(String t) { if (t == null || t.isEmpty()) return false; char c = t.charAt(0); return Character.isLetter(c) || c == '_' || c == '@'; }

    private static final Set<String> RESERVED = new HashSet<>(Arrays.asList(
        "abstract","as","base","bool","break","byte","case","catch","char","checked",
        "class","const","continue","decimal","default","delegate","do","double","else",
        "enum","event","explicit","extern","false","finally","fixed","float","for",
        "foreach","goto","if","implicit","in","int","interface","internal","is","lock",
        "long","namespace","new","null","object","operator","out","override","params",
        "private","protected","public","readonly","ref","return","sbyte","sealed","short",
        "sizeof","stackalloc","static","string","struct","switch","this","throw","true",
        "try","typeof","uint","ulong","unchecked","unsafe","ushort","using","virtual",
        "void","volatile","while"));
    private static boolean isReserved(String name) { return RESERVED.contains(name); }

    private void populateBuiltins()
    {
        for (String t : new String[]{ "bool","byte","sbyte","char","decimal","double","float","int","uint","long","ulong","short","ushort","object","string","void","dynamic",
            "Boolean","Byte","SByte","Char","Decimal","Double","Single","Int16","Int32","Int64","UInt16","UInt32","UInt64","IntPtr","UIntPtr","Object","String","Void",
            "Guid","DateTime","DateTimeOffset","TimeSpan","Uri","Exception","Type","Enum","Delegate","MulticastDelegate","Attribute","Math","Convert","Console","Environment",
            "List","IList","IEnumerable","IEnumerator","ICollection","IReadOnlyList","IReadOnlyCollection","IReadOnlyDictionary","Dictionary","IDictionary",
            "SortedDictionary","SortedList","HashSet","SortedSet","Queue","Stack","LinkedList","LinkedListNode","Func","Action","Predicate","Comparison","Converter",
            "Task","ValueTask","Nullable","IAsyncEnumerable","IAsyncEnumerator","Span","ReadOnlySpan","Memory","ReadOnlyMemory","KeyValuePair","Tuple",
            "ImmutableArray","ImmutableList","ImmutableDictionary","ImmutableHashSet","Lazy","WeakReference","EventHandler",
            "IEqualityComparer","IComparer","EqualityComparer","Comparer","ConcurrentDictionary","ConcurrentQueue","ConcurrentStack","ConcurrentBag"})
            _knownTypeNames.add(t);
    }
}
