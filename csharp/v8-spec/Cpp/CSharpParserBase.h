#pragma once
#include "antlr4-runtime.h"
#include "CSharpSymbolTable.h"
#include <memory>
#include <string>

class CSharpParser;

class CSharpParserBase : public antlr4::Parser
{
public:
    CSharpParserBase(antlr4::TokenStream *input);

    // ── Symbol-table access ────────────────────────────────────────────────
    CSharpSymbolTable *symTable();

    // ── Semantic predicates ────────────────────────────────────────────────
    bool IsLocalVariableDeclaration();
    bool IsCastExpressionAhead();
    bool IsDeclarationPatternAhead();
    bool IsConstantPatternAhead();
    bool IsTypeParameterName();
    bool IsValueTypeName();
    bool IsReferenceTypeName();
    bool IsDelegateTypeName();
    bool IsInterfaceTypeName();
    bool IsClassTypeName();
    bool IsClassBaseInterfaceList();
    bool IsClassBaseClassType();
    bool IsExplicitlyTypedLocalVariable();
    bool IsExplicitlyTypedRefLocalVariable();
    bool IsImplicitlyTypedLocalVariable();

    // ── Grammar actions ────────────────────────────────────────────────────
    void EnterNamespaceScope();
    void EnterTypeScope();
    void EnterBlockScope();
    void ExitCurrentScope();
    void OnTypeParameter();
    void OnUsingAliasDirective();
    void OnUsingNamespaceDirective();
    void BeginVariableDeclaration();
    void OnVariableDeclarator();

    // ── Look-ahead helpers ─────────────────────────────────────────────────
    bool LookAheadIs(int pos, int value);
    bool LookAheadIsNot(int pos, int value);

    // ── Post-parse tree operations ─────────────────────────────────────────
    void ReduceTree(antlr4::ParserRuleContext *currentctx);

    void AsInvocationExpression(antlr4::ParserRuleContext *currentctx);
    void AsElementAccess(antlr4::ParserRuleContext *currentctx);
    void AsMemberAccess(antlr4::ParserRuleContext *currentctx);
    void AsNullConditionalMemberAccess(antlr4::ParserRuleContext *currentctx);
    void AsNullConditionalElementAccess(antlr4::ParserRuleContext *currentctx);
    void AsPostIncrementExpression(antlr4::ParserRuleContext *currentctx);
    void AsPostDecrementExpression(antlr4::ParserRuleContext *currentctx);
    void AsNullForgivingExpression(antlr4::ParserRuleContext *currentctx);
    void AsPointerMemberAccess(antlr4::ParserRuleContext *currentctx);

    void ElementAccessSemanticCheck(antlr4::ParserRuleContext *currentctx);

private:
    // Symbol table (lazy-initialised)
    std::unique_ptr<CSharpSymbolTable> symTable_;
    // Pending variable type text, set by BeginVariableDeclaration
    std::string pendingVarType_ = "?";

    // ── insertNode helper ──────────────────────────────────────────────────
    void notifySemanticError(int line, int charPositionInLine, const std::string &msg);
    void insertNode(antlr4::ParserRuleContext *currentctx, const std::string &contextTypeName);
    int  ruleIdx(const std::string &name) const;

    bool classBaseTypeCheck(bool wantInterface);

    void cacheRuleIndices();
    bool ruleIndicesCached_              = false;
    int  rulePrimaryExpression_          = -1;
    int  ruleTypeArgumentList_           = -1;
    int  ruleElementAccess_              = -1;
    int  rulePointerElementAccess_       = -1;
    int  ruleNullCondElementAccess_      = -1;
    int  ruleArrayCreationExpression_    = -1;
    int  ruleStackallocExpression_       = -1;
};
