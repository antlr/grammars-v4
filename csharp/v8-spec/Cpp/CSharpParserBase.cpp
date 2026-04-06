#include "antlr4-runtime.h"
#include "CSharpParserBase.h"
#include "CSharpParser.h"
#include "CSharpLexer.h"
#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <string>

CSharpParserBase::CSharpParserBase(antlr4::TokenStream *input)
    : antlr4::Parser(input)
{
}

// ─── Symbol table ─────────────────────────────────────────────────────────────

CSharpSymbolTable *CSharpParserBase::symTable()
{
    if (!symTable_)
        symTable_ = std::make_unique<CSharpSymbolTable>();
    return symTable_.get();
}

// ─── Look-ahead helpers ───────────────────────────────────────────────────────

bool CSharpParserBase::LookAheadIs(int pos, int value)
{
    return _input->LA(pos) == value;
}

bool CSharpParserBase::LookAheadIsNot(int pos, int value)
{
    return _input->LA(pos) != value;
}

// ─── Error reporting ─────────────────────────────────────────────────────────

void CSharpParserBase::notifySemanticError(int /*line*/, int /*charPositionInLine*/, const std::string &msg)
{
    notifyErrorListeners(msg);
}

// ─── Rule-index helper ────────────────────────────────────────────────────────

int CSharpParserBase::ruleIdx(const std::string &name) const
{
    const auto &names = getRuleNames();
    for (int i = 0; i < static_cast<int>(names.size()); i++)
        if (names[i] == name) return i;
    return -1;
}

// ─── Rule-index cache ─────────────────────────────────────────────────────────

void CSharpParserBase::cacheRuleIndices()
{
    if (ruleIndicesCached_) return;
    rulePrimaryExpression_       = ruleIdx("primary_expression");
    ruleTypeArgumentList_        = ruleIdx("type_argument_list");
    ruleElementAccess_           = ruleIdx("element_access");
    rulePointerElementAccess_    = ruleIdx("pointer_element_access");
    ruleNullCondElementAccess_   = ruleIdx("null_conditional_element_access");
    ruleArrayCreationExpression_ = ruleIdx("array_creation_expression");
    ruleStackallocExpression_    = ruleIdx("stackalloc_expression");
    ruleIndicesCached_ = true;
}

// ─── Semantic predicates ──────────────────────────────────────────────────────

bool CSharpParserBase::IsCastExpressionAhead()
{
    antlr4::Token *tok = _input->LT(2);
    if (!tok) return true;
    CSharpSymbol *sym = symTable()->currentScope()->lookupChain(tok->getText());
    return sym == nullptr || sym->kind != CSharpSymbolKind::Variable;
}

bool CSharpParserBase::IsDeclarationPatternAhead()
{
    auto *par = new CSharpParser(_input);
    par->removeErrorListeners();
    par->setErrorHandler(std::make_shared<antlr4::BailErrorStrategy>());
    size_t savedIndex = _input->index();
    bool result = false;
    try
    {
        par->type_();
        antlr4::Token *next = _input->LT(1);
        if (next)
        {
            int tt = static_cast<int>(next->getType());
            result = (tt == CSharpLexer::Simple_Identifier || next->getText() == "_");
        }
    }
    catch (...) { }
    _input->seek(savedIndex);
    delete par;
    return result;
}

bool CSharpParserBase::IsConstantPatternAhead()
{
    return !IsDeclarationPatternAhead();
}

bool CSharpParserBase::IsTypeParameterName()
{
    antlr4::Token *t = _input->LT(1);
    if (!t) return false;
    CSharpSymbol *sym = symTable()->currentScope()->lookupChain(t->getText());
    return sym != nullptr && sym->kind == CSharpSymbolKind::TypeParameter;
}

bool CSharpParserBase::IsValueTypeName()
{
    antlr4::Token *t = _input->LT(1);
    if (!t) return false;
    int tt = static_cast<int>(t->getType());
    switch (tt)
    {
        case CSharpLexer::KW_BOOL:
        case CSharpLexer::KW_BYTE:
        case CSharpLexer::KW_CHAR:
        case CSharpLexer::KW_DECIMAL:
        case CSharpLexer::KW_DOUBLE:
        case CSharpLexer::KW_FLOAT:
        case CSharpLexer::KW_INT:
        case CSharpLexer::KW_LONG:
        case CSharpLexer::KW_SBYTE:
        case CSharpLexer::KW_SHORT:
        case CSharpLexer::KW_UINT:
        case CSharpLexer::KW_ULONG:
        case CSharpLexer::KW_USHORT:
        case CSharpLexer::TK_LPAREN:
            return true;
        default: break;
    }
    CSharpSymbol *sym = symTable()->currentScope()->lookupChain(t->getText());
    if (!sym || sym->kind != CSharpSymbolKind::Type) return false;
    return sym->typeKind == CSharpTypeKind::Struct || sym->typeKind == CSharpTypeKind::Enum;
}

bool CSharpParserBase::IsReferenceTypeName()
{
    antlr4::Token *t = _input->LT(1);
    if (!t) return true;
    int tt = static_cast<int>(t->getType());
    switch (tt)
    {
        case CSharpLexer::KW_DYNAMIC:
        case CSharpLexer::KW_OBJECT:
        case CSharpLexer::KW_STRING:
        case CSharpLexer::TK_LBRACK:
            return true;
        default: break;
    }
    CSharpSymbol *sym = symTable()->currentScope()->lookupChain(t->getText());
    if (!sym) return true;
    if (sym->kind == CSharpSymbolKind::TypeParameter) return false;
    if (sym->kind == CSharpSymbolKind::Type)
    {
        if (sym->typeKind == CSharpTypeKind::Struct || sym->typeKind == CSharpTypeKind::Enum)
            return false;
    }
    return true;
}

bool CSharpParserBase::IsDelegateTypeName()
{
    antlr4::Token *t = _input->LT(1);
    if (!t) return false;
    CSharpSymbol *sym = symTable()->currentScope()->lookupChain(t->getText());
    if (!sym || sym->kind != CSharpSymbolKind::Type) return false;
    return sym->typeKind == CSharpTypeKind::Delegate;
}

bool CSharpParserBase::IsInterfaceTypeName()
{
    antlr4::Token *t = _input->LT(1);
    if (!t) return false;
    CSharpSymbol *sym = symTable()->currentScope()->lookupChain(t->getText());
    if (!sym || sym->kind != CSharpSymbolKind::Type) return false;
    return sym->typeKind == CSharpTypeKind::Interface;
}

bool CSharpParserBase::IsClassTypeName()
{
    antlr4::Token *t = _input->LT(1);
    if (!t) return true;
    int tt = static_cast<int>(t->getType());
    if (tt == CSharpLexer::KW_OBJECT || tt == CSharpLexer::KW_STRING) return true;
    CSharpSymbol *sym = symTable()->currentScope()->lookupChain(t->getText());
    if (!sym) return true;
    if (sym->kind != CSharpSymbolKind::Type) return true;
    return sym->typeKind != CSharpTypeKind::Interface && sym->typeKind != CSharpTypeKind::Delegate;
}

bool CSharpParserBase::classBaseTypeCheck(bool wantInterface)
{
    antlr4::Token *t = _input->LT(2);  // LT(1)=':' LT(2)=type name
    if (!t) return !wantInterface;
    int tt = static_cast<int>(t->getType());
    if (tt == CSharpLexer::KW_OBJECT || tt == CSharpLexer::KW_STRING)
        return !wantInterface;
    CSharpSymbol *sym = symTable()->currentScope()->lookupChain(t->getText());
    if (!sym) return !wantInterface;
    if (sym->kind != CSharpSymbolKind::Type) return !wantInterface;
    bool isInterface = (sym->typeKind == CSharpTypeKind::Interface);
    return wantInterface ? isInterface : !isInterface;
}

bool CSharpParserBase::IsClassBaseInterfaceList() { return classBaseTypeCheck(true); }
bool CSharpParserBase::IsClassBaseClassType()     { return classBaseTypeCheck(false); }

bool CSharpParserBase::IsExplicitlyTypedLocalVariable()
{
    antlr4::Token *t = _input->LT(1);
    if (!t) return true;
    if (static_cast<int>(t->getType()) != CSharpLexer::KW_VAR) return true;
    CSharpSymbol *sym = symTable()->currentScope()->lookupChain("var");
    if (sym && sym->kind == CSharpSymbolKind::Type) return true;
    antlr4::Token *lt3 = _input->LT(3);
    if (!lt3 || lt3->getText() != "=") return true;
    antlr4::Token *lt4 = _input->LT(4);
    if (lt4 && lt4->getText() == "{") return true;
    return false;
}

bool CSharpParserBase::IsExplicitlyTypedRefLocalVariable()
{
    antlr4::Token *t = _input->LT(1);
    return t != nullptr && static_cast<int>(t->getType()) == CSharpLexer::KW_REF;
}

bool CSharpParserBase::IsImplicitlyTypedLocalVariable()
{
    antlr4::Token *t = _input->LT(1);
    if (!t) return true;
    if (static_cast<int>(t->getType()) != CSharpLexer::KW_VAR) return false;
    CSharpSymbol *sym = symTable()->currentScope()->lookupChain("var");
    if (sym && sym->kind == CSharpSymbolKind::Type) return false;
    antlr4::Token *lt3 = _input->LT(3);
    if (!lt3 || lt3->getText() != "=") return false;
    antlr4::Token *lt4 = _input->LT(4);
    if (lt4 && lt4->getText() == "{") return false;
    return true;
}

// ─── Grammar actions ──────────────────────────────────────────────────────────

void CSharpParserBase::EnterNamespaceScope() { symTable()->enterScope(CSharpScopeKind::Namespace); }
void CSharpParserBase::EnterTypeScope()      { symTable()->enterScope(CSharpScopeKind::Type); }
void CSharpParserBase::EnterBlockScope()     { symTable()->enterScope(CSharpScopeKind::Block); }
void CSharpParserBase::ExitCurrentScope()    { symTable()->exitScope(); }

void CSharpParserBase::OnTypeParameter()
{
    antlr4::ParserRuleContext *ctx = getContext();
    if (!ctx) return;
    std::string name = ctx->children.back()->getText();
    symTable()->declareTypeParam(name);
}

void CSharpParserBase::OnUsingAliasDirective()
{
    antlr4::ParserRuleContext *ctx = getContext();
    if (!ctx || ctx->children.size() < 4) return;
    std::string alias  = ctx->children[1]->getText();
    std::string target = ctx->children[3]->getText();
    symTable()->declareAlias(alias, target);
}

void CSharpParserBase::OnUsingNamespaceDirective()
{
    antlr4::ParserRuleContext *ctx = getContext();
    if (!ctx || ctx->children.size() < 2) return;
    std::string ns = ctx->children[1]->getText();
    symTable()->importNamespace(ns);
}

void CSharpParserBase::BeginVariableDeclaration()
{
    antlr4::ParserRuleContext *ctx = getContext();
    if (!ctx || ctx->children.empty()) return;
    pendingVarType_ = ctx->children.back()->getText();
}

void CSharpParserBase::OnVariableDeclarator()
{
    antlr4::ParserRuleContext *ctx = getContext();
    if (!ctx || ctx->children.empty()) return;
    std::string id = ctx->children.back()->getText();
    symTable()->declareVariable(id, pendingVarType_);
}

// ─── ReduceTree — post-parse cleanup ─────────────────────────────────────────

static void takeOutEmpties(antlr4::ParserRuleContext *node, int ruleTypeArgumentList)
{
    for (int ix = static_cast<int>(node->children.size()) - 1; ix >= 0; ix--)
    {
        auto *child = dynamic_cast<antlr4::ParserRuleContext *>(node->children[ix]);
        if (!child) continue;
        if (child->getRuleIndex() == ruleTypeArgumentList && child->children.size() == 0)
            node->children.erase(node->children.begin() + ix);
        else
            takeOutEmpties(child, ruleTypeArgumentList);
    }
}

static void reducer(antlr4::ParserRuleContext *node, bool reduceAllChildren)
{
    for (auto *ch : node->children)
    {
        auto *childCtx = dynamic_cast<antlr4::ParserRuleContext *>(ch);
        if (childCtx) reducer(childCtx, reduceAllChildren);
    }
    if (reduceAllChildren || node->children.size() == 1)
    {
        for (int i = 0; i < static_cast<int>(node->children.size()); i++)
        {
            auto *child = dynamic_cast<antlr4::ParserRuleContext *>(node->children[i]);
            if (child && child->children.size() == 1)
            {
                auto *grandchild = dynamic_cast<antlr4::ParserRuleContext *>(child->children[0]);
                if (grandchild)
                    node->children[i] = grandchild;
            }
        }
    }
}

void CSharpParserBase::ReduceTree(antlr4::ParserRuleContext *currentctx)
{
    cacheRuleIndices();
    takeOutEmpties(currentctx, ruleTypeArgumentList_);

    const char *env = std::getenv("ANTLR_REDUCE_TREE");
    if (!env || std::string(env) != "yes") return;

    const char *envAll = std::getenv("ANTLR_REDUCE_ALL_CHILDREN");
    bool reduceAllChildren = envAll && std::string(envAll) == "yes";
    reducer(currentctx, reduceAllChildren);
}

// ─── insertNode — rewire parse tree ──────────────────────────────────────────

void CSharpParserBase::insertNode(antlr4::ParserRuleContext *currentctx, const std::string &contextTypeName)
{
    return;
    antlr4::ParserRuleContext *inserted = nullptr;
    int invokingState = currentctx->invokingState;

/*
    if      (contextTypeName == "Invocation_expressionContext")
        inserted = new CSharpParser::Invocation_expressionContext(currentctx, invokingState);
    else if (contextTypeName == "Element_accessContext")
        inserted = new CSharpParser::Element_accessContext(currentctx, invokingState);
    else if (contextTypeName == "Member_accessContext")
        inserted = new CSharpParser::Member_accessContext(currentctx, invokingState);
    else if (contextTypeName == "Null_conditional_member_accessContext")
        inserted = new CSharpParser::Null_conditional_member_accessContext(currentctx, invokingState);
    else if (contextTypeName == "Null_conditional_element_accessContext")
        inserted = new CSharpParser::Null_conditional_element_accessContext(currentctx, invokingState);
    else if (contextTypeName == "Post_increment_expressionContext")
        inserted = new CSharpParser::Post_increment_expressionContext(currentctx, invokingState);
    else if (contextTypeName == "Post_decrement_expressionContext")
        inserted = new CSharpParser::Post_decrement_expressionContext(currentctx, invokingState);
    else if (contextTypeName == "Null_forgiving_expressionContext")
        inserted = new CSharpParser::Null_forgiving_expressionContext(currentctx, invokingState);
    else if (contextTypeName == "Pointer_member_accessContext")
        inserted = new CSharpParser::Pointer_member_accessContext(currentctx, invokingState);

    if (!inserted) return;

    inserted->children = currentctx->children;
    currentctx->children.clear();
    currentctx->children.push_back(inserted);
*/
}

void CSharpParserBase::AsInvocationExpression(antlr4::ParserRuleContext *currentctx)
    { insertNode(currentctx, "Invocation_expressionContext"); }
void CSharpParserBase::AsElementAccess(antlr4::ParserRuleContext *currentctx)
    { insertNode(currentctx, "Element_accessContext"); }
void CSharpParserBase::AsMemberAccess(antlr4::ParserRuleContext *currentctx)
    { insertNode(currentctx, "Member_accessContext"); }
void CSharpParserBase::AsNullConditionalMemberAccess(antlr4::ParserRuleContext *currentctx)
    { insertNode(currentctx, "Null_conditional_member_accessContext"); }
void CSharpParserBase::AsNullConditionalElementAccess(antlr4::ParserRuleContext *currentctx)
    { insertNode(currentctx, "Null_conditional_element_accessContext"); }
void CSharpParserBase::AsPostIncrementExpression(antlr4::ParserRuleContext *currentctx)
    { insertNode(currentctx, "Post_increment_expressionContext"); }
void CSharpParserBase::AsPostDecrementExpression(antlr4::ParserRuleContext *currentctx)
    { insertNode(currentctx, "Post_decrement_expressionContext"); }
void CSharpParserBase::AsNullForgivingExpression(antlr4::ParserRuleContext *currentctx)
    { insertNode(currentctx, "Null_forgiving_expressionContext"); }
void CSharpParserBase::AsPointerMemberAccess(antlr4::ParserRuleContext *currentctx)
    { insertNode(currentctx, "Pointer_member_accessContext"); }

// ─── ElementAccessSemanticCheck ───────────────────────────────────────────────

void CSharpParserBase::ElementAccessSemanticCheck(antlr4::ParserRuleContext *currentctx)
{
    cacheRuleIndices();

    if (currentctx->getRuleIndex() != rulePrimaryExpression_
        || static_cast<int>(currentctx->children.size()) != 1)
        return;

    auto *childTree = dynamic_cast<antlr4::ParserRuleContext *>(currentctx->children[0]);
    if (!childTree) return;

    int childRuleIndex  = childTree->getRuleIndex();
    int childChildCount = static_cast<int>(childTree->children.size());

    if (childRuleIndex == ruleElementAccess_ || childRuleIndex == rulePointerElementAccess_)
    {
        if (childChildCount != 4) return;
    }
    else if (childRuleIndex == ruleNullCondElementAccess_)
    {
        if (childChildCount != 5) return;
    }
    else return;

    auto *accessTarget = dynamic_cast<antlr4::ParserRuleContext *>(childTree->children[0]);
    if (!accessTarget) return;
    if (accessTarget->getRuleIndex() != rulePrimaryExpression_
        || accessTarget->children.size() == 0)
        return;

    auto *lhsTarget = dynamic_cast<antlr4::ParserRuleContext *>(accessTarget->children[0]);
    if (!lhsTarget) return;

    if (lhsTarget->getRuleIndex() != ruleArrayCreationExpression_
        && lhsTarget->getRuleIndex() != ruleStackallocExpression_)
        return;

    antlr4::Token *lhsLast = lhsTarget->stop;
    if (!lhsLast) return;
    int lhsLastType = static_cast<int>(lhsLast->getType());

    if (lhsLastType == CSharpLexer::TK_RBRACE) return;

    if (lhsLastType != CSharpLexer::TK_RBRACK)
    {
        std::cerr << lhsLast->getLine() << ":" << lhsLast->getCharPositionInLine()
                  << " Error: Unexpected LHS last token " << lhsLast->getText()
                  << " (" << lhsLastType << ").\n";
        return;
    }

    const auto &names = getRuleNames();
    const std::string &childRuleName = names[childRuleIndex];
    const std::string &lhsRuleName   = names[lhsTarget->getRuleIndex()];

    static const std::string vowels = "AEIOUaeiou";
    std::string childPrefix = vowels.find(childRuleName[0]) != std::string::npos ? "an" : "a";
    std::string lhsPrefix   = vowels.find(lhsRuleName[0])  != std::string::npos ? "an" : "a";

    notifySemanticError(
        static_cast<int>(lhsLast->getLine()),
        lhsLast->getCharPositionInLine(),
        "LHS of " + childPrefix + " " + childRuleName +
        " cannot be " + lhsPrefix + " " + lhsRuleName +
        " unless it has an initializer");
}

// Gates comma-separated declarators: false when type is 'var' (only one declarator allowed).
bool CSharpParserBase::IsLocalVariableDeclaration()
{
    auto *ctx = dynamic_cast<CSharpParser::Local_variable_declarationContext *>(getRuleContext());
    if (ctx == nullptr)
        return true;
    auto *local_variable_type = ctx->local_variable_type();
    if (local_variable_type == nullptr)
        return true;
    return local_variable_type->getText() != "var";
}
