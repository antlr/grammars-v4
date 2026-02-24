#pragma once

#include "antlr4-runtime.h"
#include "SymbolTable.h"
#include "Symbol.h"
#include "TypeClassification.h"
#include <string>
#include <unordered_set>
#include <iostream>
#include <vector>
#include <memory>

// Forward declare generated parser
class CLexer;
class CParser;

class CParserBase : public antlr4::Parser {
public:
    CParserBase(antlr4::TokenStream *input);

    // Semantic predicate methods called from grammar actions
    bool IsAlignmentSpecifier(int k = 1);
    bool IsAtomicTypeSpecifier(int k = 1);
    bool IsAttributeDeclaration();
    bool IsAttributeSpecifier();
    bool IsAttributeSpecifierSequence();
    bool IsDeclaration();
    bool IsDeclarationSpecifier();
    bool IsTypeSpecifierQualifier(int k = 1);
    bool IsDeclarationSpecifiers();
    bool IsEnumSpecifier(int k = 1);
    bool IsFunctionSpecifier();
    bool IsGnuAttributeBeforeDeclarator(int k = 1);
    bool IsStatement();
    bool IsStaticAssertDeclaration();
    bool IsStorageClassSpecifier();
    bool IsStructOrUnionSpecifier(int k = 1);
    bool IsTypedefName(int k = 1);
    bool IsTypeofSpecifier(int k = 1);
    bool IsTypeQualifier(int k = 1);
    bool IsTypeSpecifier(int k = 1);
    bool IsSomethingOfTypeName();
    bool IsTypeName(int k = 1);
    bool IsSpecifierQualifierList(int k = 1);
    bool IsCast();
    bool IsNullStructDeclarationListExtension();

    // Action methods called from grammar
    void EnterDeclaration();
    void EnterScope();
    void ExitScope();
    void LookupSymbol();
    void OutputSymbolTable();

    // Expose symbol table for external use
    SymbolTable& getSymbolTable() { return _st; }

protected:
    SymbolTable _st;
    bool debug_ = false;
    bool outputSymbolTable_ = false;
    bool outputAppliedOccurrences_ = false;
    std::unordered_set<std::string> noSemantics_;

private:
    struct SourceLocation {
        std::string file;
        int line;
        int column;
    };

    std::shared_ptr<Symbol> resolveWithOutput(antlr4::Token *token);
    SourceLocation getSourceLocation(antlr4::Token *token);
    antlr4::Token* getDeclarationToken(antlr4::ParserRuleContext *declaratorCtx);
    std::string getDeclarationId(antlr4::ParserRuleContext *declaratorCtx);
};
