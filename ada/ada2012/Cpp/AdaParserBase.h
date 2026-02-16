#pragma once
#include "antlr4-runtime.h"
#include "SymbolTable.h"
#include <vector>
#include <set>
#include <string>

class AdaParserBase : public antlr4::Parser
{
public:
    AdaParserBase(antlr4::TokenStream * input);
    void ParsePragmas();

    bool IsAggregate();
    bool IsTypeName();
    void EnterDeclaration();
    void EnterScope();
    void ExitScope();
    void PushExpectedType();
    void PopExpectedType();
    void OutputSymbolTable();

private:
    SymbolTable _st;
    std::vector<Symbol*> _expectedTypeStack;
    bool debug = false;
    bool outputSymbolTableFlag = false;
    bool outputAppliedOccurrences = false;
    std::set<std::string> noSemantics;

    void defineSymbol(const std::string& name, TypeClassification classification, antlr4::Token* token, bool isComposite = false);
    void defineSubprogramFromSpec(antlr4::ParserRuleContext* spec);
};
