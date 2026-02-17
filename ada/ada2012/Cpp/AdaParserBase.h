#pragma once
#include "antlr4-runtime.h"
#include "SymbolTable.h"
#include <vector>
#include <set>
#include <string>
#include <fstream>
#include <sstream>
#include <map>
#include <cstdlib>

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
    void ImportWithClause();
    std::string currentFile;

private:
    SymbolTable _st;
    std::vector<Symbol*> _expectedTypeStack;
    bool debug = false;
    bool outputSymbolTableFlag = false;
    bool outputAppliedOccurrences = false;
    std::set<std::string> noSemantics;
    std::vector<std::string> _searchPaths;
    static std::map<std::string, std::vector<Symbol>> _packageCache;
    static std::set<std::string> _parsingInProgress;

    void defineSymbol(const std::string& name, TypeClassification classification, antlr4::Token* token, bool isComposite = false);
    void defineSubprogramFromSpec(antlr4::ParserRuleContext* spec);
    std::string packageNameToFileName(const std::string& packageName);
    std::string findAdsFile(const std::string& fileName);
    std::vector<Symbol> parseAdsFile(const std::string& adsPath);
    static std::string toLowerStr(const std::string& s);
    static std::string getFullPath(const std::string& p);
    static std::string getDirName(const std::string& p);
    static bool fileExists(const std::string& p);
};
