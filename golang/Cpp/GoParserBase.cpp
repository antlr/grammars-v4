#include "GoParserBase.h"
#include "GoParser.h"

void GoParserBase::myreset()
{
    table.clear();
}

bool GoParserBase::closingBracket()
{
    antlr4::BufferedTokenStream* stream = static_cast<antlr4::BufferedTokenStream*>(_input);
    auto la = stream->LT(1);
    return la->getType() == GoParser::R_CURLY || la->getType() == GoParser::R_PAREN || la->getType() == antlr4::Token::EOF;
}

bool GoParserBase::isNotReceive()
{
    antlr4::BufferedTokenStream* stream = static_cast<antlr4::BufferedTokenStream*>(_input);
    auto la = stream->LT(2);
    return la->getType() != GoParser::RECEIVE;
}

std::vector<std::string> split(const std::string& str, char delimiter)
{
    std::vector<std::string> tokens;
    std::stringstream ss(str);
    std::string token;

    while (std::getline(ss, token, delimiter))
    {
        tokens.push_back(token);
    }

    return tokens;
}

void GoParserBase::addImportSpec()
{
    antlr4::ParserRuleContext* ctx = this->_ctx;
    auto count = ctx->children.size();
    auto importSpec = dynamic_cast<GoParser::ImportSpecContext*>(ctx);
    if (importSpec == nullptr) return;
    auto packageName = importSpec->packageName();
    if (packageName != nullptr)
    {
        auto name = packageName->getText();
        if (debug) std::cout << "Entering " << name;
        table.insert(name);
    }
    else
    {
        auto name = importSpec->importPath()->getText();
        name.erase(std::remove(name.begin(), name.end(), '\"'), name.end());
        std::replace(name.begin(), name.end(), '\\', '/');
        auto pathArr = split(name, '/');
        auto fileArr = split(pathArr[pathArr.size()-1], '.');
        auto fileName = fileArr[fileArr.size()-1];
        if (this->debug) std::cout << "Entering " << fileName << std::endl;
        table.insert(fileName);
    }
}

bool GoParserBase::isOperand()
{
    antlr4::BufferedTokenStream* stream = static_cast<antlr4::BufferedTokenStream*>(_input);
    auto la = stream->LT(1);
    if (la->getText() == "err") return true;
    bool result = true;
    if (la->getType() != GoParser::IDENTIFIER) {
        if (debug) std::cout << "isOperand Returning " << result << " for " << la << std::endl;
        return result;
    }
    result = table.find(la->getText()) != table.end();
    auto la2 = stream->LT(2);
    // If it's not followed by a '.', then it really should be
    // considered as operand.
    if (la2->getType() != GoParser::DOT) {
        result = true;
        if (debug) std::cout << "isOperand Returning " << result << " for " << la << std::endl;
        return result;
    }
    // If it's followed by '.', and then followed by '(', then
    // it is a typeAssertion, and so la must be an operand.
    auto la3 = stream->LT(3);
    if (la3->getType() == GoParser::L_PAREN)
    {
        result = true;
        if (debug) std::cout << "isOperand Returning " << result << " for " << la << std::endl;
        return result;
    }
    if (debug) std::cout << "isOperand Returning " << result << " for " << la << std::endl;
    return result;
}

bool GoParserBase::isConversion()
{
    antlr4::BufferedTokenStream* stream = static_cast<antlr4::BufferedTokenStream*>(_input);
    auto la = stream->LT(1);
    auto result = la->getType() != GoParser::IDENTIFIER;
    if (debug) std::cout << "isConversion Returning " << result << " for " << la << std::endl;
    return result;
}

bool GoParserBase::isMethodExpr()
{
    antlr4::BufferedTokenStream* stream = static_cast<antlr4::BufferedTokenStream*>(_input);
    auto la = stream->LT(1);
    bool result = true;
    // See if it looks like a method expr.
    if (la->getType() == GoParser::STAR) {
        if (debug) std::cout << "isMethodExpr Returning " << result << " for " << la << std::endl;
        return result;
    }
    if (la->getType() != GoParser::IDENTIFIER) {
        result = false;
        if (debug) std::cout << "isMethodExpr Returning " << result << " for " << la << std::endl;
        return result;
    }
    result = ! (table.find(la->getText()) != table.end());
    if (debug) std::cout << "isMethodExpr Returning " << result << " for " << la << std::endl;
    return result;
}

