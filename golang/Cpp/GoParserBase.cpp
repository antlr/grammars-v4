#include "GoParserBase.h"
#include "GoParser.h"
#include <algorithm>
#include <cctype>

bool GoParserBase::hasArg(int argc, char* argv[], const std::string& arg)
{
    std::string argLower = arg;
    std::transform(argLower.begin(), argLower.end(), argLower.begin(),
                   [](unsigned char c){ return std::tolower(c); });
    for (int i = 0; i < argc; i++) {
        std::string a = argv[i];
        std::transform(a.begin(), a.end(), a.begin(),
                       [](unsigned char c){ return std::tolower(c); });
        if (a.find(argLower) != std::string::npos) {
            return true;
        }
    }
    return false;
}

GoParserBase::GoParserBase(antlr4::TokenStream* input, int argc, char* argv[])
    : Parser(input)
{
    debug = hasArg(argc, argv, "--debug");
    if (debug) {
        std::cout << "debug = " << debug << std::endl;
    }
}

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
    auto importSpec = dynamic_cast<GoParser::ImportSpecContext*>(ctx);
    if (importSpec == nullptr) return;
    auto packageName = importSpec->packageName();
    if (packageName != nullptr)
    {
        auto name = packageName->getText();
        if (debug) std::cout << "Entering " << name << std::endl;
        table.insert(name);
        return;
    }
    auto importPath = importSpec->importPath();
    if (importPath == nullptr) return;
    auto name = importPath->getText();
    if (debug) std::cout << "import path " << name << std::endl;
    name.erase(std::remove(name.begin(), name.end(), '\"'), name.end());
    if (name.empty()) return;
    std::replace(name.begin(), name.end(), '\\', '/');
    auto pathArr = split(name, '/');
    if (pathArr.empty()) return;
    auto lastComponent = pathArr[pathArr.size()-1];
    if (lastComponent.empty()) return;
    // Handle special cases like "." and ".."
    if (lastComponent == "." || lastComponent == "..") return;
    auto fileArr = split(lastComponent, '.');
    // Guard against empty array (can happen if lastComponent is all dots)
    if (fileArr.empty()) {
        table.insert(lastComponent);
        if (debug) std::cout << "Entering " << lastComponent << std::endl;
        return;
    }
    auto fileName = fileArr[fileArr.size()-1];
    if (fileName.empty()) {
        // Fall back to lastComponent if split resulted in empty string
        fileName = lastComponent;
    }
    if (debug) std::cout << "Entering " << fileName << std::endl;
    table.insert(fileName);
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

// Built-in functions that take a type as first argument
const std::set<std::string> GoParserBase::BUILTIN_TYPE_FUNCTIONS = {"make", "new"};

// Check if we're in a call to a built-in function that takes a type as first argument.
// Called after L_PAREN has been matched in the arguments rule.
bool GoParserBase::isTypeArgument()
{
    antlr4::BufferedTokenStream* stream = static_cast<antlr4::BufferedTokenStream*>(_input);
    // After matching L_PAREN, LT(-1) is '(' and LT(-2) is the token before it
    auto funcToken = stream->LT(-2);
    if (funcToken == nullptr || funcToken->getType() != GoParser::IDENTIFIER) {
        if (debug) std::cout << "isTypeArgument Returning false - no identifier before (" << std::endl;
        return false;
    }
    bool result = BUILTIN_TYPE_FUNCTIONS.find(funcToken->getText()) != BUILTIN_TYPE_FUNCTIONS.end();
    if (debug) std::cout << "isTypeArgument Returning " << result << " for " << funcToken->getText() << std::endl;
    return result;
}

// Check if we're NOT in a call to a built-in function that takes a type.
// This is the inverse of isTypeArgument for the expressionList alternative.
bool GoParserBase::isExpressionArgument()
{
    bool result = !isTypeArgument();
    if (debug) std::cout << "isExpressionArgument Returning " << result << std::endl;
    return result;
}

