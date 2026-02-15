#include "antlr4-runtime.h"
#include "AdaParserBase.h"
#include "AdaLexer.h"
#include "AdaParser.h"

AdaParserBase::AdaParserBase(antlr4::TokenStream * input) : antlr4::Parser(input)
{
}

void AdaParserBase::ParsePragmas()
{
    auto stream = dynamic_cast<antlr4::BufferedTokenStream *>(getTokenStream());
    if (!stream) return;
    stream->fill();
    auto allTokens = stream->getTokens();
    const int PRAGMA_CHANNEL = 2;
    std::vector<antlr4::Token *> *currentPragma = nullptr;
    std::vector<std::vector<antlr4::Token *>> pragmas;
    for (auto token : allTokens) {
        if (static_cast<int>(token->getChannel()) != PRAGMA_CHANNEL) continue;
        if (token->getType() == AdaLexer::PRAGMA) {
            pragmas.push_back({});
            currentPragma = &pragmas.back();
            currentPragma->push_back(token);
        } else if (currentPragma != nullptr) {
            currentPragma->push_back(token);
            if (token->getType() == AdaLexer::SEMI) {
                currentPragma = nullptr;
            }
        }
    }
    for (auto &pragmaTokens : pragmas) {
        std::vector<std::unique_ptr<antlr4::CommonToken>> ownedTokens;
        std::vector<std::unique_ptr<antlr4::Token>> sourceTokens;
        for (auto t : pragmaTokens) {
            auto ct = std::make_unique<antlr4::CommonToken>(
                t->getType(), t->getText());
            ct->setLine(t->getLine());
            ct->setCharPositionInLine(t->getCharPositionInLine());
            ct->setChannel(antlr4::Token::DEFAULT_CHANNEL);
            ct->setStartIndex(t->getStartIndex());
            ct->setStopIndex(t->getStopIndex());
            ct->setTokenIndex(t->getTokenIndex());
            ownedTokens.push_back(std::move(ct));
        }
        auto eof = std::make_unique<antlr4::CommonToken>(
            antlr4::Token::EOF, "");
        eof->setChannel(antlr4::Token::DEFAULT_CHANNEL);
        ownedTokens.push_back(std::move(eof));
        for (auto &ct : ownedTokens) {
            sourceTokens.push_back(std::unique_ptr<antlr4::Token>(ct.release()));
        }
        auto tokenSource = std::make_unique<antlr4::ListTokenSource>(std::move(sourceTokens));
        auto tokenStream = std::make_unique<antlr4::CommonTokenStream>(tokenSource.get());
        AdaParser parser(tokenStream.get());
        parser.removeErrorListeners();
        parser.addErrorListener(&getErrorListenerDispatch());
        parser.pragmaRule();
    }
}
