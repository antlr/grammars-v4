/**
 * c_grammar_cpp_test
 *
 * usage:
 *   c_grammar_cpp_test [options] <file.c>
 *
 * Options:
 *   --nopp     jump preprocess
 *   --gcc      use "gcc -E" preprocess
 *   --clang    use "clang -E" preprocess
 *   --tree     print parse tree
 */

#include "antlr4-runtime.h"
#include "CLexer.h"
#include "CParser.h"
#include "CLexerBase.h"
#include "CParserBase.h"

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

static void usage(const char *prog) {
    std::cerr << "Usage: " << prog
              << " [--nopp|--gcc|--clang] [--tree] <file.c>\n";
}

int main(int argc, char *argv[]) {
    std::vector<std::string> args;
    std::string inputFile;
    bool showTree = false;

    for (int i = 1; i < argc; ++i) {
        std::string a = argv[i];
        if (a == "--tree") {
            showTree = true;
        } else if (a.starts_with("--")) {
            args.push_back(a);
        } else {
            inputFile = a;
        }
    }

    if (inputFile.empty()) {
        usage(argv[0]);
        return 1;
    }

    // read src
    std::ifstream ifs(inputFile);
    if (!ifs) {
        std::cerr << "Cannot open file: " << inputFile << "\n";
        return 1;
    }
    std::string source((std::istreambuf_iterator<char>(ifs)),
                        std::istreambuf_iterator<char>());

    CLexerBase::setArgs(args);

    // Lexer
    antlr4::ANTLRInputStream input(source);
    input.name = inputFile;
    CLexer lexer(&input);

    antlr4::CommonTokenStream tokens(&lexer);

    // Parser
    CParser parser(&tokens);

    auto *tree = parser.compilationUnit();

    // result:
    size_t syntaxErrors = parser.getNumberOfSyntaxErrors();

    if (showTree) {
        std::cout << tree->toStringTree(&parser, true) << "\n";
    }

    if (syntaxErrors > 0) {
        std::cerr << "[FAIL] " << inputFile << ": "
                  << syntaxErrors << " syntax error(s)\n";
        return 1;
    }

    std::cout << "[OK] " << inputFile << ": parsed successfully\n";
    return 0;
}
