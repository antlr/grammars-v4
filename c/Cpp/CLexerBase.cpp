#include "CLexerBase.h"

#include <algorithm>
#include <array>
#include <cstdio>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>

CLexerBase::CLexerBase(antlr4::CharStream *input)
    : CLexerBase(input, runGccAndMakeStream(input)) {}

CLexerBase::CLexerBase(antlr4::CharStream *original,
                       std::unique_ptr<antlr4::ANTLRInputStream> preprocessed)
    : antlr4::Lexer(preprocessed ? preprocessed.get() : original)
    , preprocessedStream_(std::move(preprocessed)) {}

//  Static configuration

void CLexerBase::setArgs(const std::vector<std::string>& args) {
    args_ = args;
}

//  runGccAndMakeStream()

std::unique_ptr<antlr4::ANTLRInputStream>
CLexerBase::runGccAndMakeStream(antlr4::CharStream *input)
{
    bool vsc   = hasArg("--vsc");
    bool gcc   = hasArg("--gcc");
    bool clang = hasArg("--clang");
    bool nopp  = hasArg("--nopp");

    // Default to gcc
    if (!(vsc || gcc || clang)) {
        gcc = true;
    }

    auto ppOptions = extractPreprocessorOptions();

    // ---- source name / text -----------------------------------------------
    std::string sourceName = input->getSourceName();
    if (sourceName.empty() || sourceName.size() < 2 ||
        sourceName.substr(sourceName.size() - 2) != ".c")
    {
        sourceName = "stdin.c";
    }
    std::string outputName = sourceName + ".p";
    std::string inputText;
    auto inputSize = input->size();
    if (inputSize > 0) {
        inputText = input->getText(
            antlr4::misc::Interval(0, static_cast<ssize_t>(inputSize) - 1));
    }

    // ---- --nopp -----------------------------------------------------------
    if (nopp) {
        { std::ofstream out(outputName); if (out) out << inputText; }
        auto stream = std::make_unique<antlr4::ANTLRInputStream>(inputText);
        stream->name = sourceName;
        return stream;
    }

    // Persist stdin text so external preprocessor can read it.
    if (sourceName == "stdin.c") {
        std::ofstream out(sourceName);
        if (out) out << inputText;
    }

    // ---- gcc / clang (external) -------------------------------------------
    if (gcc || clang) {
        std::string cmd = gcc ? "gcc" : "clang";

        std::ostringstream cmdline;
        cmdline << cmd << " -std=c2x -E -C";
        for (auto& opt : ppOptions) {
            cmdline << " " << opt;
        }
        cmdline << " " << sourceName << " 2>/dev/null";

        FILE *pipe = popen(cmdline.str().c_str(), "r");
        if (!pipe) {
            throw std::runtime_error(
                std::string("Failed to run ") + cmd + " preprocessor");
        }

        std::ostringstream captured;
        std::array<char, 4096> buf{};
        while (std::fgets(buf.data(), static_cast<int>(buf.size()), pipe)) {
            captured << buf.data();
        }
        int rc = pclose(pipe);
        (void)rc;

        std::string preprocessed = captured.str();

        { std::ofstream out(outputName); if (out) out << preprocessed; }

        auto stream = std::make_unique<antlr4::ANTLRInputStream>(preprocessed);
        stream->name = sourceName;
        return stream;
    }

    // Note: vsc (Visual Studio cl.exe) is Windows-specific; not ported.
    throw std::runtime_error("No preprocessor specified.");
}

//  Argument helpers

bool CLexerBase::hasArg(const std::string& arg) {
    std::string lower_arg = arg;
    std::transform(lower_arg.begin(), lower_arg.end(),
                   lower_arg.begin(), ::tolower);
    for (auto& a : args_) {
        std::string lower_a = a;
        std::transform(lower_a.begin(), lower_a.end(),
                       lower_a.begin(), ::tolower);
        if (lower_a.find(lower_arg) != std::string::npos) {
            return true;
        }
    }
    return false;
}

std::vector<std::string> CLexerBase::extractPreprocessorOptions() {
    std::vector<std::string> options;
    for (auto& arg : args_) {
        if (arg.starts_with("--D")) {
            options.push_back("-D" + arg.substr(3));
        } else if (arg.starts_with("--I")) {
            options.push_back("-I" + arg.substr(3));
        }
    }
    return options;
}