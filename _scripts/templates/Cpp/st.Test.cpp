// Generated from trgen <version>

#include \<iostream>
#include \<iomanip>
#include \<string>
#include \<chrono>
#include \<atomic>
#include \<vector>
#include "ANTLRInputStream.h"
#include "ErrorListener.h"
<tool_grammar_tuples:{x | #include "<x.GeneratedIncludeFileName>"
} >
#include "EncodingInputStream.h"

std::string formatDuration(uint64_t duration) {
    std::stringstream oss;
    // duration is in microseconds units.
    long tseconds = duration / 1000000;
    long minutes = tseconds / 60;
    long seconds = tseconds % 60;
    long microseconds = duration % 1000000;
    oss \<\< std::setfill('0')
        \<\< minutes
        \<\< ":"
        \<\< std::setw(2)
        \<\< seconds
        \<\< "."
        \<\< std::setw(6)
        \<\< microseconds;
    return oss.str();
}

std::string formatDurationSeconds(uint64_t duration) {
    std::stringstream oss;
    // duration is in microseconds units.
    long tseconds = duration / 1000000;
    long minutes = tseconds / 60;
    long seconds = tseconds % 60;
    long microseconds = duration % 1000000;
    double s = minutes * 60.0 + seconds + (microseconds / 1000000.0);
    oss \<\< s;
    return oss.str();
}

bool tee = false;
bool show_tree = false;
bool show_tokens = false;
bool show_trace = false;
std::vector\<std::string> inputs;
std::vector\<bool> is_fns;
int error_code = 0;
int string_instance = 0;
std::string prefix;
bool quiet = false;
std::string file_encoding = "<file_encoding>";
long total_tokens = 0;
double total_parse_seconds = 0;
long first_file_tokens = 0;
double first_file_parse_seconds = 0;

void DoParse(antlr4::CharStream* str, std::string input_name, int row_number)
{
    antlr4::Lexer* lexer = new <lexer_name>(str);
    if (show_tokens)
    {
        for (int i = 0; ; ++i)
        {
            auto token = lexer->nextToken();
            auto ctoken = (antlr4::CommonToken*)(token.get());
            ctoken->setTokenIndex(i);
            std::cerr \<\< token->toString() \<\< std::endl;
            if (token->getType() == antlr4::IntStream::EOF)
                break;
        }
        lexer->reset();
    }
    auto tokens = new antlr4::CommonTokenStream(lexer);
    auto* parser = new <parser_name>(tokens);
    std::ostream* output = tee
        ? new std::ofstream(input_name + ".errors")
        : &std::cerr;
    auto listener_lexer = new ErrorListener(quiet, tee, output);
    auto listener_parser = new ErrorListener(quiet, tee, output);
    lexer->removeErrorListeners();
    parser->removeErrorListeners();
    lexer->addErrorListener(listener_lexer);
    parser->addErrorListener(listener_parser);
    if (show_trace)
    {
        parser->setTrace(true);
        // Missing ATN trace.
    }
    auto before = std::chrono::steady_clock::now();
    auto* tree = parser-><start_symbol>();
    auto after = std::chrono::steady_clock::now();
    long token_count = (long)tokens->size();
    total_tokens += token_count;
    auto duration = std::chrono::duration_cast\<std::chrono::microseconds>(after - before);
    double parse_seconds = duration.count() / 1000000.0;
    total_parse_seconds += parse_seconds;
    if (row_number == 0) {
        first_file_tokens = token_count;
        first_file_parse_seconds = parse_seconds;
    }
    std::string result;
    if (listener_parser->had_error || listener_lexer->had_error)
    {
        result = "fail";
        error_code = 1;
    }
    else
    {
        result = "success";
    }
    if (show_tree)
    {
        if (tee)
        {
            try {
                auto fn = input_name + ".tree";
                auto out = new std::ofstream(fn);
                (*out) \<\< tree->toStringTree(parser);
                delete out;
            }
            catch (...) {
            }
        }
        else
        {
            std::cerr \<\< tree->toStringTree(parser) \<\< std::endl;
        }
    }
    if (!quiet)
    {
        std::cerr \<\< prefix \<\< "Cpp " \<\< row_number \<\< " " \<\< input_name \<\< " " \<\< result \<\< " " \<\< parse_seconds \<\< " s " \<\< token_count \<\< " tokens " \<\< (long)(token_count / parse_seconds) \<\< " tps" \<\< std::endl;
    }
    if (tee)
    {
        delete output;
    }
}


void ParseStdin()
{
    antlr4::CharStream* str = nullptr;
    str = new antlr4::ANTLRInputStream(std::cin);
    DoParse(str, "stdin", 0);
}

void ParseString(std::string input, int row_number)
{
    antlr4::CharStream* str = nullptr;
    str = new antlr4::ANTLRInputStream(input);
    DoParse(str, "string" + string_instance++, row_number);
}

void ParseFilename(std::string input, int row_number)
{
    antlr4::CharStream* str = nullptr;
    std::ifstream ifs(input, std::ios::binary);
    str = new EncodingInputStream(ifs, file_encoding);
    static_cast\<EncodingInputStream*>(str)->name = input;
    DoParse(str, input, row_number);
}

int TryParse(std::vector\<std::string>& args)
{
    for (int i = 0; i \< args.size(); ++i)
    {
        if (args[i] == "-tokens")
        {
            show_tokens = true;
        }
        else if (args[i] == "-tree")
        {
            show_tree = true;
        }
        else if (args[i] == "-prefix")
        {
            prefix = args[++i] + " ";
        }
        else if (args[i] == "-input")
        {
            ++i;
            inputs.push_back(args[i]);
            is_fns.push_back(false);
        }
        else if (args[i] == "-tee")
        {
            tee = true;
        }
        else if (args[i] == "-x")
        {
            for (; ; )
            {
                std::string line;
                if (! std::getline(std::cin, line)) break;
                std::string_view v = line;
                v.remove_prefix(std::min(v.find_first_not_of(" "), v.size()));
                if (line == "")
                {
                    break;
                }
                inputs.push_back(line);
                is_fns.push_back(true);
            }
        }
        else if (args[i] == "-q")
        {
            quiet = true;
        }
        else if (args[i] == "-trace")
        {
            show_trace = true;
        }
        else if (args[i][0] == '-')
        {
            // Ignore unknown option.
        }
        else
        {
            inputs.push_back(args[i]);
            is_fns.push_back(true);
        }
    }
    if (inputs.size() == 0)
    {
        ParseStdin();
    }
    else
    {
        auto before = std::chrono::steady_clock::now();
        for (int f = 0; f \< inputs.size(); ++f)
        {
            try
            {
                if (is_fns[f])
                    ParseFilename(inputs[f], f);
                else
                    ParseString(inputs[f], f);
            }
            catch (const std::runtime_error& e)
            {
                std::cerr \<\< e.what() \<\< std::endl;
                error_code = 1;
            }
            catch (...)
            {
                std::cerr \<\< "unknown exception" \<\< std::endl;
                error_code = 1;
            }
        }
        auto after = std::chrono::steady_clock::now();
        auto duration = std::chrono::duration_cast\<std::chrono::microseconds>(after - before);
        if (!quiet) {
            double overall_seconds = duration.count() / 1000000.0;
            long warm_tokens = total_tokens - first_file_tokens;
            double warm_seconds = total_parse_seconds - first_file_parse_seconds;
            std::string warm_tps = (inputs.size() > 1 && warm_seconds > 0)
                ? std::to_string((long)(warm_tokens / warm_seconds))
                : "n.a.";
            double first_tps = first_file_parse_seconds > 0 ? (first_file_tokens / first_file_parse_seconds) : 0;
            std::ostringstream speedup_ss;
            if (inputs.size() > 1 && warm_seconds > 0 && first_tps > 0)
                speedup_ss \<\< std::fixed \<\< std::setprecision(2) \<\< ((warm_tokens / warm_seconds) / first_tps);
            else
                speedup_ss \<\< "n.a.";
            std::cerr \<\< prefix \<\< "PT: " \<\< total_parse_seconds \<\< std::endl;
            std::cerr \<\< prefix \<\< "OT: " \<\< (overall_seconds - total_parse_seconds) \<\< std::endl;
            std::cerr \<\< prefix \<\< "TT: " \<\< overall_seconds \<\< std::endl;
            std::cerr \<\< prefix \<\< "TPS: " \<\< (long)(total_tokens / total_parse_seconds) \<\< std::endl;
            std::cerr \<\< prefix \<\< "Post-warmup TPS: " \<\< warm_tps \<\< std::endl;
            std::cerr \<\< prefix \<\< "Post-warmup speed up: " \<\< speedup_ss.str() \<\< std::endl;
        }
    }
    return error_code;
}

int main(int argc, const char * argv[])
{
    std::vector \<std::string> args;
    for (int i = 1; i \< argc; ++i)
    {
        args.push_back(argv[i]);
    }   
    return TryParse(args);
}

