#pragma once

#include "antlr4-runtime.h"
#include <memory>

template<typename CLS>
class unowned_unique_ptr  {
public:
    unowned_unique_ptr(CLS* cls) {
        ptr.reset(cls);
    }
    ~unowned_unique_ptr()
    {
        ptr.release();
    }

    operator std::unique_ptr<CLS>&() {
        return ptr;
    }

    CLS* operator ->() {
        return ptr.get();
    }

    unowned_unique_ptr& operator =(CLS* cls) {
        ptr.release();
        ptr.reset(cls);
        return *this;
    }
private:
    std::unique_ptr<CLS> ptr;
};

class String {
public:
    String(const std::string&& str)
        : str(std::move(str))
    {

    }

    const char* data() const {
        return str.data();
    }

    size_t size() const {
        return str.size();
    }

    bool contains(const char* sub) const {
        return str.find(sub) != std::string::npos;
    }

    bool equals(const char* other) const {
        return str == std::string(other);
    }
private:
    const std::string str;
};

class SwiftSupport : public antlr4::Parser {

    class OperatorHead : public std::bitset<0xE0101> {
    public:
        OperatorHead();

        bool get(std::size_t pos) const {
            return bitset::test(pos);
        }
    };

    class OperatorCharacter : public OperatorHead {
    public:
        OperatorCharacter();
    };

    class LeftWS : public std::bitset<255> {
    public:
        LeftWS();

        bool get(std::size_t pos) const {
            return bitset::test(pos);
        }
    };

    class RightWS : public std::bitset<255> {
    public:
        RightWS();

        bool get(std::size_t pos) const {
            return bitset::test(pos);
        }
    };

public:
    SwiftSupport(antlr4::TokenStream* input) : Parser(input) { }

    static const OperatorHead operatorHead;
    static const OperatorCharacter operatorCharacter;
    static const LeftWS leftWS;
    static const RightWS rightWS;

    static bool isOperatorHead(std::unique_ptr<antlr4::Token>& token);
    static bool isOperatorCharacter(std::unique_ptr<antlr4::Token>& token);
    static bool isOpNext(antlr4::TokenStream*);
    static int getLastOpTokenIndex(antlr4::TokenStream*);
    static bool isBinaryOp(antlr4::TokenStream*);
    static bool isPrefixOp(antlr4::TokenStream*);
    static bool isPostfixOp(antlr4::TokenStream*);
    static bool isOperator(antlr4::TokenStream* tokens, const char* op);
    static bool isLeftOperatorWS(std::unique_ptr<antlr4::Token>& t);
    static bool isRightOperatorWS(std::unique_ptr<antlr4::Token>& t);
    static bool isSeparatedStatement(antlr4::TokenStream* tokens, int indexOfPreviousStatement);
    static void fillUp(antlr4::TokenStream* tokens);
};
