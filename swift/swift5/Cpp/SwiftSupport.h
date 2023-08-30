#pragma once

#include "antlr4-runtime.h"
#include <memory>

/*
* This is a helper class to have a unique_ptr when antlr returns raw self owned pointers.
*/
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

/*
* This is a helper class to be close to the Java version.
*/
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

    /*
     * Java allows to call
     *      set(int, bool)
     *      set(int, int)
     *      set(int, int, bool)
     *  This class is here to have the same for c++.
     *  Beware there are no error checks on the input.
     */
    template<std::size_t nbits>
    class JBitSet : public std::bitset<nbits> {
    public:
        std::bitset<nbits>& set(std::size_t pos) {
            std::bitset<nbits>::set(pos);
            return *this;
        }
        template<typename VT>
        std::bitset<nbits>& set(std::size_t pos, VT val) {
            static_assert(std::is_same<VT, bool>::value || std::is_same<VT, int>::value, "only bool or int is supported");
            if (std::is_same<VT, bool>::value) {
                std::bitset<nbits>::set(pos, val);
            } else {
                std::bitset<nbits>::set(pos);
                while(++pos < val) {
                    std::bitset<nbits>::set(pos);
                }
            }
            return *this;
        }
    };

    class OperatorHead : public JBitSet<0xE01F0> {
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

    class LeftWS : public JBitSet<255> {
    public:
        LeftWS();

        bool get(std::size_t pos) const {
            return bitset::test(pos);
        }
    };

    class RightWS : public JBitSet<255> {
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
    static bool isOpNext(antlr4::TokenStream* tokens);
    static int getLastOpTokenIndex(antlr4::TokenStream* tokens);
    static bool isBinaryOp(antlr4::TokenStream* tokens);
    static bool isPrefixOp(antlr4::TokenStream* tokens);
    static bool isPostfixOp(antlr4::TokenStream* tokens);
    static bool isOperator(antlr4::TokenStream* tokens, const char* op);
    static bool isLeftOperatorWS(std::unique_ptr<antlr4::Token>& t);
    static bool isRightOperatorWS(std::unique_ptr<antlr4::Token>& t);
    static bool isSeparatedStatement(antlr4::TokenStream* tokens, int indexOfPreviousStatement);
    static void fillUp(antlr4::TokenStream* tokens);
};
