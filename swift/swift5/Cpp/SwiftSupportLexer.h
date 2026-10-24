#pragma once

#include "antlr4-runtime.h"

class SwiftSupportLexer : public antlr4::Lexer {
    class Stack : protected std::stack<unsigned long> {
    public:
        void push(unsigned long v) {
            stack::push(v);
        }

        unsigned long pop() {
            unsigned long val = stack::top();
            stack::pop();
            return val;
        }

        bool isEmpty() {
            return stack::empty();
        }

        unsigned long peek() {
            return stack::top();
        }

        void clear() {
            Stack empty;
            stack::swap(empty);
        }
    };
public:
    SwiftSupportLexer(antlr4::CharStream *input) : Lexer(input) { }

    Stack parenthesis;

    void reset() override
    {
        Lexer::reset();
        parenthesis.clear();
    }
};
