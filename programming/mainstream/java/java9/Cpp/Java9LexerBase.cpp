/*
 * [The "BSD license"]
 *  Copyright (c) 2014 Terence Parr
 *  Copyright (c) 2014 Sam Harwell
 *  Copyright (c) 2017 Chan Chung Kwong
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. The name of the author may not be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 *  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 *  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 *  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 *  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 *  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "antlr4-runtime.h"
#include "Java9LexerBase.h"

Java9LexerBase::Java9LexerBase(antlr4::CharStream * input) : antlr4::Lexer(input)
{
    _input = input;
}

bool Java9LexerBase::Character::isJavaIdentifierPart(int c)
{
    if ((int)'a' <= c && c <= (int)'z' || (int)'A' <= c && c <= (int)'Z')
        return true;
    else if (c == (int)'$')
        return true;
    else if (c == (int)'_')
        return true;
    else if ((int)'0' <= c && c <= (int)'9')
        return true;
    return false;
}

bool Java9LexerBase::Character::isJavaIdentifierStart(int c)
{
    if ((int)'a' <= c && c <= (int)'z' || (int)'A' <= c && c <= (int)'Z')
        return true;
    else if (c == (int)'$')
        return true;
    else if (c == (int)'_')
        return true;
    return false;
}

char32_t surrogate_to_utf32(char16_t high, char16_t low) { 
	return (high << 10) + low - 0x35fdc00; 
}

int Java9LexerBase::Character::toCodePoint(int high, int low)
{
    return surrogate_to_utf32(high, low);
}

bool Java9LexerBase::Check1()
{
    return Java9LexerBase::Character::isJavaIdentifierStart(_input->LA(-1));
}

bool Java9LexerBase::Check2()
{
    return Java9LexerBase::Character::isJavaIdentifierStart(Java9LexerBase::Character::toCodePoint((char)_input->LA(-2), (char)_input->LA(-1)));
}

bool Java9LexerBase::Check3()
{
    return Java9LexerBase::Character::isJavaIdentifierPart(_input->LA(-1));
}

bool Java9LexerBase::Check4()
{
    return Java9LexerBase::Character::isJavaIdentifierPart(Java9LexerBase::Character::toCodePoint((char)_input->LA(-2), (char)_input->LA(-1)));
}
