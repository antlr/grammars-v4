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
import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:convert';

abstract class JavaLexerBase extends Lexer
{
    JavaLexerBase(CharStream input) : super(input)
    {
    }

    bool Check1()
    {
        return Character.isJavaIdentifierStart(inputStream.LA(-1)!);
    }

    bool Check2()
    {
        return Character.isJavaIdentifierStart(Character.toCodePoint(inputStream.LA(-2)!, inputStream.LA(-1)!));
    }

    bool Check3()
    {
        return Character.isJavaIdentifierPart(inputStream.LA(-1)!);
    }

    bool Check4()
    {
        return Character.isJavaIdentifierPart(Character.toCodePoint(inputStream.LA(-2)!, inputStream.LA(-1)!));
    }
}

class Character
{
    static bool isJavaIdentifierPart(int c)
    {
        if (('a'.codeUnitAt(0) <= c && c <= 'z'.codeUnitAt(0)) || ('A'.codeUnitAt(0) <= c && c <= 'Z'.codeUnitAt(0))) //Char.IsLetter((char)c))
			return true;
        else if (c == '\$'.codeUnitAt(0))
			return true;
        else if (c == '_'.codeUnitAt(0))
			return true;
        else if ('0'.codeUnitAt(0) <= c && c <= '9'.codeUnitAt(0)) //Char.IsDigit(c))
			return true;
        else if ('0'.codeUnitAt(0) <= c && c <= '9'.codeUnitAt(0)) //Char.IsNumber(c))
			return true;
        return false;
    }

    static bool isJavaIdentifierStart(int c)
    {
        if (('a'.codeUnitAt(0) <= c && c <= 'z'.codeUnitAt(0)) || ('A'.codeUnitAt(0) <= c && c <= 'Z'.codeUnitAt(0))) //Char.IsLetter((char)c))
			return true;
        else if (c == '\$'.codeUnitAt(0))
			return true;
        else if (c == '_'.codeUnitAt(0))
			return true;
        return false;
    }

    static int toCodePoint(int high, int low)
    {
		List<int> encoded = List.filled(2, 0, growable: false);
		encoded.add(high);
		encoded.add(low);
		return base64.encode(encoded).codeUnitAt(0);

//		return Utf16CodeUnitDecoder(encoded).codeUnitAt(0);
//		return utf16.decode(encoded);
//        return Char.ConvertToUtf32(high, low);
    }
}

