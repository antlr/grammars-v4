/*
 [The "BSD license"]
 Copyright (c) 2020 Tom Everett
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;

/**
 * https://raw.githubusercontent.com/antlr/antlr4/master/doc/resources/CaseChangingCharStream.java
 */
/**
 * This class supports case-insensitive lexing by wrapping an existing {@link CharStream} and
 * forcing the lexer to see either upper or lowercase characters. Grammar literals should then be
 * either upper or lower case such as 'BEGIN' or 'begin'. The text of the character stream is
 * unaffected. Example: input 'BeGiN' would match lexer rule 'BEGIN' if constructor parameter
 * upper=true but getText() would return 'BeGiN'.
 */
public class BinaryCharStream implements CharStream {
    final CharStream stream;

    public BinaryCharStream(CharStream stream) {
	this.stream = stream;
    }

    @Override
    public void consume() {
	stream.consume();
    }

    @Override
    public String getSourceName() {
	return stream.getSourceName();
    }

    @Override
    public String getText(Interval interval) {
	final StringBuilder buf = new StringBuilder();
	final int start = interval.a;
	final int stop = interval.b;
	final int index = stream.index();
	stream.seek(0);
	for (int i = start; i \< (stop + 1); i++) {
	    final int t = stream.LA(i + 1);
	    buf.append(t);
	}
	stream.seek(index);
	return buf.toString();
    }

    @Override
    public int index() {
	return stream.index();
    }

    @Override
    public int LA(int i) {
	return stream.LA(i);
    }

    @Override
    public int mark() {
	return stream.mark();
    }

    @Override
    public void release(int marker) {
	stream.release(marker);
    }

    @Override
    public void seek(int index) {
	stream.seek(index);
    }

    @Override
    public int size() {
	return stream.size();
    }
}
