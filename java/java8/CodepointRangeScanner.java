/* [The "BSD license"]
 *  Copyright (c) 2019 Student Main
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
/*
 * Scan full unicode range, find all valid identifier character, output in ANTLRv4 lexer fragment rule.
 */
import java.lang.*;
import java.util.*;

class CodepointRangeScanner {
    static int UNICODE_RANGE = 0x10000;

    public static void main(String[] args) {
        boolean startCh[] = new boolean[UNICODE_RANGE];
        boolean partCh[] = new boolean[UNICODE_RANGE];

        for (int i = 0; i < UNICODE_RANGE; i++) {
            startCh[i] = Character.isJavaIdentifierStart(i);
            partCh[i] = (Character.isJavaIdentifierPart(i) && !startCh[i] && i > 0x20);
        }

        System.out.println("fragment IdentifierStart\n\t: " + printRanges(startCh));
        System.out.println("fragment IdentifierPart\n\t: IdentifierStart\n\t| " + printRanges(partCh));
    }

    static String printRanges(boolean[] a) {
        ArrayList<String> s = new ArrayList<String>();
        boolean last = false;
        int start = -1;
        for (int p = 0; p <= a.length; p++) {
            if (p == a.length && last == false) {
                // Do nothing
            } else if ((p == a.length || a[p] == false) && last == true) {
                s.add(printRange(start, p - 1));
                last = false;
            } else if (a[p] == true && last == false) {
                start = p;
                last = true;
            }
        }
        return String.join("\n\t| ", s) + "\n\t;\n";
    }

    static String printRange(int start, int end) {
        if (start >= end) {
            return String.format("[%s]", escapeCodepoint(start));
        } else {
            return String.format("[%s-%s]", escapeCodepoint(start), escapeCodepoint(end));
        }
    }

    static String escapeCodepoint(int cp) {
        if (cp < 0x10000) return String.format("\\u%04X", cp);
        else {
            int s1 = 0xD800 | (cp >> 10);
            int s2 = 0xDC00 | (cp & 0x3FF);
            return String.format("\\u%04X\\u%04X", s1, s2);
        }
    }
}