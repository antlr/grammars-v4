# /****************************************************************
# Copyright (C) Lucent Technologies 1997
# All Rights Reserved
#
# Permission to use, copy, modify, and distribute this software and
# its documentation for any purpose and without fee is hereby
# granted, provided that the above copyright notice appear in all
# copies and that both that the copyright notice and this
# permission notice and warranty disclaimer appear in supporting
# documentation, and that the name Lucent Technologies or any of
# its entities not be used in advertising or publicity pertaining
# to distribution of the software without specific, written prior
# permission.
#
# LUCENT DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
# INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
# IN NO EVENT SHALL LUCENT OR ANY OF ITS ENTITIES BE LIABLE FOR ANY
# SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
# IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
# ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
# THIS SOFTWARE.
# ****************************************************************/

CFLAGS = -fsanitize=address -O1 -g -fno-omit-frame-pointer -fno-optimize-sibling-calls
CFLAGS = -g
CFLAGS =
CFLAGS = -O2

# compiler options
#CC = cc -Wall -g -Wwrite-strings
#CC = cc -O4 -Wall -pedantic -fno-strict-aliasing
#CC = cc -fprofile-arcs -ftest-coverage # then gcov f1.c; cat f1.c.gcov
HOSTCC = cc -g -Wall -pedantic -Wcast-qual
# HOSTCC = g++ -g -Wall -pedantic -Wcast-qual
CC = $(HOSTCC)  # change this is cross-compiling.

# By fiat, to make our lives easier, yacc is now defined to be bison.
# If you want something else, you're on your own.
# YACC = yacc -d -b awkgram
YACC = bison -d

OFILES = b.o main.o parse.o proctab.o tran.o lib.o run.o lex.o

SOURCE = awk.h awkgram.tab.c awkgram.tab.h proto.h awkgram.y lex.c b.c main.c \
	maketab.c parse.c lib.c run.c tran.c proctab.c

LISTING = awk.h proto.h awkgram.y lex.c b.c main.c maketab.c parse.c \
	lib.c run.c tran.c

SHIP = README LICENSE FIXES $(SOURCE) awkgram.tab.[ch].bak makefile  \
	 awk.1

a.out:	awkgram.tab.o $(OFILES)
	$(CC) $(CFLAGS) awkgram.tab.o $(OFILES) $(ALLOC)  -lm

$(OFILES):	awk.h awkgram.tab.h proto.h

awkgram.tab.c awkgram.tab.h:	awk.h proto.h awkgram.y
	$(YACC) $(YFLAGS) awkgram.y

proctab.c:	maketab
	./maketab awkgram.tab.h >proctab.c

maketab:	awkgram.tab.h maketab.c
	$(HOSTCC) $(CFLAGS) maketab.c -o maketab

bundle:
	@cp awkgram.tab.h awkgram.tab.h.bak
	@cp awkgram.tab.c awkgram.tab.c.bak
	@bundle $(SHIP)

tar:
	@cp awkgram.tab.h awkgram.tab.h.bak
	@cp awkgram.tab.c awkgram.tab.c.bak
	@bundle $(SHIP) >awk.shar
	@tar cf awk.tar $(SHIP)
	gzip awk.tar
	ls -l awk.tar.gz
	@zip awk.zip $(SHIP)
	ls -l awk.zip

gitadd:
	git add README LICENSE FIXES \
           awk.h proto.h awkgram.y lex.c b.c main.c maketab.c parse.c \
	   lib.c run.c tran.c \
	   makefile awk.1 testdir

gitpush:
	# only do this once:
	# git remote add origin https://github.com/onetrueawk/awk.git
	git push -u origin master

names:
	@echo $(LISTING)

test check:
	./REGRESS

clean: testclean
	rm -f a.out *.o *.obj maketab maketab.exe *.bb *.bbg *.da *.gcov *.gcno *.gcda # proctab.c

cleaner: testclean
	rm -f a.out *.o *.obj maketab maketab.exe *.bb *.bbg *.da *.gcov *.gcno *.gcda proctab.c awkgram.tab.*

# This is a bit of a band-aid until we can invest some more time
# in the test suite.
testclean:
	cd testdir; rm -fr arnold-fixes beebe devnull echo foo* \
		glop glop1 glop2 lilly.diff tempbig tempsmall time

# For the habits of GNU maintainers:
distclean: cleaner
