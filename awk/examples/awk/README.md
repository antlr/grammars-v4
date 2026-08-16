# The One True Awk

This is the version of `awk` described in _The AWK Programming Language_,
Second Edition, by Al Aho, Brian Kernighan, and Peter Weinberger
(Addison-Wesley, 2024, ISBN-13 978-0138269722, ISBN-10 0138269726).

## What's New? ##

This version of Awk handles UTF-8 and comma-separated values (CSV) input.

### Strings ###

Functions that process strings now count Unicode code points, not bytes;
this affects `length`, `substr`, `index`, `match`, `split`,
`sub`, `gsub`, and others.  Note that code
points are not necessarily characters.

UTF-8 sequences may appear in literal strings and regular expressions.
Arbitrary characters may be included with `\u` followed by 1 to 8 hexadecimal digits.

### Regular expressions ###

Regular expressions may include UTF-8 code points, including `\u`.

### CSV ###

The option `--csv` turns on CSV processing of input:
fields are separated by commas, fields may be quoted with
double-quote (`"`) characters, quoted fields may contain embedded newlines.
Double-quotes in fields have to be doubled and enclosed in quoted fields.
In CSV mode, `FS` is ignored.

If no explicit separator argument is provided,
field-splitting in `split` is determined by CSV mode.

## Copyright

Copyright (C) Lucent Technologies 1997<br/>
All Rights Reserved

Permission to use, copy, modify, and distribute this software and
its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appear in all
copies and that both that the copyright notice and this
permission notice and warranty disclaimer appear in supporting
documentation, and that the name Lucent Technologies or any of
its entities not be used in advertising or publicity pertaining
to distribution of the software without specific, written prior
permission.

LUCENT DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
IN NO EVENT SHALL LUCENT OR ANY OF ITS ENTITIES BE LIABLE FOR ANY
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.

## Distribution and Reporting Problems

Changes, mostly bug fixes and occasional enhancements, are listed
in `FIXES`.  If you distribute this code further, please please please
distribute `FIXES` with it.

If you find errors, please report them
to the current maintainer, ozan.yigit@gmail.com.
Please _also_ open an issue in the GitHub issue tracker, to make
it easy to track issues.
Thanks.

## Submitting Pull Requests

Pull requests are welcome. Some guidelines:

* Please do not use functions or facilities that are not standard (e.g.,
`strlcpy()`, `fpurge()`).

* Please run the test suite and make sure that your changes pass before
posting the pull request. To do so:

  1. Save the previous version of `awk` somewhere in your path. Call it `nawk` (for example).
  1. Run `oldawk=nawk make check > check.out 2>&1`.
  1. Search for `BAD` or `error` in the result. In general, look over it manually to make sure there are no errors.

* Please create the pull request with a request
to merge into the `staging` branch instead of into the `master` branch.
This allows us to do testing, and to make any additional edits or changes
after the merge but before merging to `master`.

## Building

The program itself is created by

	make

which should produce a sequence of messages roughly like this:

	bison -d  awkgram.y
	awkgram.y: warning: 44 shift/reduce conflicts [-Wconflicts-sr]
	awkgram.y: warning: 85 reduce/reduce conflicts [-Wconflicts-rr]
	awkgram.y: note: rerun with option '-Wcounterexamples' to generate conflict counterexamples
	gcc -g -Wall -pedantic -Wcast-qual   -O2   -c -o awkgram.tab.o awkgram.tab.c
	gcc -g -Wall -pedantic -Wcast-qual   -O2   -c -o b.o b.c
	gcc -g -Wall -pedantic -Wcast-qual   -O2   -c -o main.o main.c
	gcc -g -Wall -pedantic -Wcast-qual   -O2   -c -o parse.o parse.c
	gcc -g -Wall -pedantic -Wcast-qual -O2 maketab.c -o maketab
	./maketab awkgram.tab.h >proctab.c
	gcc -g -Wall -pedantic -Wcast-qual   -O2   -c -o proctab.o proctab.c
	gcc -g -Wall -pedantic -Wcast-qual   -O2   -c -o tran.o tran.c
	gcc -g -Wall -pedantic -Wcast-qual   -O2   -c -o lib.o lib.c
	gcc -g -Wall -pedantic -Wcast-qual   -O2   -c -o run.o run.c
	gcc -g -Wall -pedantic -Wcast-qual   -O2   -c -o lex.o lex.c
	gcc -g -Wall -pedantic -Wcast-qual   -O2 awkgram.tab.o b.o main.o parse.o proctab.o tran.o lib.o run.o lex.o   -lm

This produces an executable `a.out`; you will eventually want to
move this to some place like `/usr/bin/awk`.

If your system does not have `yacc` or `bison` (the GNU
equivalent), you need to install one of them first.
The default in the `makefile` is `bison`; you will have
to edit the `makefile` to use `yacc`.

NOTE: This version uses ISO/IEC C99, as you should also.  We have
compiled this without any changes using `gcc -Wall` and/or local C
compilers on a variety of systems, but new systems or compilers
may raise some new complaint; reports of difficulties are
welcome.

This compiles without change on Macintosh OS X using `gcc` and
the standard developer tools.

You can also use `make CC=g++` to build with the GNU C++ compiler,
should you choose to do so.

## A Note About Releases

We don't usually do releases.

## A Note About Maintenance

NOTICE! Maintenance of this program is on a ''best effort''
basis.  We try to get to issues and pull requests as quickly
as we can.  Unfortunately, however, keeping this program going
is not at the top of our priority list.

#### Last Updated

Mon 05 Feb 2024 08:46:55 IST
