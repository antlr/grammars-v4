
Structure Definition Language (SDL) Grammar.

SDL is a language used to provide a common definition of data structures
that could be exported and shared by various programming languages. Many
applications as well as the VMS Operating System and associated tools and
utilities were written in a variety of programming languages. SDL itself
contains modules written in Bliss, PLI, and Macro Assembler. It also uses
tool written in Pascal.

SDL was written at Digital Equipment. HP inherited the product after it
took over Compaq, which had taken over Digital Equipment. SDL is still in
use today at VMS, Inc which now owns the VMS operating system on all
platforms except the VAX. It has ported the VMS operating system to the x86
platform.

The following description of SDL is taken from:
	Guide to the HP Structure Definition Language
*
*******************************************************************************
*
The HP Structure Definition Language (HP SDL) is used to write source
statements that describe data structures and that can be translated to
source statements in other languages. You can include the resulting output
files in a corresponding target language program for subsequent compilation.

Because HP SDL is compiler- and language-independent, it is particularly
useful for maintaining multilanguage implementations. For example, you can
create and later modify a single HP SDL source file that can be translated
to multilanguage output files; any number of these output files can then be
included in one or several multilanguage programming applications.

 HP SDL supports the following OpenVMS languages:
 • HP Ada
 • HP BASIC
 • HP BLISS
 • HP C/C++
 • HP DATATRIEVE
 • HP OpenVMSDCL
 • HP FORTRAN
 • HP MACRO
 • HP Pascal
 • Kednos PL/I
 • SDML
 • DECTPU
 • UIL

*******************************************************************************

During efforts to port SDL to new platforms it was rewritten in C++ due to 
the lack of a PLI compiler. The original version was released by HP as freeware.
The software as well as the source code is available from VMS, Inc at:

	https://vmssoftware.com/community/freeware/

## Reference
* [pldb](http://pldb.info/concepts/sdl)

