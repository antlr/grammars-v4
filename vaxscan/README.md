This is the grammar for VAX SCAN. The following description is lifted from the
Guide to VAX SCAN (Copyright ©1985, 1986, 1989 by Digital Equipment 
Corporation).

"VAX SCAN is a block-structured programming language in the
VAX/VMS environment that is designed to build tools to manipulate
text strings and text files. The primary applications for VAX SCAN are
filters, translators, extractors/analyzers, and preprocessors."

Digital Equipment (DEC) having decided not to continue the product or port
it to their new hardware platforms (ALPHA and Itanium) made the source
code FREEWARE.

This ANTLR4 grammar is based on the BNF provided in the source code.

The following copyright statement is used throughout the source code. As they
copied the source directory tree and provided it AS-IS I would assume this no
longer applies.

******************************************************** 
                COPYRIGHT  1989, 1996
            DIGITAL EQUIPMENT CORPORATION, MAYNARD, MASS.

THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY BE USED AND COPIED ONLY IN ACCORDANCE WITH THE TERMS OF SUCH  LICENSE
AND  WITH THE INCLUSION OF THE ABOVE COPYRIGHT NOTICE.  THIS SOFTWARE OR ANY OTHER COPIES THEREOF MAY NOT BE PROVIDED OR
OTHERWISE MADE AVAILABLE TO ANY OTHER PERSON.  NO TITLE TO AND OWNERSHIP OF THE SOFTWARE IS HEREBY TRANSFERRED.

THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT NOTICE AND SHOULD NOT BE  CONSTRUED  AS  A  COMMITMENT  BY
DIGITAL EQUIPMENT CORPORATION.

DIGITAL  ASSUMES  NO  RESPONSIBILITY  FOR  THE  USE OR RELIABILITY OF ITS SOFTWARE ON EQUIPMENT WHICH IS NOT SUPPLIED BY
DIGITAL.

*********************************************************

The source code along with VAX/VMS binaries are available on the VMS Software
Inc (VSI) website under Freeware.

https://vmssoftware.com/community/freeware/

The source code is a mix of Bliss, PLI, and Macro Assembler. The compiler uses
a code generator (VAX Code Generator) that will create VAX/VMS object files.
Bliss is a family of compilers across several DEC platforms. It was designed as
a systems implementation language.

Under the examples directory are a number of small sample programs that were
provided with the software kit. They all parse cleanly.

## Reference

