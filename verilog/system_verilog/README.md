The SysVerilogHDL grammar is used to generate a parser and lexer using ANTLR.
It has only been tested using the C# target and has never been used to generate
Java source files.  This grammar does not completely cover all features of the
System Verilog and is offered here as is with no warranty.  It was developed largely
organically starting from a basic implementation and adding/modifying rules as I
added more files to a growing corpus and finding examples that would not parse.
By the time work ceased on the development on this grammar it was parsing a corpus
or over 10,000 verilog and system verilog files.  There is currently no on going
development for this grammar.  There are some rules of the grammar are commented
out as they were written by I did not encounter an example file that exercised them.

This grammar was used two research projects a Microsoft Research between 2013 and 2016
including a project to create code scanners for hardware debugging to integrating tools
for HDL development in Visual Studio.

Links:
https://www.microsoft.com/en-us/research/project/gnosis-advancing-hardware-development/
https://www.microsoft.com/en-us/research/publication/extending-gnosis-for-system-verilog-hdl-static-analysis/

