# Makefile Grammar

An ANTLR4 grammar for [Makefiles](https://en.wikipedia.org/wiki/Make_(software)), the
input format for the `make` build automation tool.  Supports simple variable
assignments (`=`, `:=`, `+=`, `?=`, `!=`), rules with multiple targets and
prerequisites, tab-indented recipe (command) lines, and `#` comments.

See the GNU Make manual for the full language reference:
[https://www.gnu.org/software/make/manual/make.html](https://www.gnu.org/software/make/manual/make.html)
