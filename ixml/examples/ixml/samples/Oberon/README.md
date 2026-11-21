# Oberon samples

Programming languages are a prominent example of structured
information for which a context-free grammar is often provided.  This
directory contains an illustration of using Invisible XML to parse
software source code.

*Oberon* is the name of both an operating system and the programming
language in which the operating system is implemented, as well as the
project in which the operating system, the programming language, and
suitable workstation hardware for running them were developed.  The
results of the original Project Oberon were published in 1992 (Niklaus
Wirth and Jürg Gutknecht, *Project Oberon: The Design of an Operating
System, a Compiler, and a Computer*, New York: ACM Press, 1992), and
several variants of the programming language and the system were
developed over the ensuing years.  The samples here are drawn from the
2013 revision of the system.

The programming language Oberon was designed as a successor to Pascal
and Modula-2; the sample source code reproduced here is the full
source of the Oberon compiler.

The directory `Project-Oberon-2013-materials/` contains some source
materials copied from Niklaus Wirth's [Project Oberon (New Edition
2013)](https://people.inf.ethz.ch/wirth/ProjectOberon/index.html)
page, together with the appropriate license.  The material includes
Wirth's grammar for Oberon and five modules for the compiler.  (The
material is also available from a [mirror
site](http://www.projectoberon.net/).

The directory `Grammars` contains Invisible-XML versions of the Oberon
grammar.  One (`Oberon.0.ixml`) is a purely mechanical translation of
Wirth's grammar into ixml notation, unusable since Wirth's grammar
presupposes a lexical scanner to identify tokens and consume
whitespace and comments.  The second (`Oberon.1.ixml`) is a mostly
mechanical augmentation of the first, allowing whitespace and comments
to appear at appropriate locations and hiding the lowest-level
nonterminals like *letter* and *digit* as well as keywords and
delimiters which are redundant in the XML form of a program.  It can
be used to parse the source files, but the output is a bit more
cluttered than is convenient or visually appealing.  The third grammar
(`Oberon.ixml`) is a more idiomatic Invisible XML grammar, which thins
the XML output tree to make it easier to read and process.  All three
grammars are supplied in order to allow the reader to compare them and
study the kinds of changes needed to make a useful ixml grammar from
an existing grammar like Wirth's.

The directory `XML` contains parse trees for the five source files, in
several formats distinguished by their names.  Some were made by
parsing against `Oberon.ixml` and some by parsing against
`Oberon.1.ixml`.  Some are indented (pretty-printed) to show the
structure and others are not.  (In these documents, the extra
whitespace can make a sizable difference in file size.)

For the ORB module, for example,

  * `ORB.xml` is the unindented XML produced by parsing `ORB.Mod.txt`
    with `Oberon.ixml`

  * `ORB.pp.xml` is an indented (pretty-printed) equivalent
  
  * `ORB.1.xml` and `ORB.1.pp.xml` are the unindented and indented XML
    produced by parsing `ORB.Mod.txt` with `Oberon.1.ixml`

The `XML` directory also contains XML versions of the grammars.

The directory `misc` contains auxiliary material that does not seem to
belong anywhere else.

