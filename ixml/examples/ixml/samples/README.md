# Samples directory

This directory contains a collection of ixml grammars for various
languages or notations for which some more or less authoritative
grammar or syntax description is available.

These grammars are offered as illustrations of ixml which may be
useful in real applications, and due care has been exercised in
preparing them, but no guarantees of accuracy or fitness for purpose
are offered.

Note that it is not always possible for an ixml grammar to match a
published language perfectly: many published language descriptions
assume a lexical scanner (which may for example distinguish
identifiers from reserved words), and some may require ad-hoc checks
that are not feasible in a pure context-free grammar.  So do not
assume without checking that the language accepted by the ixml grammar
here matches exactly the language defined by the relevant source.

The samples are free to use, they are [licensed](LICENSE) under
the [Creative Commons CC0](https://creativecommons.org/public-domain/cc0/)
license.

Samples in this directory include:

* ISBN: a grammar for International Standard Book Numbers and
  International Standard Serial numbers that checks to make sure the
  check digit is correct.  (Currently only ISBN-13; ISBN-10 and ISSN
  coming soon.)

* ABNF: an ixml version of the grammar notation used in IETF
  Requests for Comment, as defined in RFC 5234.

* bcp47: The grammar from [BCP47](tools.ietf.org/rfc/bcp/bcp47.txt),
  _Tags for Identifying Languages_.

* XPath: two ixml versions of the grammar for XPath3.1 expressions:
    **XPath.ixml** produces a full parse tree, **XPath.reducedTree.ixml**  (derived from a decorated version of the full grammar) produces
    a minimised parse tree where any element in the resulting parse tree which would have only a single child is removed 
      - this for example ensures that a simple integer is returned as a single element, rather a tree some 30+ levels deep.


