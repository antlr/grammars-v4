# BCP47, Tags for Identifying Languages

Transcribed into Invisible XML by Norm Tovey-Walsh, April 2022.

The nonterminals used in this grammar are those of 
[BCP47](tools.ietf.org/rfc/bcp/bcp47.txt), except for
the “shortcuts” listed at the end of the grammar.

The ABNF description has no analogue to Invisible XML marks for
guiding XML serialization; the marks used here have been supplied by
the transcriber. Suggestions for changes or improvements most welcome.

The grammar is ambiguous in a few places. The “regular” language tags
that are deprecated match both the `langtag` production and the
`regular` production. For example, `no-bok` can be parsed as:

```xml
<Language-Tag>
   <regular>no-bok</regular>
</Language-Tag>
```

or as

```xml
<Language-Tag>
   <language>no
      <extlang>bok</extlang>
   </language>
</Language-Tag>
```

It’s a feature of the grammar that the primary 2-3 character alpha
language has no XML wrapper, although extensions and subsequent
variants do. The author would be inclined to add a new nonterminal to
make the XML more regular, but it has been left unchanged in favor of
faithfulness to BCP47.

Note: there is a bug in the Invisible XML grammar of 2022-02-22, and
perhaps earlier, that parses comments following literals incorrectly.
You will have to delete (at least some of) the comments in this
grammar to get the correct results, or use a later version of the
Invisible XML grammar.
