# Gopher Grammar

An ANTLR4 grammar for the directory-listing ("menu") format of the
[Gopher protocol](https://en.wikipedia.org/wiki/Gopher_(protocol)).

A Gopher menu is a sequence of tab-separated directory entries
(`type+username`, `selector`, `host`, `port`), each terminated by a CRLF,
with the whole listing terminated by a line containing only a period.

## References

* [RFC 1436](https://datatracker.ietf.org/doc/html/rfc1436) — The Internet
  Gopher Protocol, which defines the directory listing format this grammar
  parses.
