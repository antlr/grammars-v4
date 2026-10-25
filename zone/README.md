# Zone File Grammar

An ANTLR4 grammar for [Zone Files](https://en.wikipedia.org/wiki/Zone_file)

DNS zone master files describe DNS resource records. Syntax references:
[RFC 1035, section 5](https://www.rfc-editor.org/rfc/rfc1035#section-5),
[BIND master file format](https://bind9.readthedocs.io/en/latest/chapter3.html),
and [RFC 3597 generic records](https://www.rfc-editor.org/rfc/rfc3597).

The grammar recognizes `$ORIGIN`, `$TTL`, `$INCLUDE`, explicit or omitted owners,
both TTL/class orderings, common record types, and `TYPE<number>` records.
Whitespace at the start of a record is significant: it denotes an omitted owner.
Record data supports quoted strings, escaped characters, and parenthesized
multiline data. Semicolon comments are ignored outside strings. LF and CRLF
are supported, and the final newline is optional.

Record data is parsed as fields rather than validated per record type. Consumers
must validate addresses, record-specific field counts, TTL ranges, and domain
names, and resolve omitted owners, origins, and included files. Parsing does not
read `$INCLUDE` files. BIND-specific `$GENERATE` and other extensions are not
supported.

All ten files in [`examples/`](examples/) are tested with the `zonefile` entry
point, which consumes the entire input. Run `mvn -pl zone clean test` from the
repository root, or `mvn clean test` from this directory. `desc.xml` configures
the same entry point for Trash Toolkit drivers.
