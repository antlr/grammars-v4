# Uuencoding

An ANTLR 4 grammar for traditional
[uuencoding](https://en.wikipedia.org/wiki/Uuencoding), an encoding that represents
binary data as printable text. See also the
[POSIX uuencode specification](https://pubs.opengroup.org/onlinepubs/9799919799/utilities/uuencode.html).

The `uuencoding` entry point accepts a complete `begin MODE FILENAME` wrapper
with multiple data lines, a zero-length line, and an `end` trailer, or bare data
lines without a wrapper. Three- and four-digit octal modes, filenames containing
spaces, LF and CRLF line endings, and space or backtick zero characters are
supported. The last line need not have a newline. Empty files require a wrapper.

This grammar checks the text structure and character ranges, not whether each
line's encoded length matches its length character. It does not decode data or
support the separate `begin-base64` format.

All ten inputs in [`examples/`](examples/) are tested with
`mvn -pl uuencoding clean test` from the repository root, or `mvn clean test`
from this directory. The original `example3.txt` is retained as a historical
syntax sample; its data-line lengths are not canonical uuencode output.
