# Javadoc Grammar

An ANTLR4 grammar for Javadoc.

This grammar matches the documentation comments used in Java, for example:

```Java
/**
 * This is a description text with {@see InlineTag inline tags}.
 * It can also contain <b>HTML</b>.
 *
 * @return Lines beginning with an @ sign start the tag section.
 */
```
