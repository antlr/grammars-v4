# HyperTalk Grammar

An Antlr4 grammar for parsing HyperTalk, the scripting language of Apple's long-obsolete (but still insanely great) HyperCard. HyperTalk was created in 1987 by Dan Winkler and Bill Atkinson.

This grammar is part of the [WyldCard](https://github.com/defano/wyldcard) project, an open-sourced clone of HyperCard. A detailed description of this flavor of the language is [available here](https://github.com/defano/wyldcard#the-hypertalk-language).

HyperTalk's elegant syntax mimics natural English (`sort the lines of field "Students" by the last word of each`), relying on a context-sensitive evaluation of terms (called _factors_) to eliminate referential and type formalities present in most programming languages. HyperTalk "does what I mean, not what I said."

## Special Considerations

* This grammar is based on HyperCard v2.4.1 (the last commercial release of the product from 1998). That said, not all commands, functions or operators are supported (especially those addressing AppleTalk or multiple stacks). A few grammatical extensions to the syntax are also present.
* HyperTalk is a case-insensitive language, but this grammar assumes keywords in the input stream are all lowercase. Use a lowercasing `ANTLRInputStream` [such as this one](https://gist.github.com/defano/0148bcf5b7619a79a38aa766e12de10a) to provide case insensitivity.   
* This grammar is somewhat more strict than HyperCard's. HyperCard allowed uncommented, arbitrary text not conforming to the language to appear outside of function/handler definitions; this grammar does not. This grammar also disallows function and handler definitions in scriptlets.

## Start Symbols

Symbol      | Description
------------| -----------------------
`script`    | Accepts well-formed part scripts consisting of whitespace, comments, and zero or more function and handler definitions. Disallows expressions or statements (or arbitrary text) appearing outside of a handler/function. Typically useful for parsing scripts assigned to parts (like buttons, fields, or cards).
`scriptlet` | Accepts a newline-delimited list of valid HyperTalk statements, expressions, whitespace, and comments. Disallows arbitrary text and function or handler definitions. Typically useful for executing or evaluating text entered into the message box or via the `do` / `value of` constructs.

## Example Script

```
--
-- Allow the user to drag this button around the screen provided they
-- stay within the bounds of the button named "Boundary".
--
on mouseDown
  put the name of me into oldName
  repeat while the mouse is down
    if the mouseLoc is within the rect of card button "boundary" then
      set the location of me to the mouseLoc
      set the name of me to "(" & the mouseLoc & ")"
    end if
  end repeat
  set the name of me to oldName
end mouseDown
```

## Contributors

* Matt DeFano
