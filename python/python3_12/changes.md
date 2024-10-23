Szept 05, 2024
--------------
Type comment tokens are no longer generated.
Type comments will now be tokenized as plain comment tokens.

Line continuation for string literals (backslash followed by a newline) is no longer resolved.
(backslash+newline is no longer removed from string literals)

---
Oct. 18, 2024
---
Fix that `case [a, *_] if a == 0:` throws error `rule soft_kw__not__wildcard failed predicate: {this.isnotEqualToCurrentTokenText("_")}?`