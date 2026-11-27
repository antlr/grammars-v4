-- Double-quoted text in expressions, and parameter markers. Every statement here is valid
-- with and without ANSI_QUOTES: "a" is an identifier with it and a string without it.
SELECT "a", CONCAT("a", b), JSON_OBJECT("k", v) FROM t WHERE y = "a";
SELECT ? + 1, COALESCE(?, "a") FROM t WHERE c IN (?, "b");
