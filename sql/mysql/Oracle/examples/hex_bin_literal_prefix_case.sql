-- The X and B of quoted hexadecimal and bit-value literals may be written in either case;
-- only the 0x and 0b prefixes are lower case only.
-- The quoted hexadecimal form takes whole bytes, two digits each: X'0F' and X'' are literals; MySQL rejects X'F'.
CREATE TABLE t_lit (h VARBINARY(4) DEFAULT X'01AF', b BIT(2) DEFAULT B'01');
INSERT INTO t_lit VALUES (X'4D79', B'10'), (x'4d79', b'10'), (X'', 0b11);
SELECT h FROM t_lit WHERE h = X'01AF' OR b = B'01';
SELECT X'0F' = x'0f' AS v, X'' = x'' AS e, HEX(X'aBcD') AS h;
SELECT * FROM (VALUES ROW(X'0F', x'', X'01AF')) AS d;
