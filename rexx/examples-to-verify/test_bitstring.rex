/* */
binary_good1  = '1'b
binary_good2  = "0"b
binary_good3  = '0'B
binary_good4  = "1"B
binary_good5  = '0101'b
binary_good6  = "1010"b
binary_good7  = '0101'B
binary_good8  = "1010"B
binary_good9  = '0101 0101'b
binary_good10 = "1010 1010"B
binary_good11 = '1'b
binary_good12 = '11'b
binary_good13 = '111'b
binary_good14 = '1111'b
binary_good15 = '1 1111'b
binary_good16 = '11 1111'b
binary_good17 = '111 1111'b
binary_good18 = '1111 1111'b
binary_good19 = '1 1111 1111'b
binary_good20 = '11 1111 1111'b

/* A bunch of cases that are not binary strings, because there are blanks in wrong places. */
binary_bad1 = '0 1 0 1'b
binary_bad2 = "1 0 1 0"B
binary_bad3 = "10 10"b
binary_bad4 = '10 10'B
binary_bad5 = "0 101"b
binary_bad6 = '0 101'B
binary_bad7 = '1 1'b
binary_bad8 = '1 11'b
binary_bad9 = '11 11'b
binary_bad10 = '1111 1'b
binary_bad11 = '111 111'b
binary_bad12 = '1111 111'b
binary_bad13 = '11 1111 11'b
binary_bad14 = '11 1111 111'b
binary_bad15 = '1 1111 1111 1'b

hex_good1 = '1'x
hex_good2 = "1"x
hex_good3 = '1'X
hex_good4 = "1"X
hex_good5 = 'DEAF'x
hex_good6 = "deaf"x
hex_good7 = 'deaf'X
hex_good8 = "DEAF"X
hex_good9 = 'de af'x

/* A bunch of cases that are not hex strings, because there are blanks in wrong places. */
hex_bad1  = ' de af'x
hex_bad2  = "de af "X
hex_bad3  = "d e af"x
hex_bad4  = 'de a f'X
hex_bad5  = "hi mom!"x	/* Not a hex string - invalid characters */

str_good1 = 'hi mom!'
str_good2 = "bye mom!"
str_good3 = 'hi mom!'q
str_good4 = "bye mom!"Z
str_good5 = "deaf"
str_good6 = '0101'
str_good7 = 'hi mom!' x
str_good8 = "bye mom!" B
str_good9 = '1010'xyzzy
str_good10 = ab
str_good11 = a b
