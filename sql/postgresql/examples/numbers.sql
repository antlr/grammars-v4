--
-- Tests for numeric literal
--
SELECT 0b10, 0B10, 0x10, 0X10, 0o10, 0O10, 010, 0 x10, 0 b10, 0 o10;
SELECT abs(0b10), abs(0B10), abs(0x10), abs(0X10), abs(0o10), abs(0O10),
       ('{1}'::int[])[0b01], 1::char(0o01), (array[1])[0x01];
