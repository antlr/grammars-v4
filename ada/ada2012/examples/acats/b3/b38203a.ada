-- B38203A.ADA

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
-- CHECK THAT THE PREFIX OF AN ARRAY ATTRIBUTE CANNOT BE AN
-- ACCESS TYPE.

-- AH  8/27/86

PROCEDURE B38203A IS

     TYPE ARR1 IS ARRAY (INTEGER RANGE <>, INTEGER RANGE <>) OF INTEGER;
     TYPE ARR2 IS ARRAY (INTEGER RANGE <>) OF INTEGER;
     TYPE ARR3 IS ARRAY (1..3, 2..4) OF INTEGER;
     TYPE ARR4 IS ARRAY (0..8) OF INTEGER;
     
     TYPE ACC1 IS ACCESS ARR1(1..3, 2..4);
     TYPE ACC2 IS ACCESS ARR2(0..8);
     TYPE ACC3 IS ACCESS ARR3;
     TYPE ACC4 IS ACCESS ARR4;
     
     P1 : ACC1 := NEW ARR1(1..3, 2..4);
     P2 : ACC2 := NEW ARR2(0..8);
     P3 : ACC3 := NEW ARR3;
     P4 : ACC4 := NEW ARR4;
     
     A1 : INTEGER := P1'FIRST(1);           -- OK P1.
     A2 : INTEGER := P2'LAST;               -- OK P2.
     A3 : INTEGER := P3'LENGTH(2);          -- OK P3.
     A4 : BOOLEAN := 4 IN P4'RANGE;         -- OK P4.

     I1A : INTEGER := ACC1'FIRST(1);        -- ERROR: ACC1.
     I1B : INTEGER := ACC1'FIRST(2);        -- ERROR: ACC1.
     I1C : INTEGER := ACC1'LAST(1);         -- ERROR: ACC1.
     I1D : INTEGER := ACC1'LAST(2);         -- ERROR: ACC1.
     I1E : INTEGER := ACC1'LENGTH(1);       -- ERROR: ACC1.
     I1F : INTEGER := ACC1'LENGTH(2);       -- ERROR: ACC1.
     I1G : BOOLEAN := 2 IN ACC1'RANGE(1);   -- ERROR: ACC1.
     I1H : BOOLEAN := 3 IN ACC1'RANGE(2);   -- ERROR: ACC1.
     
     I2A : INTEGER := ACC2'FIRST;           -- ERROR: ACC2.
     I2B : INTEGER := ACC2'LAST;            -- ERROR: ACC2.
     I2C : INTEGER := ACC2'LENGTH;          -- ERROR: ACC2.
     I2D : BOOLEAN := 2 IN ACC2'RANGE;      -- ERROR: ACC2.
     
     I3A : INTEGER := ACC3'FIRST(1);        -- ERROR: ACC3.
     I3B : INTEGER := ACC3'FIRST(2);        -- ERROR: ACC3.
     I3C : INTEGER := ACC3'LAST(1);         -- ERROR: ACC3.
     I3D : INTEGER := ACC3'LAST(2);         -- ERROR: ACC3.
     I3E : INTEGER := ACC3'LENGTH(1);       -- ERROR: ACC3.
     I3F : INTEGER := ACC3'LENGTH(2);       -- ERROR: ACC3.
     I3G : BOOLEAN := 2 IN ACC3'RANGE(1);   -- ERROR: ACC3.
     I3H : BOOLEAN := 3 IN ACC3'RANGE(2);   -- ERROR: ACC3.
     
     I4A : INTEGER := ACC4'FIRST;           -- ERROR: ACC4.
     I4B : INTEGER := ACC4'LAST;            -- ERROR: ACC4.
     I4C : INTEGER := ACC4'LENGTH;          -- ERROR: ACC4.
     I4D : BOOLEAN := 8 IN ACC4'RANGE;      -- ERROR: ACC4.

BEGIN
     NULL;
END B38203A;
