-- B46004C.ADA

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
-- WHEN THE TARGET TYPE OF A TYPE CONVERSION IS AN ARRAY TYPE, CHECK 
-- THAT FOR CORRESPONDING INDEX POSITIONS OF THE OPERAND AND TARGET 
-- TYPE, ONE INDEX TYPE CANNOT BE AN ENUMERATION TYPE AND THE OTHER A
-- NUMERIC TYPE; ALSO IF BOTH ARE DIFFERENT ENUMERATION TYPES, ONE
-- MUST BE DERIVED FROM THE OTHER OR THEY MUST HAVE A COMMON ANCESTOR
-- (ALSO SEE C46041A AND C46042A).

-- R.WILLIAMS 9/19/86

PROCEDURE B46004C IS

     TYPE MYBOOL IS (FALSE, TRUE);
     TYPE CHAR IS ('A', 'B', 'C', 'D', 'E');

     TYPE ARR1 IS ARRAY (INTEGER RANGE <>) OF INTEGER;
     A1 : ARR1 (1 .. 5) := (OTHERS => 0);

     TYPE ARR2 IS ARRAY (CHARACTER RANGE <>) OF INTEGER;
     A2 : ARR2 ('A' .. 'E') := (OTHERS => 0);

     TYPE ARR3 IS ARRAY (CHAR RANGE <>) OF INTEGER;
     A3 : ARR3 ('A' .. 'E') := (OTHERS => 0);

     TYPE ARR4 IS ARRAY (BOOLEAN RANGE <>) OF INTEGER;
     A4 : ARR4 (FALSE .. TRUE) := (OTHERS => 0);
     N4 : ARR4 (TRUE .. FALSE);

     TYPE ARR5 IS ARRAY (MYBOOL RANGE <>) OF INTEGER;
     A5 : ARR5 (FALSE .. TRUE) := (OTHERS => 0);
     N5 : ARR5 (TRUE .. FALSE);

     TYPE ARR6 IS ARRAY (CHARACTER RANGE <>, CHARACTER RANGE <>) OF 
          INTEGER;
     A6 : ARR6 ('A' .. 'E', 'A' .. 'E') := (OTHERS => (OTHERS => 0));

     TYPE ARR7 IS ARRAY (CHARACTER RANGE <>, CHAR RANGE <>) OF INTEGER;
     A7 : ARR7 ('A' .. 'E', 'A' .. 'E') := (OTHERS => (OTHERS => 0));
          
     TYPE ARR8 IS ARRAY (CHAR RANGE <>, CHARACTER RANGE <>) OF INTEGER;
     A8 : ARR8 ('A' .. 'E', 'A' .. 'E') := (OTHERS => (OTHERS => 0));
          
     
BEGIN
     A1 := ARR1 (A2);              -- ERROR: INCOMPATIBILE INDEX TYPES.
     
     A2 := ARR2 (A3);              -- ERROR: INCOMPATIBILE INDEX TYPES.

     A4 := ARR4 (A5);              -- ERROR: INCOMPATIBILE INDEX TYPES.

     A5 := ARR5 (A4);              -- ERROR: INCOMPATIBILE INDEX TYPES.

     N4 := ARR4 (N5);              -- ERROR: INCOMPATIBILE INDEX TYPES.

     A6 := ARR6 (A7);              -- ERROR: INCOMPATIBILE INDEX TYPES.

     A6 := ARR6 (A8);              -- ERROR: INCOMPATIBILE INDEX TYPES.
END B46004C;
