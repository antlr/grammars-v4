-- BC1201G.ADA

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
-- CHECK THAT AN ACCURACY CONSTRAINT IS NOT ALLOWED ON A
-- COMPONENT TYPE SPECIFICATION IN A GENERIC FORMAL
-- ARRAY TYPE DECLARATION.

-- CHANGE HISTORY:
--      03 Feb 1986   PWB
--      22 Apr 2021   RLB   Added error location indicators.
--!

PROCEDURE BC1201G IS

     TYPE FLOAT_5  IS DIGITS 5;
     TYPE FIXED_01 IS DELTA 0.01 RANGE -1.0 .. 1.0;

     GENERIC
          TYPE FLOAT_4_ARRAY IS
               ARRAY ( INTEGER RANGE <> )
               OF FLOAT_5 DIGITS 4;              -- ERROR: CONSTRAINT. {2:11;1}
     PROCEDURE GEN_PROC ( X : INTEGER );

     GENERIC
          TYPE FIXED_1_ARRAY IS
               ARRAY ( BOOLEAN )
               OF FIXED_01 DELTA 0.1;            -- ERROR: CONSTRAINT. {2:11;1}
     FUNCTION GEN_FUNC ( X : INTEGER ) RETURN INTEGER;

     PROCEDURE GEN_PROC ( X : INTEGER ) IS
     BEGIN
          NULL;
     END GEN_PROC;

     FUNCTION GEN_FUNC ( X : INTEGER ) RETURN INTEGER IS
     BEGIN
          IF X = INTEGER'LAST THEN
               RETURN INTEGER'FIRST;
          ELSE
               RETURN X + 1;
          END IF;
     END GEN_FUNC;

BEGIN     -- BC1201G
     NULL;
END BC1201G;
