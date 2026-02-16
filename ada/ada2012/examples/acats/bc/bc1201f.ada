-- BC1201F.ADA

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
-- CHECK THAT A RANGE CONSTRAINT IS NOT ALLOWED ON THE
--   COMPONENT TYPE OF A GENERIC FORMAL ARRAY TYPE.

-- CHANGE HISTORY:
--      03 Feb 1986   PWB
--      22 Apr 2021   RLB   Added error location indicators.
--!

PROCEDURE BC1201F IS

     TYPE FLT IS DIGITS 5;
     TYPE FIX IS DELTA 0.1 RANGE -1.0 .. 1.0;

     GENERIC
          TYPE INDEX IS ( <> );
          TYPE FORMAL_ARRAY IS
               ARRAY ( INDEX )
               OF INTEGER RANGE 1..10;         -- ERROR: CONSTRAINT. {2:11;1}
          TYPE FLT_ARRAY IS
               ARRAY ( INDEX )
               OF FLT RANGE -1.0 .. 1.0;       -- ERROR: CONSTRAINT. {2:11;1}
     PROCEDURE GEN_PROC ( X : INTEGER );

     GENERIC
          TYPE FORMAL_STRING IS
               ARRAY ( POSITIVE RANGE <> )
               OF CHARACTER RANGE 'A' .. 'Z';  -- ERROR: CONSTRAINT. {2:11;1}
          TYPE FIX_ARRAY IS
               ARRAY ( POSITIVE RANGE <> )
               OF FIX RANGE 0.0 .. 0.5;        -- ERROR: CONSTRAINT. {2:11;1}
     PACKAGE GEN_PACK IS
     END GEN_PACK;

     GENERIC
          TYPE FORMAL_TABLE IS
               ARRAY ( POSITIVE RANGE <>,
                       INTEGER RANGE <>)
               OF BOOLEAN RANGE TRUE .. FALSE; -- ERROR: CONSTRAINT. {3:11;1}
     FUNCTION GEN_FUNC ( X : INTEGER ) RETURN INTEGER;

     PROCEDURE GEN_PROC ( X : INTEGER ) IS
     BEGIN
          NULL;
     END GEN_PROC;

     FUNCTION GEN_FUNC ( X : INTEGER ) RETURN INTEGER IS
     BEGIN
          IF X = INTEGER'FIRST THEN
               RETURN INTEGER'LAST;
          ELSE
               RETURN X - 1;
          END IF;
     END GEN_FUNC;

BEGIN     -- BC1201F
     NULL;
END BC1201F;
