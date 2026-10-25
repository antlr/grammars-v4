-- BC1201I.ADA

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
-- CHECK THAT A DISCRIMINANT CONSTRAINT IS NOT ALLOWED ON A COMPONENT
-- TYPE SPECIFICATION IN A GENERIC FORMAL ARRAY TYPE DECLARATION.

-- CHANGE HISTORY:
--      10 Feb 1986   PWB
--      22 Apr 2021   RLB   Added error location indicators.
--!

PROCEDURE BC1201I IS

     TYPE REC_1 (DISC : INTEGER) IS
          RECORD
               NULL;
          END RECORD;

     TYPE REC_2 (DISC : INTEGER := 1) IS
          RECORD
               NULL;
          END RECORD;

     GENERIC
          TYPE ARRAY_1 IS 
               ARRAY (INTEGER RANGE <>) OF 
                    REC_1 (5);                  -- ERROR: CONSTRAINT. {2:11;1}
          TYPE ARRAY_2 IS
               ARRAY (BOOLEAN RANGE <>) OF 
                   REC_2 (1);                   -- ERROR: CONSTRAINT. {2:11;1}
     PROCEDURE GEN_PROC (X : INTEGER);

     GENERIC
          TYPE ARRAY_1 IS
               ARRAY (BOOLEAN RANGE <>) OF 
                    REC_1 (DISC => 5);          -- ERROR: CONSTRAINT. {2:11;1}
          TYPE ARRAY_2 IS
               ARRAY (INTEGER RANGE <>) OF 
                    REC_2 (DISC => 1);          -- ERROR: CONSTRAINT. {2:11;1}
     FUNCTION GEN_FUNC (X : INTEGER) 
                       RETURN BOOLEAN;

     GENERIC
          TYPE ARRAY_1 IS
               ARRAY (CHARACTER RANGE <>) OF
                    REC_1(4);                   -- ERROR: CONSTRAINT. {2:11;1}
          TYPE ARRAY_2 IS
               ARRAY (BOOLEAN RANGE <>) OF 
                    REC_2 (DISC => 3);          -- ERROR: CONSTRAINT. {2:11;1}
     PACKAGE GEN_PACK IS
     END GEN_PACK;

     PROCEDURE GEN_PROC (X : INTEGER) IS
     BEGIN
          NULL;
     END GEN_PROC;

     FUNCTION GEN_FUNC (X : INTEGER) RETURN BOOLEAN IS
     BEGIN
          RETURN (X=5);
     END GEN_FUNC;

BEGIN    -- BC1201I
     NULL;
END BC1201I;
