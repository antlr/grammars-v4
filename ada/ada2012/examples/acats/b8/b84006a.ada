-- B84006A.ADA

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
-- CHECK THAT IF TWO RENAMING DECLARATIONS (IN DIFFERENT PACKAGES)
-- DECLARE THE SAME IDENTIFIER AND BOTH DECLARATIONS RENAME THE
-- SAME ENTITY, A USE CLAUSE CANNOT MAKE THE IDENTIFIER VISIBLE,
-- UNLESS IT IS A SUBROGRAM.

-- EG  02/15/84

PROCEDURE B84006A IS

BEGIN

     -- RENAMING OF OBJECTS.

     DECLARE

          C1 : INTEGER := 0;

          PACKAGE P1 IS
               U1 : INTEGER RENAMES C1;
          END P1;
          PACKAGE P2 IS
               U1 : INTEGER RENAMES C1;
          END P2;

          USE P1, P2;

     BEGIN

          C1 := U1 + 1;                                -- ERROR:

     END;

     -- RENAMING OF EXCEPTIONS.

     DECLARE

          PACKAGE P1 IS
               C_ER : EXCEPTION RENAMES CONSTRAINT_ERROR;
          END P1;
          PACKAGE P2 IS
               C_ER : EXCEPTION RENAMES CONSTRAINT_ERROR;
          END P2;

          USE P1, P2;

     BEGIN

          RAISE C_ER;                                  -- ERROR:

     EXCEPTION

          WHEN C_ER =>                                 -- ERROR:
               NULL;

     END;

     -- RENAMING OF PACKAGES.

     DECLARE

          PACKAGE P1 IS
               I : INTEGER;
          END P1;
          PACKAGE P2 IS
               PACKAGE P4 RENAMES P1;
          END P2;
          PACKAGE P3 IS
               PACKAGE P4 RENAMES P1;
          END P3;

          USE P2, P3;

     BEGIN

          P4.I := 1;                                   -- ERROR:

     END;

     -- RENAMING OF SUBPROGRAMS.

     DECLARE

          PROCEDURE PROC1;

          PACKAGE P1 IS
               PROCEDURE PROC2 RENAMES PROC1;
          END P1;
          PACKAGE P2 IS
               PROCEDURE PROC2 RENAMES PROC1;
          END P2;

          PROCEDURE PROC1 IS
          BEGIN
               NULL;
          END PROC1;

          USE P1, P2;

     BEGIN

          PROC2;             -- ERROR: AMBIGUITY - TWO VISIBLE PROC2.

     END;

END B84006A;
