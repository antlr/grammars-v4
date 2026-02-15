-- B83008B.ADA

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
-- OBJECTIVE:
--     CHECK THAT A SUBPROGRAM DECLARATION AND A NON-OVERLOADABLE
--     DECLARATION, BOTH GIVEN IN THE DECLARATIVE PART OF A BLOCK
--     STATEMENT, CANNOT BE HOMOGRAPHS.

-- HISTORY:
--     VCL  03/08/88  CREATED ORIGINAL TEST.

PROCEDURE B83008B IS

-- MULTIPLE BLOCKS ARE USED SO THAT ONLY ONE SUBPROGRAM HOMOGRAPH WHICH
-- REQUIRES A BODY IS DECLARED IN EACH BLOCK.

BEGIN
     DECLARE
          TYPE D1 IS RANGE 1..10;

     --  DECLARATION OF HOMOGRAPH.

          PROCEDURE P;
          PROCEDURE D1 RENAMES P;                   -- ERROR: HOMOGRAPH.

          PROCEDURE P IS
          BEGIN
               NULL;
          END P;

     BEGIN
          NULL;
     END;

     DECLARE
          TYPE D1 IS RANGE 1..10;

     --  DECLARATION OF HOMOGRAPH.

          PROCEDURE D1 IS                           -- ERROR: HOMOGRAPH.
          BEGIN
               NULL;
          END D1;

     BEGIN
          NULL;
     END;

     DECLARE
          SUBTYPE D2 IS INTEGER RANGE 1..10;

     --  DECLARATION OF HOMOGRAPH.

          PROCEDURE P (P1 : BOOLEAN);
          PROCEDURE D2 (FP : BOOLEAN) RENAMES P;    -- ERROR: HOMOGRAPH.

          PROCEDURE P (P1 : BOOLEAN) IS
          BEGIN
               NULL;
          END P;

     BEGIN
          NULL;
     END;

     DECLARE
          SUBTYPE D2 IS INTEGER RANGE 1..10;

     --  DECLARATION OF HOMOGRAPH.

          PROCEDURE D2 (P1 : INTEGER) IS            -- ERROR: HOMOGRAPH.
          BEGIN
               NULL;
          END D2;
     BEGIN
          NULL;
     END;

     DECLARE
          D3 : CONSTANT INTEGER := 10;

     --  DECLARATION OF HOMOGRAPH.

          FUNCTION F RETURN BOOLEAN;
          FUNCTION D3 RETURN BOOLEAN RENAMES F;     -- ERROR: HOMOGRAPH.

          FUNCTION F RETURN BOOLEAN IS
          BEGIN
               RETURN FALSE;
          END F;

     BEGIN
          NULL;
     END;

     DECLARE
          D3 : CONSTANT INTEGER := 10;

     --  DECLARATION OF HOMOGRAPH.

          FUNCTION D3 RETURN INTEGER IS             -- ERROR: HOMOGRAPH.
          BEGIN
               RETURN 0;
          END D3;

     BEGIN
          NULL;
     END;

     DECLARE
          D4 : CONSTANT := 8;

     --  DECLARATION OF HOMOGRAPH.

          FUNCTION F (P1 : BOOLEAN) RETURN INTEGER;
          FUNCTION D4 (P1 : BOOLEAN) RETURN INTEGER RENAMES F; -- ERROR:
                                                           -- HOMOGRAPH.

          FUNCTION F (P1 : BOOLEAN) RETURN INTEGER IS
          BEGIN
               RETURN 0;
          END F;
     BEGIN
          NULL;
     END;

     DECLARE
          D4 : CONSTANT := 8;

     --  DECLARATION OF HOMOGRAPH.

          FUNCTION D4 (P1 : INTEGER) RETURN BOOLEAN IS         -- ERROR:
          BEGIN                                            -- HOMOGRAPH.
               RETURN FALSE;
          END D4;

     BEGIN
          NULL;
     END;
END B83008B;
