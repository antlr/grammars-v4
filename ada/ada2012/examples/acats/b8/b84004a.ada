-- B84004A.ADA

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
-- CHECK THAT IF THE SET OF POTENTIALLY VISIBLE DECLARATIONS INCLUDES
-- A MIXTURE OF SUBPROGRAM OR ENUMERATION LITERAL DECLARATIONS
-- TOGETHER WITH A DECLARATION OF SOME OTHER KIND OF ENTITY, NONE OF
-- THE DECLARATIONS ARE MADE VISIBLE.

-- EG  02/17/84

PROCEDURE B84004A IS

BEGIN

     DECLARE

          PACKAGE P1 IS
               TYPE LET IS (W, X, Y, Z);
               PROCEDURE Y;
          END P1;

          USE P1;

          PACKAGE P2 IS
               Z : INTEGER := 10;
               FUNCTION X (A : INTEGER := 20) RETURN LET;
               FUNCTION X (B : BOOLEAN := FALSE) RETURN LET;
          END P2;

          PACKAGE P3 IS
               X : INTEGER := 15;
          END P3;

          A : LET := X;                                -- OK.

          USE P2;

          PACKAGE BODY P1 IS
               PROCEDURE Y IS
               BEGIN
                    NULL;
               END Y;
          END P1;

          PACKAGE BODY P2 IS
               FUNCTION X (A : INTEGER := 20) RETURN LET IS
               BEGIN
                    RETURN W;
               END X;
               FUNCTION X (B : BOOLEAN := FALSE) RETURN LET IS
               BEGIN
                    RETURN W;
               END X;
          END P2;

     BEGIN

          A := X(15);                                  -- OK.
          A := X(TRUE);                                -- OK.

          DECLARE

               USE P3;

               B : INTEGER := X;                       -- ERROR:
               C : LET := Y;                           -- OK.

          BEGIN

               A := X(15);                             -- ERROR:
               Y;                                      -- OK.

          END;

     END;

     DECLARE

          PACKAGE P1 IS
               X : INTEGER := 15;
          END P1;

          PACKAGE P2 IS
               X : BOOLEAN := TRUE;
          END P2;

          USE P1;

          A : INTEGER := X;                            -- OK.

          USE P2;

          B : INTEGER := X;                            -- ERROR:
          C : BOOLEAN := X;                            -- ERROR:

     BEGIN

          NULL;

     END;

END B84004A;
