-- B85012A.ADA

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
-- CHECK THAT:

--   A) A RENAMING IS ILLEGAL IF THE PARAMETER MODES ARE NOT THE SAME.

--   B) THE MODES ARE NOT USED TO HELP RESOLVE WHICH SUBPROGRAM OR
--      ENTRY IS BEING RENAMED.

-- EG  02/17/84
-- RLB 03/15/07 Corrected limited function returns.

PROCEDURE B85012A IS

BEGIN

     DECLARE

          PROCEDURE P1 (A : IN OUT INTEGER);
          PROCEDURE P1A (B : IN OUT INTEGER) RENAMES P1; -- OK.
          PROCEDURE P1B (C : IN INTEGER) RENAMES P1;     -- ERROR: A.
          PROCEDURE P1C (D : OUT INTEGER) RENAMES P1;    -- ERROR: A.

          PROCEDURE P2 (A : IN INTEGER);
          PROCEDURE P2A (B : IN OUT INTEGER) RENAMES P2; -- ERROR: A.
          PROCEDURE P2B (C : IN INTEGER) RENAMES P2;     -- OK.
          PROCEDURE P2C (D : OUT INTEGER) RENAMES P2;    -- ERROR: A.

          PROCEDURE P3 (A :  OUT INTEGER);
          PROCEDURE P3A (B : IN OUT INTEGER) RENAMES P3; -- ERROR: A.
          PROCEDURE P3B (C : IN INTEGER) RENAMES P3;     -- ERROR: A.
          PROCEDURE P3C (D : OUT INTEGER) RENAMES P3;    -- OK.

          PROCEDURE P1 (A : IN OUT INTEGER) IS
          BEGIN
               NULL;
          END P1;

          PROCEDURE P2 (A : IN INTEGER) IS
          BEGIN
               NULL;
          END P2;

          PROCEDURE P3 (A :  OUT INTEGER) IS
          BEGIN
               NULL;
          END P3;

     BEGIN

          NULL;

     END;

     DECLARE

          PACKAGE P1 IS
               PROCEDURE PROC1 (A : INTEGER);
          END P1;

          PACKAGE P2 IS
               PROCEDURE PROC1 (A : IN OUT INTEGER);
          END P2;

          USE P1, P2;

          PROCEDURE PROC2 (A : INTEGER) RENAMES PROC1; -- ERROR: B.
          PROCEDURE PROC3 (A : IN OUT INTEGER)
                                        RENAMES PROC1; -- ERROR: B.
          PACKAGE BODY P1 IS
               PROCEDURE PROC1 (A : INTEGER) IS
               BEGIN
                    NULL;
               END PROC1;
          END P1;

          PACKAGE BODY P2 IS
               PROCEDURE PROC1 (A : IN OUT INTEGER) IS
               BEGIN
                    NULL;
               END PROC1;
          END P2;

     BEGIN

          NULL;

     END;

     DECLARE

          TASK TYPE T1 IS
               ENTRY E (A : INTEGER);
               ENTRY E;
          END T1;

          TASK TYPE T2 IS
               ENTRY E (A : IN OUT INTEGER);
          END T2;

          FUNCTION FUN RETURN T1;
          FUNCTION FUN RETURN T2;

          PROCEDURE PROC1(A : INTEGER) RENAMES FUN.E;  -- ERROR: B.
          PROCEDURE PROC2 RENAMES FUN.E;               -- OK.

          TASK BODY T1 IS
          BEGIN
               NULL;
          END T1;

          TASK BODY T2 IS
          BEGIN
               NULL;
          END T2;

          FUNCTION FUN RETURN T1 IS
          BEGIN
               RETURN TSK1 : T1;
          END FUN;

          FUNCTION FUN RETURN T2 IS
          BEGIN
               RETURN TSK2 : T2;
          END FUN;

     BEGIN

          NULL;

     END;

END B85012A;
