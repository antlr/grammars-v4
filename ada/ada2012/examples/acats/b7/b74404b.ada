-- B74404B.ADA

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
--     CHECK THAT ASSIGNMENT AND EQUALITY ARE NOT
--     AVAILABLE FOR LIMITED COMPOSITE TYPES.

-- HISTORY:
--     BCB 07/15/88  CREATED ORIGINAL TEST.
--     RLB 03/20/07  CORRECTED OBJECTIVE AND ERROR REASONS
--                   FOR AMENDMENT 1; ADDED TEST CASES.

PROCEDURE B74404B IS

     PACKAGE P IS
          TYPE LP IS LIMITED PRIVATE;
          C1, C2, C3, C4, C5 : CONSTANT LP;

          TYPE ARR IS ARRAY(1..5) OF LP;

          TYPE REC (D : INTEGER := 0) IS RECORD
               COMP1 : LP;
               COMP2 : INTEGER;
               COMP3 : BOOLEAN;
          END RECORD;

          FUNCTION F (V : IN INTEGER) RETURN LP;

          PROCEDURE INIT (ONE : IN OUT ARR);

          FUNCTION INIT RETURN REC;
     PRIVATE
          TYPE LP IS RANGE 1 .. 100;
          C1 : CONSTANT LP := 1;
          C2 : CONSTANT LP := 2;
          C3 : CONSTANT LP := 3;
          C4 : CONSTANT LP := 4;
          C5 : CONSTANT LP := 5;
     END P;

     USE P;

     A, B : ARR;

     C : ARR := (C1,C2,C3,C4,C5);       -- ERROR: CANNOT INIT W/ OBJECT.

     R, S : REC;

     T : REC := (0,C1,2,TRUE);          -- ERROR: CANNOT INIT W/ OBJECT.

     PACKAGE BODY P IS
          FUNCTION F (V : IN INTEGER) RETURN LP IS
          BEGIN
               RETURN LP(V);
          END F;

          PROCEDURE INIT (ONE : IN OUT ARR) IS
          BEGIN
               ONE := (1,2,3,4,5);
          END INIT;

          FUNCTION INIT RETURN REC IS
          BEGIN
               RETURN (0,1,0,TRUE);
          END INIT;
     BEGIN
          NULL;
     END P;

     X : ARR := (F(1),F(2),F(3),F(4),F(5)); -- OK.

     Y : REC := (0,F(1),2,TRUE);        -- OK.

BEGIN

     A := (C1,C2,C3,C4,C5);             -- ERROR: ARRAY ASSIGNMENT.

     INIT(B);

     A := B;                            -- ERROR: ARRAY ASSIGNMENT.

     R := INIT;                         -- ERROR: RECORD ASSIGNMENT.

     R := (0,C1,2,TRUE);                -- ERROR: RECORD ASSIGNMENT.

     IF A = B THEN                      -- ERROR: ARRAY EQUALITY.
          NULL;
     END IF;

     IF A /= B THEN                     -- ERROR: ARRAY INEQUALITY.
          NULL;
     END IF;

     IF R = S THEN                      -- ERROR: RECORD EQUALITY.
          NULL;
     END IF;

     IF R /= S THEN                     -- ERROR: RECORD INEQUALITY.
          NULL;
     END IF;

END B74404B;
