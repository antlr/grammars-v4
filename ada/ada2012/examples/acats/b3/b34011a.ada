-- B34011A.ADA

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
--     CHECK THAT A DERIVED TYPE DECLARATION IS NOT CONSIDERED EXACTLY
--     EQUIVALENT TO AN ANONYMOUS DECLARATION OF THE DERIVED TYPE
--     FOLLOWED BY A SUBTYPE DECLARATION OF THE DERIVED SUBTYPE.  IN
--     PARTICULAR, CHECK THAT A DERIVED FUNCTION CANNOT BE USED IN THE
--     DERIVED TYPE DECLARATION ITSELF.

-- HISTORY:
--     JRK 09/03/87  CREATED ORIGINAL TEST.

PROCEDURE B34011A IS

     TYPE ARR IS ARRAY (POSITIVE RANGE <>) OF INTEGER;

     TYPE REC (D : INTEGER := 0) IS
          RECORD
               I : INTEGER;
          END RECORD;

     PACKAGE PT IS
          TYPE PRIV (D : INTEGER := 0) IS PRIVATE;
          C : CONSTANT PRIV;
     PRIVATE
          TYPE PRIV (D : INTEGER := 0) IS
               RECORD
                    I : INTEGER;
               END RECORD;
          C : CONSTANT PRIV := (1, 2);
     END PT;

     USE PT;

     TYPE ACC_ARR IS ACCESS ARR;

     TYPE ACC_REC IS ACCESS REC;

     PACKAGE P IS

          TYPE T1 IS NEW BOOLEAN;
          FUNCTION F RETURN T1;

          TYPE T2 IS NEW INTEGER;
          FUNCTION F RETURN T2;

          TYPE T3 IS NEW FLOAT;
          FUNCTION F RETURN T3;

          TYPE T4 IS NEW DURATION;
          FUNCTION F RETURN T4;

          TYPE T5 IS NEW ARR;
          FUNCTION F RETURN T5;

          TYPE T6 IS NEW REC;
          FUNCTION F RETURN T6;

          TYPE T7 IS NEW PRIV;
          FUNCTION F RETURN T7;

          TYPE T8 IS NEW ACC_ARR;
          FUNCTION F RETURN T8;

          TYPE T9 IS NEW ACC_REC;
          FUNCTION F RETURN T9;

     END P;

     USE P;

     PACKAGE Q1 IS
          TYPE U IS NEW T1 RANGE FALSE .. T1 (Q1.F);   -- ERROR: Q1.F.
     END Q1;

     PACKAGE Q2 IS
          TYPE U IS NEW T2 RANGE T2 (Q2.F) .. 10;      -- ERROR: Q2.F.
     END Q2;

     PACKAGE Q3 IS
          TYPE U IS NEW T3 RANGE 0.0 .. T3 (Q3.F);     -- ERROR: Q3.F.
     END Q3;

     PACKAGE Q4 IS
          TYPE U IS NEW T4 RANGE T4 (Q4.F) .. 10.0;    -- ERROR: Q4.F.
     END Q4;

     PACKAGE Q5 IS
          TYPE U IS NEW T5 (1 .. Q5.F (1));            -- ERROR: Q5.F.
     END Q5;

     PACKAGE Q6 IS
          TYPE U IS NEW T6 (Q6.F.I);                   -- ERROR: Q6.F.
     END Q6;

     PACKAGE Q7 IS
          TYPE U IS NEW T7 (Q7.F.D);                   -- ERROR: Q7.F.
     END Q7;

     PACKAGE Q8 IS
          TYPE U IS NEW T8 (Q8.F (1) .. 3);            -- ERROR: Q8.F.
     END Q8;

     PACKAGE Q9 IS
          TYPE U IS NEW T9 (Q9.F.I);                   -- ERROR: Q9.F.
     END Q9;

     PACKAGE BODY P IS

          FUNCTION F RETURN T1 IS
          BEGIN
               RETURN TRUE;
          END F;

          FUNCTION F RETURN T2 IS
          BEGIN
               RETURN 0;
          END F;

          FUNCTION F RETURN T3 IS
          BEGIN
               RETURN 10.0;
          END F;

          FUNCTION F RETURN T4 IS
          BEGIN
               RETURN 0.0;
          END F;

          FUNCTION F RETURN T5 IS
          BEGIN
               RETURN (1 .. 3 => 2);
          END F;

          FUNCTION F RETURN T6 IS
          BEGIN
               RETURN (1, 2);
          END F;

          FUNCTION F RETURN T7 IS
          BEGIN
               RETURN T7 (C);
          END F;

          FUNCTION F RETURN T8 IS
          BEGIN
               RETURN NEW ARR'(1 .. 3 => 2);
          END F;

          FUNCTION F RETURN T9 IS
          BEGIN
               RETURN NEW REC'(1, 2);
          END F;

     END P;

BEGIN
     NULL;
END B34011A;
