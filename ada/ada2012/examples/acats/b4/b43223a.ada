-- B43223A.ADA

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
--     CHECK THAT OVERLOADING RESOLUTION MAY NOT USE THE RESTRICTIONS
--     ON THE CONTEXTS IN WHICH AN ARRAY AGGREGATE WITH AN OTHERS
--     CHOICE MAY APPEAR.

-- HISTORY:
--     DHH 08/12/88 CREATED ORIGINAL TEST.

PROCEDURE B43223A IS

     X : INTEGER;
BEGIN

     DECLARE
          TYPE INT IS NEW INTEGER;

          TYPE A1 IS ARRAY(INTEGER RANGE <>) OF INT;
          TYPE A2 IS ARRAY(INTEGER RANGE 1 .. 4) OF INT;

          PROCEDURE PROC(PARAM : A1) IS
          BEGIN
               NULL;
          END PROC;

          PROCEDURE PROC(PARAM : A2) IS
          BEGIN
               NULL;
          END PROC;

     BEGIN
          PROC((1 .. ABS(2) => 1, OTHERS => 0));    -- ERROR: AMBIGUOUS
     END;

     DECLARE
          TYPE INT IS RANGE 1 .. 4;

          GENERIC
               TYPE ONE IS RANGE <>;
          PROCEDURE GEN_PROC(PAR : ONE);

          PROCEDURE GEN_PROC(PAR : ONE) IS
               TYPE A1 IS ARRAY(ONE) OF INTEGER;
               TYPE A2 IS ARRAY(INT) OF INTEGER;

               PROCEDURE PROC(A : A1) IS
               BEGIN
                    NULL;
               END PROC;

               PROCEDURE PROC(A : A2) IS
               BEGIN
                    NULL;
               END PROC;

          BEGIN
               PROC((1 => 1, OTHERS => 0));   -- ERROR: AMBIGUOUS

          END GEN_PROC;

     BEGIN
          NULL;
     END;

     DECLARE
          P : INTEGER := 1;
          Q : INTEGER := 5;

          SUBTYPE SMALL IS INTEGER RANGE P .. Q;

          TYPE B1 IS ARRAY(1 .. 5) OF INTEGER;
          TYPE B2 IS ARRAY(SMALL) OF INTEGER;

          PROCEDURE PROC(PARAM : B1) IS
          BEGIN
               NULL;
          END PROC;

          PROCEDURE PROC(PARAM : B2) IS
          BEGIN
               NULL;
           END PROC;

     BEGIN
          PROC((1, OTHERS => 2));                   -- ERROR: AMBIGUOUS
     END;

END B43223A;
