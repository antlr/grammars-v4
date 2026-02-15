-- B33302A.ADA

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
-- CHECK THAT OPERATIONS ARE NOT DECLARED IMPLICITLY BEFORE THE TYPE
-- DEFINITION IN A TYPE DECLARATION.

-- TBN  10/6/86

PROCEDURE B33302A IS

     PACKAGE P IS
          TYPE T IS RANGE 1 .. P."+"(1, 1);                    -- ERROR:
     END P;

     PACKAGE Q IS
          TYPE T IS DIGITS 5;
          FUNCTION F RETURN T;
     END Q;

     PACKAGE P1 IS
          TYPE T1 IS NEW FLOAT RANGE 1.0 .. P1."+"(1.0, 1.0);  -- ERROR:
          TYPE T2 IS NEW Q.T RANGE 0.0 .. Q.T(P1.F);           -- ERROR:
          TYPE T3 IS
               NEW BOOLEAN RANGE FALSE .. BOOLEAN(P1.TRUE);    -- ERROR:
          TYPE T4 IS PRIVATE;
          TYPE T5 IS PRIVATE;
          TYPE T6 IS PRIVATE;
     PRIVATE
          TYPE T4 IS RANGE 1 .. BOOLEAN'POS(P1."="(1, 1));     -- ERROR:
          TYPE T5 IS ACCESS BOOLEAN
               RANGE FALSE .. P1."="(NEW BOOLEAN, NULL);       -- ERROR:
          TYPE T6 IS
               ARRAY (FALSE .. P1."="("AB", ('A', 'B')))       -- ERROR:
                    OF CHARACTER;
     END P1;

     PACKAGE P2 IS
          TYPE T7 (D : INTEGER) IS PRIVATE;
          FUNCTION F RETURN T7;
     PRIVATE
          TYPE T7 (D : INTEGER) IS
               RECORD
                    C1 : INTEGER := F.D;
                    C2 : INTEGER := F.C1;                      -- ERROR:
               END RECORD;
     END P2;

     PACKAGE BODY Q IS
          FUNCTION F RETURN T IS
               X : T := 1.0;
          BEGIN
               RETURN (X);
          END F;
     END Q;

     PACKAGE BODY P2 IS
          FUNCTION F RETURN T7 IS
               A : T7 (2);
          BEGIN
               RETURN (A);
          END F;
     END P2;

BEGIN
     NULL;
END B33302A;
