-- B83030B.ADA

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
--     WITHIN A GENERIC SUBPROGRAM BODY THE USE OF AN IDENIFIER OF A
--     HOMOGRAPHIC SUBPROGRAM DECLARED IN AN OUTER DECLARATIVE REGION
--     IS NOT ALLOWED IF IT WOULD BE ILLEGAL AS A REFERENCE TO THE
--     GENERIC SUBPROGRAM.

-- HISTORY:
--     RJW 11/07/88  CREATED ORIGINAL TEST.

PROCEDURE B83030B IS

BEGIN
     ONE:
     DECLARE
          GENERIC
               Y : INTEGER;
          PROCEDURE P (X : INTEGER);

          PROCEDURE P (X : INTEGER) IS
          BEGIN
               NULL;
          END P;

     BEGIN
          DECLARE
               GENERIC
               PROCEDURE P;

               PROCEDURE P IS
                    A : INTEGER := 2;
                    PROCEDURE NP IS NEW P (A);                -- ERROR:
               BEGIN
                    NULL;
               END P;

          BEGIN
               NULL;
          END;
     END ONE;


     TWO:
     DECLARE
          GENERIC
               WITH PROCEDURE Q;
          FUNCTION P RETURN STRING;

          FUNCTION P RETURN STRING IS
          BEGIN
               RETURN "LET'S HEAR IT FOR THE TESTWRITERS";
          END;
     BEGIN
          DECLARE
               GENERIC
                    TYPE T IS (<>);
               PROCEDURE P (X : T);

               PROCEDURE P (X : T) IS
                    PROCEDURE Q IS
                    BEGIN
                         NULL;
                    END Q;

                    FUNCTION NP IS NEW P (Q);                 -- ERROR:
               BEGIN
                    NULL;
               END P;
          BEGIN
               NULL;
          END;
     END TWO;

     THREE:
     DECLARE

          GENERIC
               TYPE T IS PRIVATE;
          FUNCTION F (X, Y : FLOAT) RETURN BOOLEAN;

          FUNCTION F (X, Y : FLOAT) RETURN BOOLEAN IS
          BEGIN
               RETURN FALSE;
          END F;
     BEGIN
          DECLARE
               GENERIC
               FUNCTION F RETURN INTEGER;

               FUNCTION F RETURN INTEGER IS
                    FUNCTION NF IS NEW F (STRING);            -- ERROR:
               BEGIN
                    RETURN 3;
               END F;

          BEGIN
               NULL;
          END;
     END THREE;

     FOUR:
     DECLARE
          GENERIC
               WITH PROCEDURE P;
          PROCEDURE F (X : INTEGER);

          PROCEDURE F (X : INTEGER) IS
          BEGIN
               NULL;
          END F;

     BEGIN
          DECLARE
               GENERIC
                    TYPE T IS (<>);
               FUNCTION F RETURN T;

               FUNCTION F RETURN T IS
                    PROCEDURE NF IS NEW F (STRING);        -- ERROR:
               BEGIN
                    RETURN T'LAST;
               END F;

          BEGIN
               NULL;
          END;
     END FOUR;

END B83030B;
