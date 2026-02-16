-- B83F02C.ADA

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
--     CHECK THAT SUBPROGRAM REDECLARATIONS ARE FORBIDDEN.
--     SUBTESTS ARE:
--            (A) ONE DECLARATION IN THE VISIBLE PART OF A PACKAGE
--                 AND THE OTHER IN THE PRIVATE PART OR BODY.
--            (B) ONE DECLARATION IN THE PRIVATE PART OF A PACKAGE
--                 AND THE OTHER IN THE BODY.

-- HISTORY:
--     DAS 02/03/81  CREATED ORIGINAL TEST.
--     RDH 04/27/90  FORMATTED HEADER AND ADDED "-- OPTIONAL ERR
--                   MSG."
--     THS 09/25/90  ADDED CHECK WHERE PARAMETER NAMES ARE DIFFERENT
--                   IN THE TWO DECLARATION AND RENAMED FROM
--                   B83F04A.ADA.
--     RLB 06/29/16  UPDATED ERROR MARKERS TO STANDARDS.
PROCEDURE B83F02C IS
BEGIN

     --------------------------------------------------

     DECLARE   -- (A)

          PACKAGE PKGA IS
               SUBTYPE INT IS INTEGER RANGE 1..10;
               PROCEDURE PA1  (X : IN OUT INTEGER); -- OPTIONAL ERROR:
               PROCEDURE PA2  (X : IN OUT INTEGER); -- OPTIONAL ERROR:
               PROCEDURE PA3  (X : IN OUT INTEGER); -- OPTIONAL ERROR:
               PROCEDURE PA4  (X : IN OUT INTEGER);
               PROCEDURE PA5  (X : IN OUT INTEGER);
               PROCEDURE PA6  (X : IN OUT INTEGER);
               PROCEDURE PA7  (X : IN OUT INTEGER); -- OPTIONAL ERROR:
               FUNCTION FA1  (X : INTEGER) RETURN INTEGER; -- OPTIONAL ERROR:

               FUNCTION FA2  (X : INTEGER) RETURN INTEGER; -- OPTIONAL ERROR:

               FUNCTION FA3  (X : INTEGER) RETURN INTEGER;
               FUNCTION FA4  (X : INTEGER := 1)
                                           RETURN INTEGER; -- OPTIONAL ERROR:

               FUNCTION FA5  (X : INTEGER := 1) RETURN INTEGER;
          PRIVATE
               PROCEDURE PA1  (X : IN OUT INTEGER);   -- ERROR:
                                                      -- REDECLARES PA1
               PROCEDURE PA2  (X : IN INT);           -- ERROR:
                                                      -- REDECLARES PA2
               PROCEDURE PA3  (X : OUT INTEGER);      -- ERROR:
                                                      -- REDECLARES PA3
               PROCEDURE PA7  (Y : IN OUT INTEGER);   -- ERROR:
                                                      -- REDECLARES PA7
               FUNCTION FA1 (X : INTEGER) RETURN INTEGER; -- ERROR:
                                                      -- REDECLARES FA1
               FUNCTION FA2 (X : INTEGER) RETURN INT; -- ERROR:
                                                      -- REDECLARES FA2
               FUNCTION FA4 (X : INTEGER := 4) RETURN INTEGER; -- ERROR:
                                                      -- REDECLARES FA4
          END PKGA;

          PACKAGE BODY PKGA IS

               PROCEDURE PA4 (X : IN OUT INTEGER) IS
               BEGIN
                    NULL;
               END PA4;

               PROCEDURE PA5 (X : IN OUT INTEGER) IS
               BEGIN
                    NULL;
               END PA5;

               PROCEDURE PA6 (X : IN OUT INTEGER) IS
               BEGIN
                    NULL;
               END PA6;

               PROCEDURE PA7 (X : IN OUT INTEGER) IS
               BEGIN
                    NULL;
               END PA7;

               FUNCTION FA3 (X : INTEGER) RETURN INTEGER IS
               BEGIN
                    RETURN 1;
               END FA3;

               FUNCTION FA5 (X : INTEGER := 1) RETURN INTEGER IS
               BEGIN
                    RETURN 2;
               END FA5;

               PROCEDURE PA4 (X : IN OUT INTEGER) IS  -- ERROR:
                                                      -- REDECLARES PA4
               BEGIN
                    NULL;
               END PA4;

               PROCEDURE PA5 (X : IN OUT INT) IS      -- ERROR:
                                                      -- REDECLARES PA5
               BEGIN
                    NULL;
               END PA5;

               PROCEDURE PA6 (X : IN INTEGER) IS      -- ERROR:
                                                      -- REDECLARES PA6
               BEGIN
                    NULL;
               END PA6;

               PROCEDURE PA7 (X : IN OUT INTEGER) IS  -- ERROR:
                                                      -- REDECLARES PA7
               BEGIN
                    NULL;
               END PA7;

               FUNCTION FA3 (X : INTEGER) RETURN INTEGER IS -- ERROR:
                                                      -- REDECLARES FA3
               BEGIN
                    RETURN 5;
               END FA3;

               FUNCTION FA5(X : INTEGER := 7) RETURN INTEGER IS-- ERROR:
                                                      -- REDECLARES FA5
               BEGIN
                    RETURN 7;
               END FA5;

          END PKGA;

     BEGIN     -- (A)
          NULL;
     END;      -- (A)

     --------------------------------------------------

     DECLARE   -- (B)

          PACKAGE PKGB IS
               SUBTYPE INT IS INTEGER RANGE 1..10;
          PRIVATE
               PROCEDURE PB1 (X : OUT INTEGER);
               PROCEDURE PB2 (X : OUT INTEGER);
               PROCEDURE PB3 (X : OUT INTEGER);
               PROCEDURE PB4 (X : OUT INTEGER);
               FUNCTION FB1 (X : INTEGER) RETURN INTEGER;
               FUNCTION FB2 (X : INTEGER) RETURN INTEGER;
               FUNCTION FB3 (X : INTEGER) RETURN INTEGER;
               FUNCTION FB4 (X : INTEGER := 1) RETURN INTEGER;
               FUNCTION FB5 (X : INTEGER := 1) RETURN INTEGER;
          END PKGB;

          PACKAGE BODY PKGB IS

               PROCEDURE PB1 (X : OUT INTEGER) IS
               BEGIN
                    NULL;
               END PB1;

               FUNCTION FB4 (X : INTEGER := 1) RETURN INTEGER IS
               BEGIN
                    RETURN 2;
               END FB4;

               PROCEDURE PB2 (X : IN OUT INTEGER);    -- ERROR:
                                                      -- REDECLARES PB2

               PROCEDURE PB3 (X : IN OUT INT);        -- ERROR:
                                                      -- REDECLARES PB3

               PROCEDURE PB4 (X : IN INTEGER);        -- ERROR:
                                                      -- REDECLARES PB4

               FUNCTION FB1 (Y : INTEGER) RETURN INTEGER; -- ERROR:
                                                      -- REDECLARES FB1

               FUNCTION FB2 (X : INTEGER) RETURN INTEGER; -- ERROR:
                                                      -- REDECLARES FB2

               FUNCTION FB3 (X : INTEGER) RETURN INT;   -- ERROR:
                                                      -- REDECLARES FB3

               FUNCTION FB5(X : INTEGER := 7) RETURN INTEGER; -- ERROR:
                                                      -- REDECLARES FB5

          END PKGB;  -- OPTIONAL ERROR:  MESSAGES ABOUT MISSING BODIES.

     BEGIN     -- (B)
          NULL;
     END;      -- (B)

END B83F02C;
