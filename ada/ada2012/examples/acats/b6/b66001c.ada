-- B66001C.ADA

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
-- CHECK THAT A SUBPROGRAM CANNOT HAVE THE SAME IDENTIFIER AS A 
--   VARIABLE, TYPE, SUBTYPE, CONSTANT, NUMBER, ARRAY, GENERIC
--   UNIT, OR PACKAGE DECLARED PREVIOUSLY IN THE SAME DECLARATIVE PART.

-- DAS 2/3/81
-- CPP 6/8/84

PROCEDURE B66001C IS
BEGIN

     DECLARE
          P : INTEGER;
          PROCEDURE P IS           -- ERROR: DUPLICATE NAME P.
          BEGIN
               NULL;
          END P;
     BEGIN
          NULL;
     END;

     DECLARE
          TYPE P IS (P0,P1,P2);
          PROCEDURE P IS           -- ERROR: DUPLICATE NAME P.
          BEGIN
               NULL;
          END P;
     BEGIN
          NULL;
     END;

     DECLARE
          SUBTYPE P IS INTEGER RANGE 1..10;
          PROCEDURE P IS           -- ERROR: DUPLICATE NAME P.
          BEGIN
               NULL;
          END P;
     BEGIN
          NULL;
     END;

     DECLARE
          P : CONSTANT INTEGER := 2;
          PROCEDURE P IS           -- ERROR: DUPLICATE NAME P.
          BEGIN
               NULL;
          END P;
     BEGIN
          NULL;
     END;

     DECLARE
          P : CONSTANT := 7;
          PROCEDURE P IS           -- ERROR: DUPLICATE NAME P.
          BEGIN
               NULL;
          END P;
     BEGIN
          NULL;
     END;

     DECLARE
          P : ARRAY (1..3) OF INTEGER;
          PROCEDURE P IS           -- ERROR: DUPLICATE NAME P.
          BEGIN
               NULL;
          END P;
     BEGIN
          NULL;
     END;

     DECLARE
          GENERIC
          PROCEDURE P;
          PROCEDURE P (A : INTEGER);  -- ERROR: DUPLICATE NAME P.

          PROCEDURE P IS
               PROCEDURE P (B : INTEGER) IS  -- OK.
               BEGIN
                    NULL;
               END P;
          BEGIN
               NULL;
          END P;
     BEGIN
          NULL;
     END;

     DECLARE
          PACKAGE P IS
          END P;
          PROCEDURE P IS           -- ERROR: DUPLICATE NAME P.
          BEGIN
               NULL;
          END P;
     BEGIN
          NULL;
     END;

END B66001C;
