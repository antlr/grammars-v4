-- B35701A.ADA

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
--     ERRORS IN REAL TYPE DEFINITIONS:
--        (A) EXPRESSION AFTER DIGITS IS REAL
--        (B) DIGITS 0, NEGATIVE
--        (C) NOT STATIC_EXPRESSION AFTER DIGITS
--        (D) FLOATING POINT PRECISION TOO HIGH
--        (E) CHECK REAL RANGE NOT INTEGER

-- HISTORY:
--     BAW 05/09/80  CREATED ORIGINAL TEST.
--     BCB 08/01/88  MODIFIED HEADER FORMAT, CHANGED FROM A .'TST' TEST,
--                   CHANGED $MAX_DIGITS TO SYSTEM.MAX_DIGITS, AND ADDED
--                   CHECKS FOR THE UPPER AND LOWER BOUNDS OF A RANGE
--                   OUTSIDE THE RANGE OF THE SAFE NUMBERS.
--     PWN 12/27/94  REMOVED TESTS INCONSISTENT WITH ADA 9X.

WITH SYSTEM;

PROCEDURE B35701A IS

     TYPE MY_R1 IS DIGITS 1.0; -- ERROR: EXPRESSION AFTER DIGITS
                               --           IS NOT INTEGER
     TYPE MY_R2 IS DIGITS 0;   -- ERROR: EXPRESSION AFTER DIGITS
                               --         HAS VALUE ZERO
     TYPE MY_R3 IS DIGITS -1;  -- ERROR: NEGATIVE DIGITS VALUE
     D : INTEGER := 1;
     TYPE MY_R4 IS DIGITS D;   -- ERROR: EXPRESSION AFTER DIGITS
                               --        IS NOT STATIC
     M : CONSTANT := SYSTEM.MAX_DIGITS + 1;

     TYPE MY_R5 IS DIGITS M;   -- ERROR: EXPRESSION AFTER DIGITS
                               --        HAS VALUE MAX_DIGITS+1
     TYPE MY_REAL IS DIGITS 5
                  RANGE -1..1; -- ERROR: INTEGER EXPRESSION IN
                               --        RANGE CONSTRAINT



BEGIN
      NULL;
END B35701A;
