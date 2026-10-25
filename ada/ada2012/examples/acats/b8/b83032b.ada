-- B83032B.ADA

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
--     CHECK THAT IF AN IMPLICIT DECLARATION OF A PREDEFINED OPERATOR IS
--     HIDDEN BY A DERIVED SUBPROGRAM HOMOGRAPH, THEN A USE OF THE
--     COMMON IDENTIFIER OF THE HOMOGRAPHS MUST BE REJECTED IF IT WOULD
--     BE A LEGAL REFERENCE TO THE IMPLICIT DECLARATION BUT ILLEGAL FOR
--     THE DERIVED SUBPROGRAM.

-- HISTORY:
--     BCB 09/19/88  CREATED ORIGINAL TEST.

PROCEDURE B83032B IS

BEGIN
     DECLARE             -- CHECK PREDEFINED OPERATOR.
          PACKAGE P IS
               TYPE INT IS RANGE -20 .. 20;

               FUNCTION "ABS" (X : INT) RETURN INT;
          END P;
          USE P;

          TYPE NINT IS NEW INT;

          I2 : NINT := -5;

          PACKAGE BODY P IS
               I1 : NINT := 5;

               FUNCTION "ABS" (X : INT) RETURN INT IS
               BEGIN
                    RETURN INT (- (ABS (INTEGER (X))));
               END "ABS";
          BEGIN
               I1 := "ABS" (RIGHT => -10);             -- ERROR:
          END P;
     BEGIN
          I2 := "ABS" (RIGHT => 10);                   -- ERROR:
     END;

END B83032B;
