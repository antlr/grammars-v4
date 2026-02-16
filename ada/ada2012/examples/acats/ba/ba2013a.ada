-- BA2013A.ADA

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
-- CHECK THAT USE_CLAUSES GIVEN FOR ANCESTORS OF A SUBUNIT ARE COMBINED
-- WITH ANY USE_CLAUSE GIVEN FOR THE SUBUNIT ITSELF.

-- BHS 8/07/84

PACKAGE BA2013A_P1 IS

     INT : INTEGER;
     PROCEDURE PROC;

END BA2013A_P1;

PACKAGE BODY BA2013A_P1 IS

     PROCEDURE PROC IS
     BEGIN
          NULL;
     END PROC;

BEGIN
     INT := 1;
END BA2013A_P1;


PACKAGE BA2013A_P2 IS

     INT : INTEGER;
     PROCEDURE PROC (Z : INTEGER := 3);

END BA2013A_P2;

PACKAGE BODY BA2013A_P2 IS

     PROCEDURE PROC (Z : INTEGER := 3) IS
     BEGIN
          NULL;
     END PROC;

BEGIN
     INT := 2;
END BA2013A_P2;

WITH BA2013A_P1;
USE BA2013A_P1;
PROCEDURE BA2013A IS

     PROCEDURE BA2013A_P3 IS SEPARATE;

BEGIN

     NULL;

END BA2013A;


WITH BA2013A_P2;
USE BA2013A_P2;
SEPARATE (BA2013A)
PROCEDURE BA2013A_P3 IS

     X : INTEGER := INT;         -- ERROR: INT IS NOT DIRECTLY VISIBLE.

BEGIN

     PROC;                      -- ERROR: PROC IS AMBIGUOUS.

END BA2013A_P3;
