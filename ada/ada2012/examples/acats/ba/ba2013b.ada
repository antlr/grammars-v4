-- BA2013B.ADA

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
-- CHECK THAT AN IDENTIFIER DECLARED IN A LIBRARY PACKAGE NAMED IN A
-- USE_CLAUSE IS NOT VISIBLE IN A SUBUNIT IF THE IDENTIFIER IS DECLARED
-- IN A PARENT UNIT AND IS DIRECTLY VISIBLE AT THE STUB.

-- BHS 8/07/84

PROCEDURE BA2013B IS

     VAR : INTEGER;
     F : FLOAT;
     PROCEDURE BA2013B_SUB IS SEPARATE;

BEGIN
     NULL;
END BA2013B;


PACKAGE BA2013B_PACK IS

     VAR : BOOLEAN := FALSE;
     FUNCTION F RETURN BOOLEAN;

END BA2013B_PACK;

PACKAGE BODY BA2013B_PACK IS

     FUNCTION F RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END F;

END BA2013B_PACK;

WITH BA2013B_PACK;
USE BA2013B_PACK;
SEPARATE (BA2013B)
PROCEDURE BA2013B_SUB IS
BEGIN
     
     IF NOT VAR THEN          -- ERROR: ONLY BA2013B.VAR VISIBLE.
          NULL;
     END IF;

     VAR := 5;                      -- OK.

     IF NOT BA2013B_PACK.VAR THEN   -- OK.
          NULL;
     END IF;

     IF F THEN                -- ERROR: ONLY BA2013B.F VISIBLE.
          NULL;
     END IF;

     F := 6.0;                      -- OK.

     IF BA2013B_PACK.F THEN         -- OK.
          NULL;
     END IF;

END BA2013B_SUB;
