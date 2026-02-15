-- B35501B.ADA

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
-- CHECK THAT THE PREFIX OF WIDTH, POS, VAL, SUCC, PRED, IMAGE, AND 
-- VALUE CANNOT BE FIXED OR FLOATING POINT TYPES.

-- RJW 2/26/86
-- PWN 11/30/94 REMOVED ATTRIBUTE TESTS ILLEGAL IN ADA 9X.
-- PWN 12/27/94 REMOVED ATTRIBUTE TESTS ILLEGAL IN ADA 9X.
-- PWN 01/31/95 REMOVED INCONSISTENCIES WITH ADA 9X.

PROCEDURE B35501B IS

     TYPE FIXED IS DELTA 1.0 RANGE 0.0 .. 2.0;
     FI : FIXED := 0.0;

     TYPE FLOAT IS DIGITS 3 RANGE 0.0 .. 2.0;
     FL : FLOAT := 0.0;

     I1 : INTEGER := 0;
     S1 : STRING (1 .. 5) := "1.0E0";
BEGIN
     
     I1 := FIXED'POS (FI);          -- ERROR: TYPE FIXED NOT ALLOWED.
     I1 := FLOAT'POS (FL);          -- ERROR: TYPE FLOAT NOT ALLOWED.
     
     FI := FIXED'VAL (I1);          -- ERROR: TYPE FIXED NOT ALLOWED.
     FL := FLOAT'VAL (I1);          -- ERROR: TYPE FLOAT NOT ALLOWED.
     
END B35501B;
