-- BC1014B.ADA

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
-- CHECK THAT THE IDENTIFIER OF A GENERIC SUBPROGRAM
--   IS NOT OVERLOADABLE.

-- CASE B: DIFFERENT NUMBERS OF PARAMETERS.

-- PWB  1/24/86

PROCEDURE BC1014B IS

     GENERIC
          T : INTEGER;
     PROCEDURE CHECK (X : STRING);

     GENERIC
     PROCEDURE CHECK (X : STRING; Y : INTEGER);     -- ERROR: OVERLOAD.

     PROCEDURE CHECK (X : STRING) IS
     BEGIN
          NULL;
     END CHECK;

     PROCEDURE CHECK (X : STRING; Y : INTEGER) IS   -- OPTIONAL ERROR.
     BEGIN
          NULL;
     END CHECK;

BEGIN  -- BC1014B
     NULL;
END BC1014B;
