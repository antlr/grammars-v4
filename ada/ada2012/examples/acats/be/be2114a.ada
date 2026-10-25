-- BE2114A.ADA

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
-- CHECK THAT FILE_TYPE IS LIMITED.

-- SPS 9/28/82

WITH SEQUENTIAL_IO;
WITH DIRECT_IO;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE BE2114A IS

     FT1, FT2 : FILE_TYPE;

BEGIN
     FT1 := FT2;              -- ERROR: ASSIGNMENT OF LIMITED TYPE.

     DECLARE
          PACKAGE SEQ IS NEW SEQUENTIAL_IO (BOOLEAN);
          USE SEQ;
          F1, F2 : SEQ.FILE_TYPE;
     BEGIN
          IF F1 = F2 THEN     -- ERROR: EQUALITY OF LIMITED TYPE.
               NULL;
          END IF;
     END;

     DECLARE
          PACKAGE DIR IS NEW DIRECT_IO (CHARACTER);
          USE DIR;
          FILE1, FILE2 : DIR.FILE_TYPE;
     BEGIN
          FILE1 := FILE2 + 1; -- ERROR: ASSIGNMENT OF LIMITED TYPE.
     END;

END BE2114A;
