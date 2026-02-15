-- B53009C.ADA

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
-- CHECK THAT DUPLICATE LABELS ARE NOT ALLOWED IN DIFFERENT ALTERNATIVES
-- OF AN IF STATEMENT.

-- DCB 03/17/80
-- SPS 3/4/83
-- RJK 9/13/83    SPLIT FROM B53009A

PROCEDURE B53009C IS

     I1, I2, I3 : INTEGER;
     B1, B2 : BOOLEAN;
     L1 : CHARACTER;

BEGIN
     B1 := TRUE; B2 := FALSE;
     I1 := 5; I2 := 9; I3 := 0;

     IF B1 THEN
          I3 := 9;
     <<L2>>
          I2 := 8;
     ELSE
          I3 := 8;
     <<L2>>                -- ERROR: LABEL DUPLICATES LABEL IN OTHER
                           --     ALTERNATIVE.
          I2 := 9;
     END IF;

END B53009C;
