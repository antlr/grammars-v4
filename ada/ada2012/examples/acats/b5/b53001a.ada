-- B53001A.ADA

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
-- CHECK THAT AN IF STATEMENT MUST END WITH "END IF;".

-- DCB 3/6/80
-- JRK 7/7/80
-- ABW 6/11/82

PROCEDURE B53001A IS

     B1, B2 : BOOLEAN;
     I1, I2 : INTEGER;

BEGIN
     B1 := TRUE;
     B2 := FALSE;

     IF B1 AND B2 THEN
          I2 := 1;
                        -- "END IF;" MISSING.
     I1 := 0;

     IF B1 AND B2 THEN
          I1 := 2;
     ELSE IF B1 OR B2 THEN
          I1 := 3;
     ELSE I1 := 4;
                        -- "END IF;" MISSING.
                        -- "END IF;" MISSING.

     IF B1 OR B2 THEN
          I1 := 1;
     END;               -- ERROR: 3 PREVIOUS "END IF;"S MISSING;
                        --        "END;" USED INSTEAD OF "END IF;".

END B53001A;
