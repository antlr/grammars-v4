-- B54A25A.ADA

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
-- CHECK THAT OUT OF RANGE NON-NULL DERIVED ENUMERATION TYPE CHOICES 
-- ARE FORBIDDEN WHEN THE CASE EXPRESSION HAS A STATIC SUBTYPE.

-- DAT 1/29/81
-- SPS 8/23/82

PROCEDURE B54A25A IS

     TYPE DAY IS (MON, TUE, WED, THU, FRI, SAT, SUN);
     TYPE WEEKDAY IS NEW DAY RANGE MON .. FRI;
     X: WEEKDAY RANGE TUE .. THU;

BEGIN
     X := WED;

     CASE X IS 
          WHEN SAT => X := TUE;             -- ERROR: OUT OF RANGE, SAT.
          WHEN TUE .. WED => X := WED;      -- OK.
          WHEN THU | FRI => X := THU;       -- ERROR: OUT OF RANGE, FRI.
     END CASE;

     CASE X IS
          WHEN MON .. WED => NULL;          -- ERROR: OUT OF RANGE, MON.
          WHEN SAT .. SUN => NULL;          -- ERROR: OUT OF RANGE, SAT,
                                            --        OUT OF RANGE, SUN.
          WHEN THU .. FRI => NULL;          -- ERROR: OUT OF RANGE, FRI.
     END CASE;

END B54A25A;
