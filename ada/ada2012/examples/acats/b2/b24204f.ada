-- B24204F.ADA

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
--     CHECK THAT THE DIGITS AND EXTENDED_DIGITS OF A FIXED POINT
--     LITERAL ARE WITHIN THE CORRECT RANGE FOR THE NUMBER'S BASE.
--     THIS TEST CHECKS LITERALS WITH BASES OTHER THAN 10.

-- HISTORY:
--     DWC 09/22/87  CREATED ORIGINAL TEST FROM SPLIT OF B24204C.ADA.

PROCEDURE B24204F IS

        TYPE FIXED IS DELTA 0.1 RANGE 0.0 .. 100.0;

        F : FIXED;

BEGIN

        F := 7#7.6#;            -- ERROR: 7#7.6#
        F := 9#8.9#;            -- ERROR: 9#8.9#

END B24204F;
