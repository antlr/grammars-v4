-- B23002A.ADA

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
-- CHECK THAT CONSECUTIVE, LEADING, AND/OR TRAILING UNDERSCORES
--  ARE NOT ALLOWED IN IDENTIFIERS.

-- DCB 12/19/79
-- JWC 6/28/85   RENAMED TO -AB

PROCEDURE B23002A IS

     I_1  : INTEGER;
     I__2 : INTEGER;   -- ERROR: CONSECUTIVE UNDERSCORES.
     I3   : INTEGER;
     _I4  : INTEGER;   -- ERROR: LEADING UNDERSCORE.
     I5   : INTEGER;
     I6_  : INTEGER;   -- ERROR: TRAILING UNDERSCORE.

BEGIN

     I_1  := 1;
     I__2 := 2;    -- ERROR: CONSECUTIVE UNDERSCORES.
     I3   := 3;
     _I4  := 4;    -- ERROR: LEADING UNDERSCORE.
     I5   := 5;
     I6_  := 6;    -- ERROR: TRAILING UNDERSCORE.

END B23002A;
