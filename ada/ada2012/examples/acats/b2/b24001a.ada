-- B24001A.ADA

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
-- CHECK THAT UNDERSCORES ARE NOT PERMITTED TO:
--     A. BE CONSECUTIVE WITHIN AN INTEGER LITERAL,
--     B. LEAD OR TRAIL IN AN INTEGER LITERAL,
--     C. BE ADJACENT TO THE E + - # CHARACTERS IN AN INTEGER
--        LITERAL.

-- DCB 1/24/80
-- JRK 10/27/80
-- TBN 10/16/85     RENAMED FROM B24001A.ADA; ADDED TRAILING # CASES.

PROCEDURE B24001A IS

     I1, I2, I3 : INTEGER;

BEGIN

     I1 := 12__3;       -- ERROR: CONSECUTIVE UNDERSCORES WITHIN AN
                        --        INTEGER LITERAL
     NULL;
     I2 := 12E0__1;     -- ERROR: CONSECUTIVE UNDERSCORES WITHIN AN
                        --        INTEGER LITERAL
     NULL;
     I2 := 16#1__A#;    -- ERROR: CONSECUTIVE UNDERSCORES WITHIN AN
                        --        INTEGER LITERAL
     NULL;
     I2 := _12;         -- ERROR: UNDERSCORE LEADS AN INTEGER LITERAL
     NULL;
     I3 := 12_;         -- ERROR: UNDERSCORE TRAILS AN INTEGER LITERAL
     NULL;
     I3 := 12E1_;       -- ERROR: UNDERSCORE TRAILS AN INTEGER LITERAL
     NULL;
     I2 := 2_E1;        -- ERROR: UNDERSCORE ADJACENT TO E
     NULL;
     I2 := 2E_1;        -- ERROR: UNDERSCORE ADJACENT TO E
     NULL;
     I2 := 2E+_1;       -- ERROR: UNDERSCORE ADJACENT TO +
     NULL;
     I2 := 2E_+1;       -- ERROR: UNDERSCORE ADJACENT TO +
     NULL;
     I2 := 2E-_1;       -- ERROR: UNDERSCORE ADJACENT TO -
     NULL;
     I2 := 2E_-1;       -- ERROR: UNDERSCORE ADJACENT TO -
     NULL;
     I1 := 16_#D#;      -- ERROR: UNDERSCORE ADJACENT TO #
     NULL;
     I2 := 16#_D#;      -- ERROR: UNDERSCORE ADJACENT TO #
     NULL;
     I2 := 16#D_#;      -- ERROR: UNDERSCORE ADJACENT TO #
     NULL;
     I2 := 16#D#_;      -- ERROR: UNDERSCORE ADJACENT TO #
     NULL;

END B24001A;
