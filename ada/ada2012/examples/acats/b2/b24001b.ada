-- B24001B.ADA

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
--     A. BE CONSECUTIVE WITHIN A FLOATING POINT LITERAL,
--     B. LEAD OR TRAIL IN A FLOATING POINT LITERAL,
--     C. BE ADJACENT TO THE . E + - # CHARACTERS IN A FLOATING
--        POINT LITERAL.

-- DCB 04/22/80
-- JRK 10/27/80
-- TBN 10/16/85     RENAMED FROM B24001B.ADA; ADDED TRAILING # CASES.

PROCEDURE B24001B IS

     F1, F2 : FLOAT;

BEGIN

     F1 := 1__2.3;      -- ERROR: CONSECUTIVE UNDERSCORES IN A
                        --        FLOATING POINT LITERAL
     NULL;
     F1 := 1.2__3;      -- ERROR: CONSECUTIVE UNDERSCORES IN A
                        --        FLOATING POINT LITERAL
     NULL;
     F1 := 1.2E0__1;    -- ERROR: CONSECUTIVE UNDERSCORES IN A
                        --        FLOATING POINT LITERAL
     NULL;
     F1 := 16#1__2.A#;  -- ERROR: CONSECTUIVE UNDERSCORES IN A
                        --        FLOATING POINT LITERAL
     NULL;
     F1 := _1.2;        -- ERROR: UNDERSCORE LEADS A FLOATING POINT
                        --        LITERAL
     NULL;
     F1 := 1.2_;        -- ERROR: UNDERSCORE TRAILS A FLOATING POINT
                        --        LITERAL
     NULL;
     F1 := 1.2E2_;      -- ERROR: UNDERSCORE TRAILS A FLOATING POINT
                        --        LITERAL
     NULL;
     F1 := 12_.1;       -- ERROR: UNDERSCORE ADJACENT TO .
     NULL;
     F1 := 12._1;       -- ERROR: UNDERSCORE ADJACENT TO .
     NULL;
     F1 := 2.0_E3;      -- ERROR: UNDERSCORE ADJACENT TO E
     NULL;
     F2 := 2.0E_3;      -- ERROR: UNDERSCORE ADJACENT TO E
     NULL;
     F1 := 2.0E+_3;     -- ERROR: UNDERSCORE ADJACENT TO +
     NULL;
     F2 := 2.0E_+3;     -- ERROR: UNDERSCORE ADJACENT TO +
     NULL;
     F1 := 2.0E-_3;     -- ERROR: UNDERSCORE ADJACENT TO -
     NULL;
     F2 := 2.0E_-3;     -- ERROR: UNDERSCORE ADJACENT TO -
     NULL;
     F2 := 16_#2.A#;    -- ERROR: UNDERSCORE ADJACENT TO #
     NULL;
     F2 := 16#_2.A#;    -- ERROR: UNDERSCORE ADJACENT TO #
     NULL;
     F2 := 16#2.A_#;    -- ERROR: UNDERSCORE ADJACENT TO #
     NULL;
     F2 := 16#2.A#_;    -- ERROR: UNDERSCORE ADJACENT TO #
     NULL;

END B24001B;
