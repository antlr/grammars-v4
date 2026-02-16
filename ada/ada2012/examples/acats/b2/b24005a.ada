-- B24005A.ADA

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
-- CHECK THAT LEADING/TRAILING DECIMAL POINTS ARE NOT PERMITTED IN
-- FLOATING POINT LITERALS.

-- JRK 12/12/79
-- JRK 10/27/80
-- JWC 6/28/85   RENAMED TO -AB

PROCEDURE B24005A IS

        FL : FLOAT;

BEGIN

        FL := 35.;      -- ERROR: 35.
        NULL;
        FL := .68;      -- ERROR: .68
        NULL;
        FL := 71.E1;    -- ERROR: 71.E1
        NULL;
        FL := .95E1;    -- ERROR: .95E1
        NULL;
        FL := 16#.A#;   -- ERROR: 16#.A#
        NULL;
        FL := 16#A.#;   -- ERROR: 16#A.#
        NULL;

END B24005A;
