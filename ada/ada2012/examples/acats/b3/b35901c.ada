-- B35901C.ADA

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
-- CHECK THAT A FIXED POINT TYPE DEFINITION MUST BE REJECTED IF IT 
-- REQUIRES MORE THAN SYSTEM.MAX_MANTISSA BITS.
 
-- CHANGE HISTORY:
--      24 Feb 86   RJW     
--      21 Apr 21   RLB     Added error location indicators, moved tags.
 
WITH SYSTEM; USE SYSTEM;

PROCEDURE B35901C IS

     TYPE FIXED_TYPE1 IS DELTA 2.0 ** (-MAX_MANTISSA)
                        RANGE -1.0 .. 1.0;                  -- OK.    {1:6;1}

     TYPE FIXED_TYPE2 IS DELTA 2.0 ** (-(MAX_MANTISSA + 1)) 
                        RANGE -1.0 .. 1.0;                  -- ERROR: {1:6;1}
                                                            -- TOO 
                                                            -- MANY 
                                                            -- BITS.
     TYPE FIXED_TYPE3 IS DELTA 1.0 
               RANGE -10.0 .. 2.0 ** MAX_MANTISSA;          -- OK.    {1:6;1}

     TYPE FIXED_TYPE4 IS DELTA 1.0 
               RANGE -10.0 .. 2.0 ** MAX_MANTISSA + 1.0;    -- ERROR: {1:6;1}
                                                            -- TOO
                                                            -- MANY 
                                                            -- BITS

BEGIN
     NULL;
END B35901C;
