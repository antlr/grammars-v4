-- BC3002D.ADA

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
-- CHECK THAT IN A GENERIC INSTANTIATION, A POSITIONAL OR NAMED GENERIC
-- ACTUAL PARAMETER AND ANOTHER NAMED GENERIC ACTUAL PARAMETER CANNOT
-- BE SPECIFIED FOR THE SAME GENERIC FORMAL PARAMETER.

-- ASL 8/14/81
-- SPS 4/26/82

PROCEDURE BC3002D IS

     GENERIC
          GFP1 : INTEGER := 1;
          GFP2 : INTEGER := 2;
          GFP3 : INTEGER := 3;
     PACKAGE P IS
     END P;

     PACKAGE BAD1 IS 
          NEW P(1,
                GFP1 => 1,                  -- ERROR: DUPL. ASSOC.
                GFP3 => 3);
     PACKAGE BAD2 IS
          NEW P(GFP1 => 1,
                GFP2 => 2,
                GFP2 => 2);                  -- ERROR: DUPL. ASSOC.
     PACKAGE BAD3 IS NEW P(1,2,GFP2 => 3);   -- ERROR: DUPL. ASSOC.
BEGIN
     NULL;
END BC3002D;
