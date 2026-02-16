-- BXE2A05.A
--
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
--
-- OBJECTIVE: 
--      Check that a Shared Passive library unit can depend only on other 
--      Shared Passive library units or Declared Pure library units. 
--      Specifically that it can not depend on a Normal unrestricted unit.
--
-- TEST DESCRIPTION: 
--      A Shared Passive library unit is constructed which depends on each of
--      the above.  The dependencies on the unrestricted unit should be
--      flagged and the others should be allowed.
--
-- TEST FILES:
--      This test depends on the following foundation code:
--
--         FXE2A00.A
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!


--====================================================================

-- This is a Shared Passive package 
--
with FXE2A00_0;   -- declared pure  - OK.
with FXE2A00_1;   -- shared_passive - OK.
with FXE2A00_4;   -- normal         -- Illegal dependency 

package BXE2A05 is 

   pragma Shared_Passive (BXE2A05);                                  -- ERROR:
                                                      -- Illegal dependency on
                                                      -- package (FXE2A00_4)

   Color_Set_0 : FXE2A00_0.Type_From_0;    -- OK: declared pure
   Color_Set_1 : FXE2A00_1.Type_From_1;    -- OK: shared passive
   Color_Set_4 : FXE2A00_4.Type_From_4;    -- Illegal: normal

end BXE2A05;
