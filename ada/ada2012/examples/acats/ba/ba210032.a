-- BA210032.A
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
--      Check that all compilation units of a preelaborated library unit
--      must depend semantically only on compilation units of other
--      preelaborated library units.
--
--      Check that all compilation units of a declared-pure library unit
--      must depend semantically only on compilation units of other library
--      units which are declared pure.
--
--      Check that a preelaborated unit may have a non-preelaborable child
--      unit, but not a non-preelaborable subunit.
--
-- TEST DESCRIPTION:
--
--         - - - - - - - - -
--        |     package     | - - - - - - - ---------------------
--             BA21003_0                   |                     |
--        |                 |              |             with Ada.Text_IO;
--              pragma            - - - - - - - - - -    =================
--        |  PREELABORATE   |    |  generic package  |  |     package     |
--         - - - - - - - - -          BAD_SUBUNIT       | SUBUNIT_BADWITH |
--                 |             |                   |  |                 |
--                 |               non-preelaborable    |  preelaborable  |
--       - - - - - - - - - - -   |       body        |  |      body       |
--      |    child package    |   - - - - - - - - - -    =================
--        BA21003_0.BA21003_1  
--      |                     |
--             no pragma       
--      |  non-preelaborable  |
--       - - - - - - - - - - -
--
--      Verify that an otherwise legal subunit (Subunit_BadWith) of BA21003_0
--      may not WITH a non-preelaborated unit (e.g., Ada.Text_IO).
--      
-- TEST FILES:
--      The following files comprise this test:
--
--         BA210030.A
--         BA210031.A
--      -> BA210032.A
--         BA210033.A
--         BA210034.A
--         BA210035.A
--
-- PASS/FAIL CRITERIA:
--      Each of files BA210031..5 contains errors. All errors in all these
--      files must be detected to pass the test.
--
-- CHANGE HISTORY:
--      10 Apr 95   SAIC    Initial prerelease version.
--      29 Jun 98   EDS     Renumbered to accommodate change to files 0 and 1
--!

with Ada.Text_IO;                                                     -- ERROR:
                                           -- WITHed unit is non-preelaborated.
separate (BA21003_0)
package body Subunit_BadWith is
   procedure Proc (P: in out My_Int) is
   begin
      Ada.Text_IO.Put_Line (My_Int'Image (P));
   end Proc;
end Subunit_BadWith;
