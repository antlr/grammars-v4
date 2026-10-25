-- BXE2A02.A
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
--      Check that a Declared Pure library unit can depend only on other 
--      Declared Pure library units.  Specifically it can not depend on a 
--      Remote Types unit.
--
-- TEST DESCRIPTION: 
--      A Declared Pure library unit is constructed which depends on another 
--      Declared Pure unit and on a Remote Types Unit. 
--      The dependency on the Remote Types unit should be flagged and 
--      that on the Declared Pure unit should be allowed.
--
-- TEST FILES:
--      This test depends on the following foundation code:
--
--         FXE2A00.A
--
-- PASS/FAIL CRITERIA:
--      The error related to the illegal dependency must be detected.
--      This may be reported at the point of the context clause.
--      Alternatively, it is permissible for the implementation to
--      reject the categorization pragma because of the context clause
--      that is marked as an error. 
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      01 Dec 95   SAIC    Fixed problems for ACVC 2.0.1.
--      01 DEC 97   EDS     Reformatted comment.
--!


--====================================================================

-- This is a Declared Pure package 
--
with FXE2A00_0;   -- declared pure

with FXE2A00_2;                                                      -- ERROR:
                              -- Illegal dependency on remote types package

package BXE2A02 is 

   pragma pure (BXE2A02);

   -- use something from each package we with just to make sure
   -- the compiler looked at those packages.

   subtype Is_OK is FXE2A00_0.Type_From_0;                              -- OK. 
                                                      -- from a pure package.

   subtype Not_So_Good is FXE2A00_2.Type_From_2;            -- OPTIONAL ERROR:
     -- reference to a package that cannot be referenced from a pure package.


end BXE2A02;
