-- B7300062.A
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
--      See B7300060.A.
--
-- TEST DESCRIPTION:
--      See B7300060.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         B7300060.A
--         B7300061.A
--      -> B7300062.A
--         B7300063.AM
--
-- PASS/FAIL CRITERIA:
--      See B7300060.A.
--
-- CHANGE HISTORY:
--      14 Sep 99   RLB     Created test.
--      02 Feb 17   RLB     Fixed case of error tags, add location indicators.
--!

package body B730006_0.Child3 is
   procedure Pound (Claw : in out Hammer) is
   begin
      Claw.D := 1.0;                 -- OK. {7;8)
      if Claw.B then                 -- OK. {10;5}
         null;
      end if;
      if Claw.I = 2 then             -- ERROR: Component not visible. {10;9}
         null;
      end if;
      Claw.C := 'A';                 -- ERROR: Component not visible. {7;8}
      B730006_0.Root(Claw).I := 3;   -- OK. {7;6}  (Root's components are
      B730006_0.Root(Claw).C := 'A'; -- OK. {7;8}   visible here.)
   end Pound;
end B730006_0.Child3;

