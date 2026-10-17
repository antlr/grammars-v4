-- B7310016.AM
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE
--      See B7310010.A.
--
-- TEST DESCRIPTION
--      See B7310010.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         B7310010.A
--         B7310011.A
--         B7310012.A
--         B7310013.A
--         B7310014.A
--      -> B7310015.AM
--
-- PASS/FAIL CRITERIA:
--      See B7310010.A.
--
-- CHANGE HISTORY:
--      29 JUN 1999   RAD   Initial Version.
--      16 DEC 1999   RLB   Revised to insure that units don't depend on other
--                          units containing errors.
--!

-- Main subprogram. Units that contained errors may not be available here.

with B7310016_1.Unrelated;
with B7310016_1.Parent;
with B7310016_1.Parent.Child;
with B7310016_1.Parent.Child_2; -- OPTIONAL ERROR:
with B7310016_1.Places.P;
with B7310016_1.Places.P.Q;
with B7310016_1.Places.P.R; -- OPTIONAL ERROR:

procedure B7310016 is
begin
    null;
end B7310016;
