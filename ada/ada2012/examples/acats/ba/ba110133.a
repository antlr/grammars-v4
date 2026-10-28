-- BA110133.A
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
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
--
-- OBJECTIVE
--      See BA110130.A.
--
-- TEST DESCRIPTION
--      See BA110130.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         BA110130.A
--         BA110131.A
--         BA110132.A
--      -> BA110133.A
--         BA110134.AM
--
-- PASS/FAIL CRITERIA:
--      See BA110130.A.
--
-- CHANGE HISTORY:
--      21 Mar 2007   RLB   Created test from submitted test.
--      17 Aug 2007   RLB   Removed unintentionally duplicated unit.
--      13 Feb 2018   RLB   Added error location indicators.
--
--!

with BA11013A.Child3;
private separate (BA11013A.Child1) -- ERROR: {1}
                                   -- Subunit cannot be declared private.
procedure Stub is
begin
    null;
end Stub;

