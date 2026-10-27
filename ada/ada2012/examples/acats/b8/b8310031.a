-- B8310031
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
--      See B8310030.A.
--
-- TEST DESCRIPTION
--      See B8310030.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         B8310030.A
--      -> B8310031.A
--         B8310032.A
--         B8310033.A
--
-- PASS/FAIL CRITERIA:
--      See B8310030.A.
--
-- CHANGE HISTORY:
--      22 Aug 2007   RLB   Created test from submitted test. Added child
--                          private part cases. Added interesting not
--                          overriding cases.
--
--!
package B831003.Child3 is

    package Nested is

        type T is limited private;

        overriding
        function "=" (X, Y : T) return Boolean; -- ERROR:

        overriding
        function "or" (X, Y : T) return T; -- ERROR:

        type A is array (Integer range <>) of T;

        overriding
        function "=" (X, Y : A) return Boolean; -- ERROR:

        overriding
        function "or" (X, Y : A) return A; -- ERROR:

    private
        type T is new B831003.T;
    end Nested;

    type TT is limited private;

    overriding
    function "=" (X, Y : TT) return Boolean; -- ERROR:

    overriding
    function "or" (X, Y : TT) return TT; -- ERROR:

    not overriding
    function "not" (X : TT) return TT; -- ERROR:

    -- Note that for the above three cases, the other indicator also
    -- is illegal (because these are all overridden below).

private
    type TT is new B831003.T; -- We find out about all of the Boolean
                              -- operations here.

    overriding
    function "and" (X, Y : TT) return TT; -- OK

    not overriding
    function "xor" (X, Y : TT) return TT; -- ERROR:

end B831003.Child3;
