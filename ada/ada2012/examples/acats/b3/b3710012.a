-- B3710012.A
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
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE
--      See B3710010.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         B3710010.A
--         B3710011.A
--      -> B3710012.A
--         B3710013.A
--         B3710014.AM
--
-- PASS/FAIL CRITERIA:
--      See B3710010.A.
--
-- CHANGE HISTORY:
--    18 Jan 2001   PHL   Initial version
--    16 Mar 2001   RLB   Readied for release. Split files.
--    05 Jun 2001   RLB   Normalized "ERROR:" comments.
--    21 Mar 2007   RLB   Updated for Amendment 1 changes.
--    23 Mar 2007   RLB   Updated after discussion of legality of P4.
--    20 Aug 2007   RLB   Removed most of subtest pending resolution of
--                        AI05-0041.
--    28 Apr 2008   RLB   Replaced test cases, with new comments to reflect
--                        wording of AI05-0041.
--
--!

package B371001_1.Child_2 is
    pragma Elaborate_Body;

    type T is new B371001_1.T;  -- Derived from partial view
                                -- (full view is not visible).
    type Ptr1 is access T;
    subtype S1A is Ptr1 (1);    -- ERROR: Discriminant is not visible.

private
    subtype S1B is Ptr1 (2);    -- ERROR: Designated subtype is definite
                                --        and a general access type
                                --        (not known to be constrained).

    type Ptr2 is access constant T;
    subtype S2 is Ptr2 (3);     -- ERROR: Designated subtype is definite
                                --        and a general access type
                                --        (not known to be constrained).

    type Ptr3 is access T;
    P3 : Ptr3 (0);              -- ERROR: T has an ancestor with a constrained
                                --        partial view (not known to be
                                --        constrained). 3.4(23.9/3) thus does
                                --        not apply.

end B371001_1.Child_2;

