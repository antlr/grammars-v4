-- BDE0010.A
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
--    The ACAA has created and maintains the Ada Conformity Assessment Test
--    Suite for the purpose of conformity assessments conducted in accordance
--    with the International Standard ISO/IEC 18009 - Ada: Conformity
--    assessment of a language processor. This test suite should not be used
--    to make claims of conformance unless used in accordance with
--    ISO/IEC 18009 and any applicable ACAA procedures.
--
--*
--
-- OBJECTIVE:
--    Check that an object renaming causes freezing.
--
--    Check that implicit_dereferences cause freezing in a construct that is
--    being frozen. Check that the nominal subtype of the implicit dereference
--    is frozen.
--
--    Check that implicit conversions cause freezing.
--
--    These objectives come from Defect Report 8652/0046, as reflected in
--    Technical Corrigendum 1).
--
-- CHANGE HISTORY:
--    12 FEB 2001   PHL   Initial version.
--    14 MAR 2003   RLB   Readied for issuing.
--
--!
package BDE0010 is

    type T (D : Integer) is private;

    type A is access T;
    Obj : A;

    X : Integer renames Obj.all.D; -- ERROR: Cannot freeze partial view.

    V : Integer := Obj.D; -- ERROR: Cannot freeze partial view.
    C : constant Integer;

    type Rec (D : Integer) is null record;

    type Acc is access Rec;
    Ptr : Acc;

    N : Integer := Ptr.D; -- OK (freezes Rec).

    for Rec'Alignment use Integer'Alignment; -- ERROR: Rec is frozen.
          -- Note: The test passes here if this alignment is not supported, as
          -- an error indication will be given. Thus this test is applicable to
          -- all implementations. The same is true for the size clause below.

    type Flubber is range 1..10;
    function Is_Flipper (X: Flubber) return Boolean;
    Flop : Boolean := Is_Flipper (10); -- OK (Freezes type Flubber)
    for Flubber'Size use Integer'Size; -- ERROR: Flubber is frozen.

private
    type T (D : Integer) is null record;

    Y : Integer renames Obj.D; -- OK

    C : constant Integer := Obj.D; -- OK

end BDE0010;

