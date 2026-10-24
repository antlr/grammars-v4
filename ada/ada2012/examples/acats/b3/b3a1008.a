-- B3A1008.A
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
--    Check that a dereference of an access-to-incomplete type cannot be used
--    as a prefix.
--
-- TEST DESCRIPTION:
--    We declare some incomplete types and access types that designate them,
--    tjhen test whether dereferences of the access types are allowed.
--
-- CHANGE HISTORY:
--    28 MAY 2004   PHL    Initial version.
--    13 JAN 2015   RLB    Renamed, readied for release, added additional
--                         cases.
--    13 MAR 2015   RLB    Eliminated overlong lines and tab characters.
--
--!
package B3A1008 is

    type T1 (<>) is tagged;
    type T2 (D : Integer := 3);

    type A1 is access all T1;
    type A2 is access constant T2;

    X1 : A1;
    X2 : A2;

    D1 : constant Integer := X2.D;                          -- ERROR:
    D2 : constant Integer := X2.all.D;                      -- ERROR:
    S : constant Integer := X1.all'Size;                    -- ERROR:

    package Nested is
        procedure P (X : T1 := X1.all);                     -- OK.
        procedure P (X : Integer := X2.D);                  -- ERROR:
        procedure Q (X : Integer := X1.all'Alignment);      -- ERROR:
        procedure R (X : Integer := X2.D'First_Bit);        -- ERROR:
    end Nested;

    type T1 is tagged null record;
    type T2 (D : Integer := 3) is null record;

end B3A1008;
