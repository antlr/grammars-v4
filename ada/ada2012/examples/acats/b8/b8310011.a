-- B8310011
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
--      See B8310010.A.
--
-- TEST DESCRIPTION
--      See B8310010.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         B8310010.A
--      -> B8310011.A
--         B8310012.A
--
-- PASS/FAIL CRITERIA:
--      See B8310010.A.
--
-- CHANGE HISTORY:
--      22 Aug 2007   RLB   Created test from submitted test. Added null
--                          procedure cases. Added missing body cases.
--      07 Sep 2007   RLB   Removed functions that are illegal for other
--                          reasons.
--      28 Apr 2008   RLB   Made test changes to compensate for correction
--                          to generic function Gft.
--      01 Oct 2008   RLB   Corrected subtest that depended on a previously
--                          incorrect declaration.
--!
with B831001_0;
package B831001_1 is

    -- Vanilla untagged type.
    package P1 is

        type T is new Integer;

        not overriding
        procedure P (X : T); -- OK

        not overriding
        procedure Q (Z : T) renames P; -- OK

        not overriding
        function F return T; -- OK

        not overriding
        function G return T renames F; -- OK

        overriding
        function "-" (Left : T) return T; -- OK

        package Nested is

            not overriding
            procedure P (X : T); -- ERROR:

            not overriding
            procedure Q (Z : T) renames P1.P; -- ERROR:

            not overriding
            function F return T; -- ERROR:

            not overriding
            function G return T renames P1.F; -- ERROR:

            not overriding
            function "*" (Left, Right : T) return T; -- ERROR:

            procedure P (X : T; Y : in out Boolean); -- OK

            function F (Y : Boolean) return T; -- OK

        end Nested;

        not overriding
        procedure Q (X : T; Y : in out Boolean) renames Nested.P; -- OK

        not overriding
        function G (Y : Boolean) return T renames Nested.F; -- OK

    end P1;

    -- Vanilla tagged type.
    package P2 is

        type T is abstract tagged null record;

        not overriding
        procedure P (X : T); -- OK

        not overriding
        procedure P (X : T'Class); -- ERROR:

        procedure P (X : T'Class; Y : in out Duration); -- OK

        not overriding
        procedure P (X : T; Y : out Integer) is abstract; -- OK

        not overriding
        procedure Q (Z : T) renames P; -- OK

        not overriding
        procedure Q (Z : T'Class; Y : in out Duration) renames P; -- ERROR:

        not overriding
        function F return T'Class; -- ERROR:

        function F (Y : Duration) return T'Class; -- OK

        not overriding
        function F (Y : Integer) return T is abstract; -- OK

        not overriding
        function G (Y : in Duration) return T'Class renames F; -- ERROR:

        not overriding
        function "+" (Left : T) return T is abstract; -- OK

        package Nested is

            not overriding
            procedure P (X : T); -- ERROR:

            not overriding
            procedure P (X : T; Y : out Integer) is abstract; -- ERROR:

            not overriding
            procedure P (X : T'Class); -- ERROR:

            not overriding
            procedure Q (Z : T) renames P2.P; -- ERROR:

            not overriding
            procedure Q (Z : T'Class; Y : in out Duration) renames P2.P; -- ERROR:

            not overriding
            function F return T'Class; -- ERROR:

            not overriding
            function F (Y : Integer) return T is abstract; -- ERROR:

            not overriding
            function G (Y : in Duration) return T'Class renames P2.F; -- ERROR:

            not overriding
            function "-" (Left : T) return T is abstract; -- ERROR:

            procedure P (X : T; Y : in out Boolean); -- OK

        end Nested;

        not overriding
        procedure Q (X : T; Y : in out Boolean) renames Nested.P; -- OK

    end P2;

    -- Visibly tagged private type.
    package P3 is

        type T is tagged private;

        not overriding
        procedure P (X : T); -- OK

        not overriding
        procedure P (X : T'Class); -- ERROR:

        procedure P (X : T'Class; Y : in out Duration); -- OK

        not overriding
        procedure Q (Z : T) renames P; -- OK

        not overriding
        procedure Q (Z : T'Class; Y : in out Duration) renames P; -- ERROR:

        not overriding
        function F return T; -- OK

        not overriding
        function F return T'Class; -- ERROR:

        function F (Y : Duration) return T'Class; -- OK

        not overriding
        function G return T renames F; -- OK

        not overriding
        function G (Y : in Duration) return T'Class renames F; -- ERROR:

        package Nested is

            not overriding
            procedure P (X : T); -- ERROR:

            not overriding
            procedure P (X : T'Class); -- ERROR:

            not overriding
            procedure Q (Z : T) renames P3.P; -- ERROR:

            not overriding
            procedure Q (Z : T'Class; Y : in out Duration) renames P3.P; -- ERROR:

            not overriding
            function F return T; -- ERROR:

            not overriding
            function F return T'Class; -- ERROR:

            not overriding
            function G return T renames P3.F; -- ERROR:

            not overriding
            function G (Y : in Duration) return T'Class renames P3.F; -- ERROR:

            procedure P (X : T; Y : in out Boolean); -- OK

            function F (Y : Boolean) return T; -- OK

        end Nested;

        not overriding
        procedure Q (X : T; Y : in out Boolean) renames Nested.P; -- OK

        not overriding
        function G (Y : Boolean) return T renames Nested.F; -- OK

    private
        type T is tagged null record;

        not overriding
        procedure P2 (X : T); -- OK

        not overriding
        procedure P2 (X : T'Class); -- ERROR:

        procedure P2 (X : T'Class; Y : in out Duration); -- OK

        not overriding
        procedure Q2 (Z : T) renames P2; -- OK

        not overriding
        procedure Q2 (Z : T'Class; Y : in out Duration) renames P2; -- ERROR:

        not overriding
        function F2 return T'Class; -- ERROR:

        function F2 (Y : Duration) return T'Class; -- OK

        not overriding
        function G2 (Y : in Duration) return T'Class renames F2; -- ERROR:

        package Nested2 is

            not overriding
            procedure P2 (X : T); -- ERROR:

            not overriding
            procedure P2 (X : T'Class); -- ERROR:

            not overriding
            procedure Q2 (Z : T) renames P3.P2; -- ERROR:

            not overriding
            procedure Q2 (Z : T'Class; Y : in out Duration)
               renames P3.P2; -- ERROR:

            not overriding
            function F2 return T; -- ERROR:

            not overriding
            function F2 return T'Class; -- ERROR:

            not overriding
            function G2 (Y : in Duration) return T'Class renames P3.F2; -- ERROR:

            procedure P2 (X : T; Y : in out Boolean); -- OK

        end Nested2;

        not overriding
        procedure Q2 (X : T; Y : in out Boolean) renames Nested2.P2; -- OK

    end P3;

    -- Not visibly tagged private type.
    package P4 is

        type T is private;

        not overriding
        procedure P (X : T); -- OK

        not overriding
        procedure Q (Z : T) renames P; -- OK

        not overriding
        function F return T; -- OK

        not overriding
        function G return T renames F; -- OK

        package Nested is

            not overriding
            procedure P (X : T); -- ERROR:

            not overriding
            procedure Q (Z : T) renames P4.P; -- ERROR:

            not overriding
            function F return T; -- ERROR:

            not overriding
            function G return T renames P4.F; -- ERROR:

            procedure P (X : T; Y : in out Boolean); -- OK

            function F (Y : Boolean) return T; -- OK

        end Nested;

        not overriding
        procedure Q (X : T; Y : in out Boolean) renames Nested.P; -- OK

        not overriding
        function G (Y : Boolean) return T renames Nested.F; -- OK

    private
        type T is tagged null record;

        not overriding
        procedure P2 (X : T); -- OK

        not overriding
        procedure P2 (X : T'Class); -- ERROR:

        procedure P2 (X : T'Class; Y : in out Duration); -- OK

        not overriding
        procedure Q2 (Z : T) renames P2; -- OK

        not overriding
        procedure Q2 (Z : T'Class; Y : in out Duration) renames P2; -- ERROR:

        -- We can't test functions returning T, because they'd be illegal
        -- by 3.9.3(10/2).

        not overriding
        function F2 return T'Class; -- ERROR:

        function F4 (Obj : T) return T'Class; -- OK

        function F2 (Y : Duration) return T'Class; -- OK

        not overriding
        function G2 (Y : in Duration) return T'Class renames F2; -- ERROR:

        package Nested2 is

            not overriding
            procedure P2 (X : T); -- ERROR:

            not overriding
            procedure P2 (X : T'Class); -- ERROR:

            not overriding
            procedure Q2 (Z : T) renames P4.P2; -- ERROR:

            not overriding
            procedure Q2 (Z : T'Class; Y : in out Duration)
               renames P4.P2; -- ERROR:

            not overriding
            function F2 return T; -- ERROR:

            not overriding
            function F2 return T'Class; -- ERROR:

            not overriding
            function G2 (Obj : T) return T'Class renames P4.F4; -- ERROR:

            not overriding
            function G2 (Y : in Duration) return T'Class renames P4.F2; -- ERROR:

            procedure P2 (X : T; Y : in out Boolean); -- OK

            function F2 (Y : Boolean) return T; -- OK

        end Nested2;

        not overriding
        procedure Q2 (X : T; Y : in out Boolean) renames Nested2.P2; -- OK

    end P4;

    -- Vanilla incomplete (tagged) type.
    package P5 is

        type T is tagged;

        not overriding
        procedure P (X : T); -- OK

        not overriding
        procedure P (X : T'Class); -- ERROR:

        procedure P (X : T'Class; Y : in out Duration); -- OK

        not overriding
        procedure Q (Z : T) renames P; -- OK

        not overriding
        procedure Q (Z : T'Class; Y : in out Duration) renames P; -- ERROR:

        package Nested is

            not overriding
            procedure P (X : T); -- ERROR:

            not overriding
            procedure P (X : T'Class); -- ERROR:

            not overriding
            procedure Q (Z : T) renames P5.P; -- ERROR:

            not overriding
            procedure Q (Z : T'Class; Y : in out Duration) renames P5.P; -- ERROR:

            procedure P (X : T; Y : in out Boolean); -- OK

        end Nested;

        not overriding
        procedure Q (X : T; Y : in out Boolean) renames Nested.P; -- OK


        type T is tagged null record;

        not overriding
        procedure P2 (X : T); -- OK

        not overriding
        procedure P2 (X : T'Class); -- ERROR:

        procedure P2 (X : T'Class; Y : in out Duration); -- OK

        not overriding
        procedure Q2 (Z : T) renames P2; -- OK

        not overriding
        procedure Q2 (Z : T'Class; Y : in out Duration) renames P2; -- ERROR:

        not overriding
        function F2 return T; -- OK

        not overriding
        function F2 return T'Class; -- ERROR:

        function F2 (Y : Duration) return T'Class; -- OK

        not overriding
        function G2 return T renames F2; -- OK

        not overriding
        function G2 (Y : in Duration) return T'Class renames F2; -- ERROR:

        package Nested2 is

            not overriding
            procedure P2 (X : T); -- ERROR:

            not overriding
            procedure P2 (X : T'Class); -- ERROR:

            not overriding
            procedure Q2 (Z : T) renames P5.P2; -- ERROR:

            not overriding
            procedure Q2 (Z : T'Class; Y : in out Duration)
               renames P5.P2; -- ERROR:

            not overriding
            function F2 return T; -- ERROR:

            not overriding
            function F2 return T'Class; -- ERROR:

            function F2 (Y : Duration) return T'Class; -- OK

            not overriding
            function G2 return T renames P5.F2; -- ERROR:

            not overriding
            function G2 (Y : in Duration) return T'Class renames P5.F2; -- ERROR:

            procedure P2 (X : T; Y : in out Boolean); -- OK

            function F2 (Y : Boolean) return T; -- OK

        end Nested2;

        not overriding
        procedure Q2 (X : T; Y : in out Boolean) renames Nested2.P2; -- OK

        not overriding
        function G2 (Y : Boolean) return T renames Nested2.F2; -- OK

    end P5;

    -- Taft-amendment incomplete (tagged) type.
    package P6 is
    private

        type T is tagged;

        not overriding
        procedure P (X : T'Class); -- ERROR:

        procedure P (X : T'Class; Y : in out Duration); -- OK

        not overriding
        procedure Q (Z : T'Class; Y : in out Duration) renames P; -- ERROR:

        package Nested is

            not overriding
            procedure P (X : T); -- ERROR:

            not overriding
            procedure P (X : T'Class); -- ERROR:

            not overriding
            procedure Q (Z : T'Class; Y : in out Duration) renames P6.P; -- ERROR:

        end Nested;

    end P6;

    -- Derived untagged type.
    package P7 is

        type T is new B831001_0.Pu.T;

        overriding
        procedure P (X : T); -- OK

        not overriding
        procedure P (X : T; Y : out Boolean); -- OK

        overriding
        function F return T; -- OK

        overriding
        function F (Y : Duration) return Integer; -- ERROR:

        overriding
        function "/" (Left, Right : T) return T; -- OK

        package Nested is

            overriding
            procedure P (X : T); -- ERROR:

            not overriding
            procedure Q (Z : T) renames P; -- ERROR:

            not overriding
            function F return T; -- ERROR:

            not overriding
            function G return T renames F; -- ERROR:

            not overriding
            function "+" (Left, Right : T) return T; -- ERROR:

            procedure P (X : T; Y : in out Boolean); -- OK

            function F (Y : Boolean) return T; -- OK

        end Nested;

        overriding
        procedure Q (X : T; Y : in out Boolean) renames Nested.P; -- OK

        overriding
        function G (Y : Boolean) return T renames Nested.F; -- OK

    end P7;

    -- Vanilla type extension.
    package P8 is

        type T is abstract new B831001_0.Pt.T with null record;

        overriding
        procedure P (X : T); -- OK

        not overriding
        procedure P (X : T; Y : out Boolean); -- OK

        overriding
        procedure Z (X : T'Class); -- ERROR:

        not overriding
        procedure Z (Z : T) renames P; -- OK

        overriding
        procedure Q (X : in T; A : in Integer) is null; -- OK

        package Nested is

            overriding
            procedure P (X : T); -- ERROR:

            not overriding
            procedure P (X : T'Class); -- ERROR:

            not overriding
            procedure Q (Z : T) renames P; -- ERROR:

            procedure P (X : T; Y : in out Boolean); -- OK

        end Nested;

        overriding
        procedure R (X : T; Y : in out Boolean) renames Nested.P; -- OK

    end P8;

    -- Private extension.
    package P9 is

        type T is new B831001_0.Pt.T with private;

        overriding
        procedure P (X : T); -- OK

        not overriding
        procedure P (X : T; Y : out Boolean); -- OK

        overriding
        procedure P (X : T'Class; Y : out Boolean); -- ERROR:

        overriding
        procedure Q (X : in T; A : in Integer) is null; -- OK

        overriding
        function F return T; -- OK

        overriding
        function G return T renames F; -- OK

        not overriding
        function F (Y : Float) return T'Class; -- ERROR:

        overriding
        function "-" (X : T) return T; -- OK

        package Nested is

            overriding
            procedure P (X : T); -- ERROR:

            not overriding
            procedure P (X : T'Class); -- ERROR:

            not overriding
            procedure Q (Z : T) renames P; -- ERROR:

            not overriding
            function F return T; -- ERROR:

            not overriding
            function F (X : T'Class) return T'Class; -- ERROR:

            not overriding
            function G return T renames F; -- ERROR:

            not overriding
            function "*" (X, Y : T) return T; -- ERROR:

            procedure P (X : T; Y : in out Boolean); -- OK

            function F (Y : Boolean) return T; -- OK

        end Nested;

        overriding
        procedure R (X : T; Y : in out Boolean) renames Nested.P; -- OK

        overriding
        function G (Y : Boolean) return T renames Nested.F; -- OK

    private
        type T is new B831001_0.Pt.T with null record;
    end P9;

    -- Tests similar to those above, except that only instantiations are tested.
    -- Instantiations cannot be intermixed with other tests because they freeze.
    -- Careful: We cannot test primitive operations of a tagged type after an
    -- instance (they cannot be defined or overridden after freezing of the
    -- type).

    package I1 is

        type T is new Integer;

        not overriding
        procedure R is new B831001_0.Gpu (T); -- OK

        not overriding
        function H is new B831001_0.Gfu (T); -- OK

        package Nested is

            not overriding
            procedure R is new B831001_0.Gpu (T); -- ERROR:

            not overriding
            function H is new B831001_0.Gfu (T); -- ERROR:

        end Nested;

    end I1;

    package I2 is

        type T is tagged null record;

        not overriding
        procedure R is new B831001_0.Gpt (T'Class); -- ERROR:

        not overriding
        function H is new B831001_0.Gft (T'Class); -- ERROR:

        package Nested is

            not overriding
            procedure R is new B831001_0.Gpt (T); -- ERROR:

            not overriding
            procedure R is new B831001_0.Gpt (T'Class); -- ERROR:

            not overriding
            function H is new B831001_0.Gft (T'Class); -- ERROR:

        end Nested;

    end I2;

    package I3 is

        type T is abstract tagged private;

    private

        type T is tagged null record;

        not overriding
        procedure R2 is new B831001_0.Gpt (T'Class); -- ERROR:

        not overriding
        function H2 is new B831001_0.Gft (T'Class); -- ERROR:

        package Nested2 is

            not overriding
            procedure R2 is new B831001_0.Gpt (T); -- ERROR:

            not overriding
            procedure R2 is new B831001_0.Gpt (T'Class); -- ERROR:

            not overriding
            function H2 is new B831001_0.Gft (T); -- ERROR:

            not overriding
            function H2 is new B831001_0.Gft (T'Class); -- ERROR:

        end Nested2;

    end I3;

    package I4 is

        type T is new B831001_0.Pu.T;

        overriding
        procedure R is new B831001_0.Gpu (T); -- OK

        not overriding
        procedure R is new B831001_0.Gpu (Integer); -- ERROR:

        overriding
        function "-" is new B831001_0.Gfu (T); -- OK

        overriding
        function "-" is new B831001_0.Gfu (Boolean); -- ERROR:

        package Nested is

            overriding
            procedure R is new B831001_0.Gpu (T); -- ERROR:

            not overriding
            procedure R is new B831001_0.Gpu (Integer); -- ERROR:

            not overriding
            function H is new B831001_0.Gfu (T); -- ERROR:

            not overriding
            function H is new B831001_0.Gfu (Boolean); -- ERROR:

        end Nested;

    end I4;

    package I5 is

        type T is new B831001_0.Pt.T with null record;

        -- Shall be overridden.
        function F return T;
        function G return T renames F;
        function G (Y : Boolean) return T;
        function "-" (Left : T) return T;
        procedure Q (X : in T; A : in Integer) is null;

        overriding
        procedure R is new B831001_0.Gpt (T'Class); -- ERROR:

        not overriding
        function H is new B831001_0.Gft (T'Class); -- ERROR:

        package Nested is

            overriding
            procedure R is new B831001_0.Gpt (T); -- ERROR:

            not overriding
            procedure R is new B831001_0.Gpt (T'Class); -- ERROR:

            not overriding
            function H is new B831001_0.Gft (T'Class); -- ERROR:

        end Nested;

    end I5;

    package I6 is

        type T is new B831001_0.Pt.T with private;

    private

        type T is new B831001_0.Pt.T with null record;

        -- Shall be overridden.
        function F return T;
        function G return T renames F;
        function G (Y : Boolean) return T;
        function "-" (Left : T) return T;
        procedure Q (X : in T; A : in Integer) is null;

        overriding
        procedure R is new B831001_0.Gpt (T'Class); -- ERROR:

        not overriding
        function H is new B831001_0.Gft (T'Class); -- ERROR:

        package Nested is

            overriding
            procedure R is new B831001_0.Gpt (T); -- ERROR:

            not overriding
            procedure R is new B831001_0.Gpt (T'Class); -- ERROR:

            not overriding
            function H is new B831001_0.Gft (T); -- ERROR:

            not overriding
            function H is new B831001_0.Gft (T'Class); -- ERROR:

        end Nested;

    end I6;

end B831001_1;
