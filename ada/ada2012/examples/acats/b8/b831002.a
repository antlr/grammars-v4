-- B831002
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
-- OBJECTIVE:
--    Check that an operation with an indicator of "overriding" is illegal
--    if it does not override a homograph at the place of the indicator.
--
--    Check that an operation with an indicator of "not overriding" is illegal
--    if it does override a homograph at the place of the indicator.
--
-- CHANGE HISTORY:
--      22 Aug 2007   RLB   Created test from submitted test.
--      25 Oct 2007   RLB   Removed unused (and in one case, illegal) generics.
--      28 Apr 2008   RLB   Corrected test error.
--
--!
package B831002 is

    generic
        type T (<>) is private;
    procedure Gpu (X : T; Y : in out T; Z : out T);

    generic
        type T (<>) is private;
    function Gfu (Y : Boolean) return T;


    package Pu is

        type T is new Integer;

        not overriding
        procedure P (X : T); -- OK

        not overriding
        procedure Q (Z : T) renames P; -- OK

        not overriding
        procedure R (X : T; Y : in out Boolean); -- OK

        not overriding
        procedure R (X : T; Y : in out T; Z : out T); -- OK

        not overriding
        function F return T; -- OK

        not overriding
        function G return T renames F; -- OK

        not overriding
        function G (Y : Boolean) return T; -- OK

    end Pu;

    package Pt is

        type T is abstract tagged null record;

        not overriding
        procedure P (X : T); -- OK

        not overriding
        procedure Q (X : in T; A : in Integer) is abstract; -- OK

        not overriding
        procedure R (Z : T) renames P; -- OK

        not overriding
        procedure R (X : T; Y : in out Boolean); -- OK

        not overriding
        procedure S (X : in out T) is null; -- OK

        procedure Z (X : T'Class) is null; -- OK

        not overriding
        function F return T is abstract; -- OK

        not overriding
        function G return T renames F; -- OK

        not overriding
        function G (Y : Boolean) return T is abstract; -- OK

        not overriding
        function "-" (Left : T) return T is abstract; -- OK

    end Pt;


    -- Vanilla untagged type.
    package P1 is

        type T is new Pu.T;

        overriding
        procedure P (X : T); -- OK

        overriding
        procedure P (X : T; Y : out Boolean); -- ERROR: Not overriding.

        not overriding
        procedure Q (Z : T) renames P; -- ERROR: Overriding.

        overriding
        function F return T; -- OK

        overriding
        function F (Y : Duration) return T; -- ERROR: Not overriding.

        not overriding
        function G return T renames F; -- ERROR: Overriding.

        not overriding
        function "-" (Left : T) return T; -- ERROR: Overriding.

        not overriding
        function "-" (Left, Right : T) return T; -- ERROR: Overriding (predefined).

        overriding
        function "*" (Left, Right : T) return T; -- OK (overrides predefined)

        package Nested is

            procedure P (X : T; Y : in out Boolean); -- OK

            function F (Y : Boolean) return T; -- OK

        end Nested;

        overriding
        procedure R (X : T; Y : in out Boolean) renames Nested.P; -- OK

        overriding
        function G (Y : Boolean) return T renames Nested.F; -- OK

    end P1;

    -- Vanilla tagged type.
    package P2 is

        type T is abstract new Pt.T with null record;

        overriding
        procedure P (X : T); -- OK

        overriding
        procedure P (X : T; Y : out Boolean); -- ERROR: Not overriding.

        overriding
        procedure Z (X : T'Class) is null; -- ERROR: Not overriding (not primitive).

        not overriding
        procedure R (Z : T) renames P; -- ERROR: Overriding.

        not overriding
        procedure Q (X : in T; A : in Integer) is abstract; -- ERROR: Overriding

        not overriding
        function "-" (Left : T) return T is abstract; -- ERROR: Overriding.

        overriding
        function "+" (Left, Right : T) return T is abstract; -- ERROR: Not overriding.

        package Nested is

            procedure P (X : T; Y : in out Boolean); -- OK

        end Nested;

        overriding
        procedure R (X : T; Y : in out Boolean) renames Nested.P; -- OK

    end P2;

    -- Private extension.
    package P3 is

        type T is new Pt.T with private;

        overriding
        procedure P (X : T); -- OK

        overriding
        procedure P (X : T; Y : out Boolean); -- ERROR: Not overriding

        not overriding
        procedure R (Z : T) renames P; -- ERROR: Overriding

        overriding
        procedure Q (X : in T; A : in Integer); -- OK

        overriding
        function F return T; -- OK

        overriding
        function F (Y : Duration) return T; -- ERROR: Not overriding

        not overriding
        function G return T renames F; -- ERROR: Overriding

        overriding
        function "-" (Left : T) return T; -- OK

        package Nested is

            procedure P (X : T; Y : in out Boolean); -- OK

            function F (Y : Boolean) return T; -- OK

        end Nested;

        overriding
        procedure R (X : T; Y : in out Boolean) renames Nested.P; -- OK

        overriding
        function G (Y : Boolean) return T renames Nested.F; -- OK

    private
        type T is new Pt.T with null record;
    end P3;

    -- Not visibly tagged derived type.
    package P4 is
        type T is private;
    private
        type T is new Pt.T with null record;

        overriding
        procedure P (X : T); -- OK

        overriding
        procedure P (X : T; Y : out Boolean); -- ERROR: Not overriding

        overriding
        procedure Q (X : in T; A : in Integer); -- OK

        not overriding
        procedure R (Z : T) renames P; -- ERROR: Overriding

        overriding
        function F return T; -- OK

        overriding
        function F (Y : Duration) return T; -- ERROR: Not overriding

        not overriding
        function G return T renames F; -- ERROR:

        overriding
        function "-" (Left : T) return T; -- OK

        package Nested is

            procedure P (X : T; Y : in out Boolean); -- OK

            function F (Y : Boolean) return T; -- OK

        end Nested;

        overriding
        procedure R (X : T; Y : in out Boolean) renames Nested.P; -- OK

        overriding
        function G (Y : Boolean) return T renames Nested.F; -- OK

    end P4;

    -- Note: Since instantiations are freezing, they cannot be used to
    -- create/override primitive operations for tagged types.

    package I1 is

        type T is new Pu.T;

        overriding
        procedure R is new Gpu (T); -- OK

        overriding
        function G is new Gfu (T); -- OK

    end I1;

end B831002;
