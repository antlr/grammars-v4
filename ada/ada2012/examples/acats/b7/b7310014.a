-- B7310014.A
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
--      -> B7310014.A
--         B7310015.A
--         B7310016.AM
--
-- PASS/FAIL CRITERIA:
--      See B7310010.A.
--
-- CHANGE HISTORY:
--      29 Jun 1999   RAD   Initial Version.
--      16 Dec 1999   RLB   Revised to insure that units don't depend on other
--                          units containing errors.
--      21 Mar 2007   RLB   Corrected limited check to work with rule changes
--                          from Amendment 1.
--!

package B7310016_1.Places.P.R is
    package R is
        type Comp2 is limited private;
        type A is array(Integer range <>) of Comp2;
        function A_Func_1 return A;
        function A_Func_2 return A;
        subtype SA is A(1..10);
        Cnst : constant SA;
        type E1 is
            record
                E1: Enum := Bool_Op(Boolean(A_Func_1(1))); -- ERROR:
                    -- Component type not convertible to Boolean here.
            end record;
        B1: Boolean := A_Func_1 = A_Func_2; -- ERROR: No "=".
        type R1 is
            record
                A0: SA := Cnst; -- ERROR:
                    -- Initializing with object of limited type.
            end record;
        procedure P0(Param: A := A_Func_1 xor A_Func_2); -- ERROR: No "xor".
        Image1: String := Comp2'Image(A_Func_2(100)); -- ERROR: No 'Image.
    private
        B2: Boolean := A_Func_1 = A_Func_2; -- ERROR:
        type E2 is
            record
                E: Enum := Bool_Op(Boolean(A_Func_1(1))); -- ERROR:
                    -- Component type not convertible to Boolean here.
            end record;
        Image2: String := Comp2'Image(A_Func_2(100)); -- ERROR: No 'Image.

        type Comp2 is new Comp1;
        -- A becomes nonlimited here.
        -- "="(A, A) return Boolean is implicitly declared here.

        A1: A := A_Func_1; -- OK.
        B3: Boolean := A_Func_1 = A_Func_2; -- OK.
        B4: Boolean := A_Func_1 /= A_Func_2; -- OK.
        A2: A := A_Func_1 xor A_Func_2; -- ERROR: Still no "xor".
        type E3 is
            record
                E: Enum := Bool_Op(Boolean(A_Func_1(1))); -- ERROR:
                    -- Component type not convertible to Boolean here.
            end record;
        Image3: String := Comp2'Image(A_Func_2(100)); -- ERROR: No 'Image.
        Cnst : constant SA := (1..10 => <>); -- OK.
    end R;
    Image4: String := R.Comp2'Image(R.A_Func_2(100)); -- ERROR: No 'Image.
private
    -- Now we find out what Comp1 really is, which reveals
    -- more information about Comp2, but we're not within
    -- the immediate scope of Comp2, so we don't do anything
    -- about it yet.
    A3: R.A(1..10); -- OK.
    A4: R.A := A3; -- ERROR: Initializing with object of limited type.
    procedure P1(Param: R.A := R.A_Func_1 xor R.A_Func_2); -- ERROR:
    procedure P2(Param: R.A := R."xor"(R.A_Func_1, R.A_Func_2)); -- ERROR:
    type E4 is
        record
            E: Enum := Bool_Op(Boolean(R.A_Func_1(1))); -- ERROR:
                -- Component type not convertible to Boolean here.
        end record;
    Image5: String := R.Comp2'Image(R.A_Func_2(100)); -- ERROR: No 'Image.
end B7310016_1.Places.P.R;

-- Note: We don't provide a body for B7310016_1.Places.P.R, as it shouldn't
-- compile anyway.

