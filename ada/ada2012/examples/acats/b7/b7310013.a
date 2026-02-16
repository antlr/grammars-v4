-- B7310013.A
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
--      -> B7310013.A
--         B7310014.A
--         B7310016.AM
--
-- PASS/FAIL CRITERIA:
--      See B7310010.A.
--
-- CHANGE HISTORY:
--      29 JUN 1999   RAD   Initial Version.
--      16 DEC 1999   RLB   Revised to insure that units don't depend on other
--                          units containing errors.
--!

package body B7310016_1.Places.P.Q is
    Enum_Obj: Another_Enum; -- OK
    type E5 is
        record
            E: Another_Enum := Bool_Op(Boolean(R.A_Func_1(1))); -- ERROR:
                -- Component type not convertible to Boolean here.
        end record;
    Image6: String := R.Comp2'Image(R.A_Func_2(100)); -- ERROR: No 'Image.

    procedure P3(Param: R.A := A_Func_1 xor A_Func_2) -- ERROR:
        is begin null; end;
    procedure P4(Param: R.A := R.A_Func_1 xor R.A_Func_2) -- ERROR:
        is begin null; end;
    procedure P5(Param: R.A := R."xor"(R.A_Func_1, R.A_Func_2)) -- ERROR:
        is begin null; end;

    package body R is
        -- Things like "xor"(A,A) return A are implicitly
        -- declared here.

        A7: A := A_Func_1 xor A_Func_2; -- OK

        function A_Func_1 return A is
        begin
            return A_Func_1;
        end A_Func_1;

        function A_Func_2 return A is
        begin
            return A_Func_2;
        end A_Func_2;

        A8: A := A_Func_1 and A_Func_2; -- OK
        A9: A := A_Func_1 or A_Func_2; -- OK
        A10: A := not A_Func_1; -- OK

        type E6 is
            record
                E: Another_Enum := Bool_Op(Boolean(R.A_Func_1(1))); -- OK
            end record;

        Image7: String := R.Comp2'Image(R.A_Func_2(100)); -- OK

    end R;

    procedure P6(Param: R.A := R.A_Func_1 xor R.A_Func_2) -- ERROR:
        is begin null; end;

    use type R.A;
    procedure P7(Param: R.A := R.A_Func_1 xor R.A_Func_2) -- ERROR:
        -- No "xor".
        is begin null; end;

    type E7 is
        record
            E: Enum := Bool_Op(Boolean(R.A_Func_1(1))); -- ERROR:
                -- Component type not convertible to Boolean here.
        end record;

    Image8: String := R.Comp2'Image(R.A_Func_2(100)); -- ERROR: No 'Image.

end B7310016_1.Places.P.Q;

