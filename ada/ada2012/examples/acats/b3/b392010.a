-- B392010.A
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
--     Check that a primitive operation of two tagged types is
--     illegal, even when the types are not known to be tagged at the
--     place where the operation is declared. This was confirmed by
--     AI-00183.
--
-- TEST DESCRIPTION
--     We first try the obvious case where the types *are* known to be tagged.
--     Then we try a case of private types, which turn out to be tagged.
--     Finally, we try a case of incomplete types which turn out to be
--     tagged when completed.
--
-- CHANGE HISTORY:
--      29 Jun 1999   RAD   Initial Version.
--      21 Sep 1999   RLB   Repaired as suggested by RAD.
--      21 Mar 2007   RLB   Placed test into a single specification,
--                          eliminated deferred incomplete type test (as it
--                          is illegal by 3.10.1(9.3/2)).
--      18 Aug 2007   RLB   Removed obsolete comment from header.
--
--!

package B392010 is
    package Pack_0 is
        type T1 is tagged null record;
        type T2 is tagged null record;
        procedure Primitive_Proc (X1 : T1; X2 : T2); -- ERROR:
            -- Violates 3.9.2(12).
        function Primitive_Func (X1 : T1) return T2; -- ERROR:
            -- Violates 3.9.2(12).
    end Pack_0;

    package Pack_1 is
        type T1 is private;
        type T2 is private;
        procedure Primitive_Proc (X1 : T1; X2 : T2); -- ERROR:
            -- Violates 3.9.2(12).
        function Primitive_Func (X1 : T1) return T2; -- ERROR:
            -- Violates 3.9.2(12).
    private
        type T1 is tagged null record;
        type T2 is tagged null record;
    end Pack_1;

    package Pack_2 is
        type T1;
        type T2;
        procedure Primitive_Proc (X1 : access T1; X2 : access T2); -- ERROR:
            -- Violates 3.9.2(12).
        function Primitive_Func (X1 : access T1) return access T2; -- ERROR:
            -- Violates 3.9.2(12).

        type T1 is tagged null record;
        type T2 is tagged null record;
    end Pack_2;
end B392010;
