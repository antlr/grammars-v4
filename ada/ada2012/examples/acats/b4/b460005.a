-- B460005.A
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
-- OBJECTIVE:
--    Check that in a view conversion of arrays, if the target type has
--    aliased components, the operand type must also have aliased components.
--    See 4.6(24.8/2).
--
--    Check that in a view conversion of arrays, the operand type cannot have
--    a tagged or private subcomponent. See 4.6(24.9/2).
--
-- CHANGE HISTORY:
--    18 JAN 2001   PHL   Initial version.
--    15 MAR 2001   RLB   Readied for release.
--    16 MAR 2007   RLB   Updated for Amendment 1 changes.
--    18 AUG 2007   RLB   Untagged (which all arrays are) view conversions
--                        must support reverse conversions by 4.6(8/2), so
--                        AR1(XR2) is illegal.
--
--!
procedure B460005 is

    type Rec is record
      C : Character := 'A';
      B : Boolean := True;
    end record;
    R1 : constant Rec := (C => '1', B => False);
    R2 : constant Rec := (C => '2', B => False);

    package P is
	type T is private;
	C1 : constant T;
	C2 : constant T;
    private
	type T (D : Integer := 0) is null record;
	C1 : constant T := (D => 1);
	C2 : constant T := (D => 2);
    end P;

    type Tag is tagged record
       N : Natural := 0;
    end record;
    T1 : constant Tag := (N => 10);
    T2 : constant Tag := (N => 20);

    type AR1 is array (1 .. 10) of aliased Rec;
    type AR2 is array (1 .. 10) of Rec;

    XR1 : AR1 := (1 .. 10 => R1);
    XR2 : AR2 := (1 .. 10 => R2);

    procedure S (X : in out AR1) is
    begin
	X (X'First) := R2;
    end S;

    procedure S (X : in out AR2) is
    begin
	X (X'First) := R1;
    end S;

    type AT1 is array (1 .. 10) of Tag;
    type AT2 is array (1 .. 10) of Tag;

    XT1 : AT1 := (1 .. 10 => T1);
    XT2 : AT2 := (1 .. 10 => T2);

    procedure S (X : in out AT2) is
    begin
	X (X'First) := T2;
    end S;

    type AP1 is array (1 .. 10) of P.T;
    type AP2 is array (1 .. 10) of P.T;

    XP1 : AP1 := (1 .. 10 => P.C1);
    XP2 : AP2 := (1 .. 10 => P.C2);

    procedure S (X : in out AP2) is
    begin
	X (X'First) := P.C2;
    end S;

begin
    S (AR2 (XR1));    -- ERROR: Back-conversion not allowed because operand has
                      -- aliased component, target does not.
    S (AR1 (XR2));    -- ERROR: Target has aliased component, operand does not.
    XR1 := AR1 (XR2); -- OK. (Not a view conversion.)

    S (AT2 (XT1));    -- ERROR: Component type is tagged.
    XT2 := AT2 (XT1); -- OK. (Not a view conversion.)

    S (AP2 (XP1));    -- ERROR: Component type is private.
    XP2 := AP2 (XP1); -- OK. (Not a view conversion.)
end B460005;

