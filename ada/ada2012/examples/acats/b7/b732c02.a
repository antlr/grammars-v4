-- B732C02.A
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
-- OBJECTIVE:
--      Check that aspect Type_Invariant'Class can only be specified on a
--      private type declaration, a private extension declaration, or an
--      interface type.
--
--      Check that aspect Type_Invariant'Class cannot be specified on an
--      an untagged type.

-- TEST DESCRIPTION:
--      We try giving aspect Type_Invariant'Class on a range of entities where
--      it is not allowed (* marks items that test the second objective):
--         (A) Object declarations (even of a private type);
--         (B) Subprograms (even with a first parameter of a private type);
--         (C) Packages;
--         (D) Subtypes (even if it is allowed on the type);
--         (E) Full types other than interfaces:
--             (1)* Array types;
--             (2) Record types (also tests * if untagged);
--             (3) Record extensions;
--             (4)* Access types (even access-to-private);
--             (5)* Scalar types;
--         (F)* Untagged private types.

--      Note that Type_Invariant'Class is different than Type_Invariant in
--      that interfaces are allowed but hidden aspects are not. (Interfaces
--      are allowed by AI12-0041-1).

-- CHANGE HISTORY:
--    15 Jan 2015   RLB   Created test.
--    13 Mar 2015   RLB   Eliminated overlong lines.
--     2 Feb 2017   RLB   Moved primitive declared too late so it's otherwise
--                        legal. Added location indicators.
--
--!
with Ada.Assertions;
with F732C00;

package B732C02 is

   pragma Assertion_Policy (Check);

   subtype Root_Tagged is F732C00.Change_Tagged_Type;

   type Priv is private
      with Type_Invariant'Class => Is_Foo (Priv);           -- ERROR: (F) {12}

   function Is_Foo (Obj : Priv) return Boolean;

   type TPriv is tagged private
      with Type_Invariant'Class => Is_Foo (TPriv);          -- OK. {12}

   function Is_Foo (Obj : TPriv) return Boolean;

   CTPriv : constant TPriv
      with Type_Invariant'Class => Is_Foo (CTPriv);         -- ERROR: (A) {12}

   function Is_Bar (Obj : TPriv) return Boolean
      with Type_Invariant'Class => Is_Foo (Is_Bar);         -- ERROR: (B) {12}

   type Ext is new Root_Tagged with private
      with Type_Invariant'Class => Is_Foo (Ext);            -- OK. {12}

   function Is_Foo (Obj : Ext) return Boolean;

   CExt : constant Ext
      with Type_Invariant'Class => Is_Foo (CExt);           -- ERROR: (A) {12}

   function Is_Bar (Obj : Ext) return Boolean
      with Type_Invariant'Class => Is_Foo (Is_Bar);         -- ERROR: (B) {12}

   type FPriv is private;

   CFPriv : constant FPriv;

   type FTPriv is tagged private;

   CFTPriv : constant FTPriv;

   type FExt is new Root_Tagged with private;

   CFExt : constant FExt;

   type NPriv is tagged private;

   function Is_Foo (Obj : NPriv) return Boolean;

   CNPriv : constant NPriv;

   type NExt is new Root_Tagged with private;

   function Is_Foo (Obj : NExt) return Boolean;

   CNExt : constant NExt;

   package Inner
      with Type_Invariant'Class => Is_Foo (CPriv) is        -- ERROR: (C) {12}
      Var : Natural := 0;
   end Inner;

   subtype SPriv is NPriv
      with Type_Invariant'Class => Is_Foo (SPriv);          -- ERROR: (D) {12}

   subtype SExt is NExt
      with Type_Invariant'Class => Is_Foo (SExt);           -- ERROR: (D) {12}

   type VArr is array (1 .. 10) of NPriv
      with Type_Invariant'Class => Is_Foo (VArr);           -- ERROR: (E1) {12}

   function Is_Foo (Obj : VArr) return Boolean is
      (for all I in VArr'range => Is_Foo (Obj(I)));

   type VRec is record
      G : NPriv;
   end record
      with Type_Invariant'Class => Is_Foo (VRec);           -- ERROR: (E2) {12}

   function Is_Foo (Obj : VRec) return Boolean is
      (Is_Foo (Obj.G));

   type VExt is new Root_Tagged with record
      H : NPriv;
   end record
      with Type_Invariant'Class => Is_Foo (VExt);           -- ERROR: (E3) {12}

   function Rounds return VExt is (F732C00.Rounds with H => CNPriv);

   function Invalid_Change return VExt is (F732C00.Invalid_Change with CNPriv);

   function Is_Foo (Obj : VExt) return Boolean is (Is_Foo (Obj.H));

   type VAcc is access all NPriv
      with Type_Invariant'Class => Is_Foo (VAcc);           -- ERROR: (E4) {12}

   function Is_Foo (Obj : VAcc) return Boolean is
      (Is_Foo (Obj.all));

   type VInt is range 0 .. 12
      with Type_Invariant'Class => Is_Foo (VInt);           -- ERROR: (E5) {12}

   function Is_Foo (Obj : VInt) return Boolean is (Obj = 12);

   type VMod is mod 2**8
      with Type_Invariant'Class => Is_Foo (VMod);           -- ERROR: (E5) {12}

   function Is_Foo (Obj : VMod) return Boolean is (Obj /= 100);

   type VEnum is (Yellow, Red, Green, Blue)
      with Type_Invariant'Class => Is_Foo (VEnum);          -- ERROR: (E5) {12}

   function Is_Foo (Obj : VEnum) return Boolean is (Obj /= Blue);

   type VIntf is interface
      with Type_Invariant'Class => Is_Foo (VIntf);          -- OK. {12}

   function Is_Foo (Obj : VIntf) return Boolean is abstract;

private

   type Priv is record
      A : Natural := 0;
   end record;

   function Is_Foo (Obj : Priv) return Boolean is (Obj.A /= 0);

   type TPriv is tagged record
      A : Natural := 0;
   end record;

   function Is_Foo (Obj : TPriv) return Boolean is (Obj.A /= 0);

   CTPriv : constant TPriv := (A => 1);

   function Is_Bar (Obj : TPriv) return Boolean is (Obj.A = 0);

   type Ext is new Root_Tagged with record
      B : Natural := 1;
   end record;

   function Rounds return Ext is (F732C00.Rounds with B => 4);

   function Invalid_Change return Ext is (F732C00.Invalid_Change with B => 4);

   function Is_Foo (Obj : Ext) return Boolean is (Obj.B /= 0);

   function Is_Bar (Obj : Ext) return Boolean is (Obj.B = 0);

   CExt : constant Ext := (F732C00.Rounds with B => 1);

   type FPriv is record
      C : Natural := 0;
   end record
      with Type_Invariant'Class => Is_Foo (FPriv);          -- ERROR: (E2) {12}

   function Is_Foo (Obj : FPriv) return Boolean is (Obj.C /= 0);

   CFPriv : constant FPriv := (C => 1)
      with Type_Invariant'Class => Is_Foo (CFPriv);         -- ERROR: (A) {12}

   type FTPriv is tagged record
      C : Natural := 0;
   end record
      with Type_Invariant'Class => Is_Foo (FPriv);          -- ERROR: (E2) {12}

   function Is_Foo (Obj : FTPriv) return Boolean is (Obj.C /= 0);

   function Is_Bar (Obj : FTPriv) return Boolean is (Obj.C = 0)
      with Type_Invariant'Class => Is_Foo (Is_Bar);         -- ERROR: (B) {12}

   CFTPriv : constant FTPriv := (C => 1)
      with Type_Invariant'Class => Is_Foo (CFTPriv);        -- ERROR: (A) {12}

   type FExt is new Root_Tagged with record
      D : Natural := 1;
   end record
      with Type_Invariant'Class => Is_Foo (FExt);           -- ERROR: (E3) {12}

   function Rounds return FExt is (F732C00.Rounds with D => 7);

   function Invalid_Change return FExt is (F732C00.Invalid_Change with D => 7);

   function Is_Foo (Obj : FExt) return Boolean is (Obj.D /= 0);

   function Is_Bar (Obj : FExt) return Boolean is (Obj.D = 0)
      with Type_Invariant'Class => Is_Foo (Is_Bar);         -- ERROR: (B) {12}

   CFExt : constant FExt :=  (F732C00.Rounds with D => 2)
      with Type_Invariant'Class => Is_Foo (CFExt);          -- ERROR: (A) {12}

   type NPriv is tagged record
      E : Natural := 0;
   end record;

   function Is_Foo (Obj : NPriv) return Boolean is (Obj.E /= 0);

   CNPriv : constant NPriv := (E => 12);

   type NExt is new Root_Tagged with record
      F : Natural := 1;
   end record;

   function Rounds return NExt is (F732C00.Rounds with F => 3);

   function Invalid_Change return NExt is (F732C00.Invalid_Change with F => 9);

   function Is_Foo (Obj : NExt) return Boolean is (Obj.F /= 0);

   CNExt : constant NExt := (F732C00.Rounds with F => 2);

   PObj : NPriv
      with Type_Invariant'Class => Is_Foo (PObj);           -- ERROR: (A) {12}

   PEObj : NExt := CNExt
      with Type_Invariant'Class => Is_Foo (PEObj);          -- ERROR: (A) {12}

   type HArr is array (1 .. 10) of NPriv
      with Type_Invariant'Class => Is_Foo (HArr);           -- ERROR: (E1) {12}

   function Is_Foo (Obj : HArr) return Boolean is
      (for all I in HArr'range => Is_Foo (Obj(I)));

   type HRec is record
      G : NPriv;
   end record
      with Type_Invariant'Class => Is_Foo (HRec);           -- ERROR: (E2) {12}

   function Is_Foo (Obj : HRec) return Boolean is
      (Is_Foo (Obj.G));

   type HExt is new Root_Tagged with record
      H : NPriv;
   end record
      with Type_Invariant'Class => Is_Foo (HExt);           -- ERROR: (E3) {12}

   function Rounds return HExt is (F732C00.Rounds with H => CNPriv);

   function Invalid_Change return HExt is (F732C00.Invalid_Change with CNPriv);

   function Is_Foo (Obj : HExt) return Boolean is (Is_Foo (Obj.H));

   type HAcc is access all NPriv
      with Type_Invariant'Class => Is_Foo (HAcc);           -- ERROR: (E4) {12}

   function Is_Foo (Obj : HAcc) return Boolean is
      (Is_Foo (Obj.all));

   type HInt is range 0 .. 12
      with Type_Invariant'Class => Is_Foo (HInt);           -- ERROR: (E5) {12}

   function Is_Foo (Obj : HInt) return Boolean is (Obj = 12);

   type HMod is mod 2**8
      with Type_Invariant'Class => Is_Foo (HMod);           -- ERROR: (E5) {12}

   function Is_Foo (Obj : HMod) return Boolean is (Obj /= 100);

   type HEnum is (Yellow, Red, Green, Blue)
      with Type_Invariant'Class => Is_Foo (HEnum);          -- ERROR: (E5) {12}

   function Is_Foo (Obj : HEnum) return Boolean is (Obj /= Blue);

   type HIntf is limited interface
      with Type_Invariant'Class => Is_Foo (HIntf);          -- OK. {12}

   function Is_Foo (Obj : HIntf) return Boolean is abstract;

end B732C02;
