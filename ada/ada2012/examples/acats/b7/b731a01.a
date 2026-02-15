-- B731A01.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
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
--      Check that the inherited primitive subprograms of a derived type
--      definition are implicitly declared at the earliest place within the
--      immediate scope of the type declaration (but after the type
--      declaration) where the corresponding declaration from the parent is
--      visible. Check that, within its scope, the full view determines
--      which components are visible.
--
--      Check for the cases where the parent is a partial view (tagged private
--      type) declared in a package, and the derived type is declared in:
--
--         - the visible part of a public child unit
--         - a package nested within the visible part of a public child unit
--
-- TEST DESCRIPTION:
--      A tagged private type is declared in the visible part of a package,
--      along with operations on the type. The full view of the tagged
--      private type is declared in the private part, along with additional
--      operations on the type.
--
--      The inherited primitive subprograms of a type derived from the tagged
--      private type will include the operations declared in both the visible
--      and private parts. However, the subprograms declared in the private
--      part are only available within the declarative region of the package
--      (which includes its child units).
--
--      The operations and components of the derived type become available at
--      the first place within the immediate scope of the derived type where
--      the corresponding operations and components of the parent type become
--      visible. For a type derived in a public child unit, they become
--      available in the private part. For a package nested within the visible
--      part of a public child unit, they become available in the body.
--
-- TEST FILES:
--      The following files comprise this test:
--
--         F731A00.A
--         B731A01.A
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      14 Nov 95   SAIC    ACVC 2.0.1 fixes: Changed Object1E case from
--                          ERROR to OK. Modified incorrect commentary.
--
--!

package F731A00.B731A01 is

   type Derived1 is new Parent with null record;
   -- function Vis_Op (P: Derived1) return Boolean implicitly declared here.

   Operand1 : Derived1;

   Object1A : Boolean := Vis_Op(Operand1);                            -- OK.

   Object1B : Boolean := Pri_Op(Operand1);                            -- ERROR:
                                           -- Pri_Op not declared for Derived1.

   Object1C : Integer  := Operand1.Component;                         -- ERROR:
                                        -- Component not declared for Derived1.

   type Derived2 is new Parent with private;
   -- function Vis_Op (P: Derived2) return Boolean implicitly declared here.

   -- The availability of operations on, and components of, type Derived2 can
   -- not be checked before the full type declaration, since objects of
   -- type Derived2 cannot be declared until then.

--
-- Nested package:
--

   package Nested is

      type Derived3 is new Parent with null record;
      -- function Vis_Op (P: Derived3) return Boolean implicitly declared here.

      Operand3 : Derived3;

      Object3A : Boolean := Vis_Op(Operand3);                         -- OK.


      Object3B : Boolean := Pri_Op(Operand3);                         -- ERROR:
                                           -- Pri_Op not declared for Derived3.

      Object3C : Integer := Operand3.Component;                       -- ERROR:
                                        -- Component not declared for Derived3.

   private
      Object3D : Boolean := Pri_Op(Operand3);                         -- ERROR:
                                           -- Pri_Op not declared for Derived3.

      Object3E : Integer := Operand3.Component;                       -- ERROR:
                                        -- Component not declared for Derived3.

   end Nested;


private
   -- function Pri_Op (P: Derived1) return Boolean implicitly declared here.

   Object1D : Boolean := Pri_Op(Operand1);                            -- OK.
   Object1E : Integer := Operand1.Component;                          -- OK.


   type Derived2 is new Parent with null record;
   -- function Pri_Op (P: Derived2) return Boolean implicitly declared here.

   Operand2 : Derived2;

   Object2A : Boolean  := Vis_Op(Operand2);                           -- OK.
   Object2B : Boolean  := Pri_Op(Operand2);                           -- OK.
   Object2C : Integer  := Operand2.Component;                         -- OK.

end F731A00.B731A01;
