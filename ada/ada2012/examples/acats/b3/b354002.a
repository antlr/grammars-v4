-- B354002.A
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
-- OBJECTIVES:
--     Check that the prefix of the Mod attribute must be a modular subtype.
--     Check that the result subtype of the Mod attribute is the subtype
--     identified by the prefix. Check that the argument of the Mod attribute
--     can be any integer type.
--
-- CHANGE HISTORY:
--     20 Mar 17   RLB     Created test.
--
--!
procedure B354002 is

   type A_Mod   is mod 2**8;
   type An_Int  is range 0 .. 255;
   type A_Flt   is digits 5;
   type An_Enum is (Red, Blue, Green);
   type Other_Mod is mod 1999;

   function Foob return A_Mod is
   begin
       return 12;
   end Foob;

   function Foob return An_Int is
   begin
       return 52;
   end Foob;

   function Boof return An_Int is
   begin
       return 92;
   end Boof;

   function Boof return An_Enum is
   begin
       return Green;
   end Boof;

   function Foof return An_Int is
   begin
       return 88;
   end Foof;

   function Foof return A_Flt is
   begin
       return 3.14159;
   end Foof;

   M1 : A_Mod := 16;

   M2 : Other_Mod := 0;

   I1 : An_Int := 4;

   E1 : An_Enum := Blue;

   F1 : A_Flt := 4.0;

begin

   M1 := A_Mod'Mod (1);                       -- OK. {4;1}

   M2 := A_Mod'Mod (2);                       -- ERROR: Wrong subtype {4;1}

   M1 := An_Int'Mod (3);                      -- ERROR: Not modular {10;1}

   M1 := A_Flt'Mod (4);                       -- ERROR: Not modular {10;1}

   M1 := Other_Mod'Mod (5);                   -- ERROR: Wrong subtype {4;1}

   M1 := M1'Mod (5);                          -- ERROR: Not a subtype {10;1}

   M1 := Foob'Mod (5);                        -- ERROR: Not a subtype {10;1}

   M1 := A_Mod'Mod (M1);                      -- OK. {4;1}

   M1 := A_Mod'Mod (M2);                      -- OK. {4;1}

   M1 := A_Mod'Mod (I1);                      -- OK. {4;1}

   M1 := A_Mod'Mod (E1);                      -- ERROR: Arg not integer {10;1}

   M1 := A_Mod'Mod (F1);                      -- ERROR: Arg not integer {10;1}

   M1 := A_Mod'Mod (2.5);                     -- ERROR: Arg not integer {10;1}

   M1 := A_Mod'Mod (Foob);                    -- ERROR: Arg ambiguous {10;1}

   M1 := A_Mod'Mod (An_Int'(Foob));           -- OK. {4;1}

   M1 := A_Mod'Mod (Foof);                    -- OK. {4;1}

   M1 := A_Mod'Mod (Boof);                    -- OK. {4;1}

end B354002;
