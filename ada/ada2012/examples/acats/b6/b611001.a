-- B611001.A
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
--
--*
--
-- OBJECTIVE:
--
--  Check that Pre and Post cannot be specified for an instance that is
--  a subprogram.
--
--  Check that Pre and Post cannot be specified for packages, objects, types,
--  single tasks, or single protected objects.
--
-- CHANGE HISTORY:
--     03 Feb 2016   RLB   Created test.
--     28 Mar 2016   RLB   Added error location codes.
--
--!
package B611001 is

   Glob : Integer := 0;

   generic
   procedure Gen (P : in out Natural);

   procedure Inst1 is new Gen
      with Pre => P > 12;                                      -- ERROR: {12;1}

   procedure Inst2 is new Gen
      with Post => P < 100;                                    -- ERROR: {12;1}

   generic
   procedure Gen2 (P : in out Natural)
      with Pre => P > 12;                                      -- OK. {12;1}

   generic
   procedure Gen3 (P : in out Natural)
      with Post => P < 100;                                    -- OK. {12;1}

   package Nest1 with Pre => Glob > 0 is                       -- ERROR: {23;3}
      procedure P (Arg : in out Natural);
   end Nest1;

   package Nest2 with Post => Glob = 0 is                      -- ERROR: {23;3}
      procedure P (Arg : in out Natural);
   end Nest2;

   Fooey1 : Character := 'A' with Pre => Fooey1 in 'A' .. 'Z'; -- ERROR: {35;1}

   Fooey2 : Character := 'A' with Post => Fooey1 in 'A' .. 'Z';-- ERROR: {35;1}

   type Small_Int_Even is range 0 .. 99
      with Pre => Small_Int_Even mod 2 = 0;                    -- ERROR: {12;1}

   type Small_Int_Odd is range 0 .. 99
      with Post => Small_Int_Even mod 2 /= 0;                  -- ERROR: {12;1}

   type Rec1 is record
      C : Character;
   end record with Pre => C in '0' .. '9';                     -- ERROR: {20;1}

   type Rec2 is record
      C : Character;
   end record with Post => C in 'A' .. 'M';                    -- ERROR: {20;1}

   task type Tsk1 (D : Natural) with Pre => D mod 2 = 0 is     -- ERROR: {38;3}
      entry Ent;
   end Tsk1;

   task type Tsk2 (D : Natural) with Post => D / 5 > 10 is     -- ERROR: {38;3}
      entry Ent;
   end Tsk2;

   task type Tsk3 (D : Natural) is
      entry Ent with Pre  => D mod 2 = 0,                      -- OK. {22}
                     Post => D < 100;                          -- OK. {22}
   end Tsk3;

   task Tsk4 with Pre => Glob > 0 is                           -- ERROR: {19;3}
      entry Ent;
   end Tsk4;

   task Tsk5 with Post => Glob > 1 is                          -- ERROR: {19;3}
      entry Ent;
   end Tsk5;

   task Tsk6 is
      entry Ent (Pm : in out Natural)
          with Pre  => Pm mod 2 = 0,                           -- OK. {16}
               Post => Pm < 100;                               -- OK. {16}
   end Tsk6;

   protected type Prot1 (D : Natural)
                                 with Pre => D mod 2 /= 0 is   -- ERROR: {39;3}
      function Get_It return Float;
      procedure Set_It (F : Float);
   private
      It : Float;
   end Prot1;

   protected type Prot2 (D : Natural) with Post => D < 92 is   -- ERROR: {44;3}
      function Get_It return Float;
      procedure Set_It (F : Float);
   private
      It : Float;
   end Prot2;

   protected type Prot3 (D : Natural) is
      function Get_It return Float
         with Post => Get_It'Result > Float(D)/100.0;          -- OK. {15;1}
      procedure Set_It (F : Float)
         with Pre => F > Float(D)/100.0;                       -- OK. {15;1}
   private
      It : Float;
   end Prot3;

   protected Prot4 with Pre => Glob > 0 is                     -- ERROR: {25;3}
      function Get_It return Float;
      procedure Set_It (F : Float);
   private
      It : Float;
   end Prot4;

   protected Prot5 with Post => Glob < 100 is                  -- ERROR: {25;3}
      function Get_It return Float;
      procedure Set_It (F : Float);
   private
      It : Float;
   end Prot5;

   protected Prot6 is
      function Get_It return Float
         with Post => Get_It'Result > Float(Glob)/100.0;       -- OK. {15;1}
      procedure Set_It (F : Float)
         with Pre => F > Float(Glob)/100.0;                    -- OK. {15;1}
   private
      It : Float;
   end Prot6;

end B611001;
