-- BDE0008.A
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
--      Check that a representation clause for a type must occur before the 
--      type is frozen.  Check for cases where the type is frozen by a static
--      expression or a nonstatic expression which is not a default expression.
--      Check that a nonstatic expression that is part of a default expression
--      does not cause freezing.  Check for cases of subprogram renaming.
--
-- TEST DESCRIPTION:
--      The test declares tagged types, type extensions, and representation 
--      clauses for these types.  The test verifies that representation clause 
--      declaration is illegal after the type is frozen, and legal otherwise.
--
--
-- CHANGE HISTORY:
--      28 Apr 95   SAIC    Initial prerelease version.
--      27 Sep 96   SAIC    Replaced 'Size with 'Alignment. Deleted subtest 
--                          Tag_T8.
--      27 Feb 97   PWB.CTA Removed extraneous comments; changed rep clause.
--!

with System.Storage_Elements;
package BDE0008 is
   type Tag_T1 is tagged record
      I : Integer;
   end record;
   Obj1 : Tag_T1'Class := Tag_T1'(I => 23);                    

   -- The nonstatic initialization expression for Obj1 above freezes Tag_T1. 
   -- The representation clause is therefore illegal.  RM 13.14(8,19);6.0    

   for Tag_T1'Alignment use 1;                                        -- ERROR:
                              -- Representation clause of a frozen tagged type.

   ---------------------------------------------------------------------------

   type Tag_T2 is range 1 .. 10;
   function Func2 return Tag_T2;
   type Rec1 is record
      I : Tag_T2 := Func2;
   end record;

   -- The nonstatic default expression for Rec1.I does not freeze Tag_T2.  The 
   -- representation clause is therefore legal.  RM 13.14(8,19);6.0

   for Tag_T2'Alignment use 2;                                        -- OK.

   type Rec2 is record
      I : Tag_T2 := 10;
   end record;

   -- The static default expression for Rec2.I above freezes Tag_T2. 
   -- The representation clause is therefore illegal. RM 13.14(8,19);6.0    

   for Tag_T2'Alignment use 1;                                        -- ERROR:
                              -- Representation clause of a frozen tagged type.

   ---------------------------------------------------------------------------
   type Type_T3 is new String (1 .. 8);
   function Func3 return Type_T3;
   procedure Op3 (P : Type_T3 := Func3);

   -- The nonstatic default expression for parameter P of subprogram Op3 
   -- does not freeze Tag_T3.  The representation clause is therefore legal.  
   -- RM 13.14(8,19);6.0

   for Type_T3'alignment use 1;                                      -- OK.

   procedure Op4 (P : Type_T3 := "freezing");

   -- The static default expression for subprogram Op4 above freezes Tag_T3.  
   -- The representation clause is therefore illegal. RM 13.14(8,19);6.0

   for Type_T3'Alignment use 1;                                       -- ERROR:
                                     -- Representation clause of a frozen type.

   ---------------------------------------------------------------------------
   type Disc is (On, Off);
   type Disc_Rec (D : Disc := Off) is record
       case D is
          when On  => Type_Int : Integer;
          when Off => Type_T5  : Boolean;
       end case;
   end record;

   -- The static default expression for Disc_Rec.D above freezes Disc.  The 
   -- representation clause is therefore illegal. RM 13.14(8,19);6.0

   for Disc'Alignment use 1;                                          -- ERROR:
                                     -- Representation clause of a frozen type.

   ---------------------------------------------------------------------------
   type Enum is (Red, White, Yellow);
   ObjA : Enum;
   ObjB : Enum := ObjA;

   -- The nonstatic expression ObjA in the initialization expression of ObjB
   -- above freezes ObjA.  The representation clause is therefore illegal. 
   -- RM 13.14(4,8,19);6.0

   for ObjA'Size use System.Storage_Unit;                             -- ERROR:
                                   -- Representation clause of a frozen object.

   ---------------------------------------------------------------------------
   type Tag_T6 is tagged record
      I : Integer;
   end record;
   function Func6    (P : Tag_T6) return Tag_T6;
   -- normal renames
   function RN_Func6 (P : Tag_T6) return Tag_T6 renames Func6;   

   -- The renaming of Func6 does not freeze Tag_T6.  The representation clause
   -- is therefore legal.  --# AARM 13.14(3.c);6.0

   for Tag_T6'Alignment use 1;                                        -- OK.

   type Tag_T7 is tagged record
      I : Integer;
   end record;

   function Func7  (P : Tag_T7) return Tag_T7;
   function RN_Func7 (P : Tag_T7) return Tag_T7;
   -- renames as body
   function RN_Func7 (P : Tag_T7) return Tag_T7 renames Func7;   

   -- The renaming as body of Func7 does not freeze Tag_T7.  The 
   -- representation clause is therefore legal.  --# AARM 13.14(3.c);6.0

   for Tag_T7'Alignment use 1;                                        -- OK.

end BDE0008;
