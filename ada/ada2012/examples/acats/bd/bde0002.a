-- BDE0002.A
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
--      Check that the explicit declaration of a primitive subprogram of a
--      tagged type must occur before the type is frozen.  Check for cases
--      where the component type of a composite type is a tagged type,
--      and the tagged type is frozen by:
--
--         - The declaration of an object of the composite type.
--
--         - An expression that is an allocator, the type of which
--           designates the composite type.
--
--         - An expression that is an aggregate, which contains
--           a composite value of the composite type.
--
--      Check that the tagged type is not frozen by a nonstatic expression 
--      that is part of a default expression.
--
-- TEST DESCRIPTION:
--      The test declares tagged types, composite types which contain a tagged 
--      type component, type extensions, private extensions, access types,
--      and primitive operations for these types.  The test verifies that 
--      explicit primitive subprogram declaration is illegal after the tagged 
--      type is frozen, and legal otherwise.
--
--
-- CHANGE HISTORY:
--      30 Mar 95   SAIC    Initial prerelease version.
--      16 Jun 95   SAIC    Modified test description per reviewers.  Declared
--                          overriding functions for the derived types.       
--      19 Feb 97   PWB.CTA Eliminated incorrect cases involving aggregates
--                          and replaced implicitly declared "=".
--!

package BDE0002 is

   type Tag_T1 is tagged record
      I : Integer;
   end record;

   function Func return Integer;

   type Rec_W_Tag_T1 is tagged record
      C : Tag_T1 := (I => Func);                 -- Does not freeze Tag_T1.
   end record;

   --  The default expression of component C of record Rec_W_Tag_T1 does not 
   --  freeze Tag_T1.  The declaration of Op1 is therefore legal.

   procedure Op1 (P : Tag_T1);                                       -- OK.

   function Func1 return Tag_T1;

   ObjA : Rec_W_Tag_T1;   

   -- The declaration of ObjA above freezes its nominal subtype Rec_W_Tag_T1, 
   -- which in turn freezes Tag_T1. Therefore, the declaration of Op4 is 
   -- illegal.  

   procedure Op4 (P : Tag_T1);                                       -- ERROR:
                             -- Primitive declaration of a frozen tagged type.

   type Tag_T2 is tagged record
      B : Boolean;
   end record;
   type Arr_Of_Tag_T2 is array (1 .. 5) of Tag_T2;
   type Rec_W_Arr_Of_Tag_T2 is tagged record
      C : Arr_Of_Tag_T2;
   end record;

   ObjB : Rec_W_Arr_Of_Tag_T2;    

   -- The declaration of ObjB above freezes its nominal subtype 
   -- Rec_W_Arr_Of_Tag_T2, which in turn freezes Arr_Of_Tag_T2, and 
   -- thus Tag_T2. Therefore, the declaration of Op6 is illegal. Op5 
   -- is legal, however, since Arr_Of_Tag_T2 is not a tagged type.

   procedure Op5 (P : Arr_Of_Tag_T2);                                -- OK.
   procedure Op6 (P : Tag_T2);                                       -- ERROR:
                             -- Primitive declaration of a frozen tagged type.

   type Tag_T3 is tagged record
      I : Integer;
   end record;
   type Rec_W_Tag_T3 is tagged record
      C : Tag_T3;
   end record;
   type AccRec_W_Tag_T3 is access Rec_W_Tag_T3;
   ObjC : AccRec_W_Tag_T3 := new Rec_W_Tag_T3'(C => (I => 10)); 

   -- The allocator in the initialization expression for ObjC above freezes
   -- the designated subtype Rec_W_Tag_T3 of AccRec_W_Tag_T3, which in turn 
   -- freezes Tag_T3.  Therefore, the declarations of Op7 and Op8 are illegal.

   procedure Op7 (P : Rec_W_Tag_T3);                                 -- ERROR:
                             -- Primitive declaration of a frozen tagged type.

   procedure Op8 (P : access Tag_T3);                                -- ERROR:
                             -- Primitive declaration of a frozen tagged type.

   type Tag_T4 is tagged null record;
   function Func4 return Tag_T4; 
   function Pred (P : Tag_T4) return Boolean;

   type Rec_W_Tag_T4 is record
      C : Tag_T4  := Func4;                         -- Does not freeze Tag_T4.
      D : Boolean := Pred (Func4);
   end record;

   -- The declaration of Rec_W_Tag_T4 does not freeze Tag_T4. The declaration 
   -- of Op9 is therefore legal.
  
   procedure Op9 (P : Tag_T4);                                       -- OK.

   ObjD : Rec_W_Tag_T4;    

   -- The declaration of ObjD above freezes Rec_W_Tag_T4, which in turn 
   -- freezes Tag_T4.  Therefore, the declaration of Op10 is illegal. 

   procedure Op10 (P : Tag_T4);                                      -- ERROR:
                             -- Primitive declaration of a frozen tagged type.

   type Tag_T5 is tagged record
      B : Boolean;
   end record;

   package BDE0002_0 is

      type Rec5 (D : Boolean) is
         record
            case D is
               when False => Cf : Integer;
               when True  => Ct : Tag_T5;           -- Does not freeze Tag_T5.
            end case;
         end record;

      function Equal ( Left, Right : Rec5 ) return Boolean;

      -- The declaration of Ct does not freeze Tag_T5. The declaration of 
      -- Func5 is therefore legal.  

      function Func5 return Tag_T5;                                  -- OK.

   end BDE0002_0;

   ObjE : Boolean := BDE0002_0.Equal ( (False, 1), (False, 1) );

   -- The declaration of ObjE above freezes Rec5, which in turn freezes Tag_T5.
   -- Therefore, the declaration of Op11 is illegal. 

   procedure Op11 (X : Tag_T5);                                      -- ERROR:
                             -- Primitive declaration of a frozen tagged type.

   type New_T1 is new Tag_T1 with private;
   type Arr_Of_New_T1 is array (1 .. 5) of New_T1;

   type Tag_T6 is tagged private;
   type Not_Tag_W_T6 is record                -- not tagged
      C : Tag_T6;
   end record;
   type Arr_Of_Not_Tag_W_T6 is array (1 .. 5) of Not_Tag_W_T6;

private
   type New_T1 is new Tag_T1 with null record;
   function Func1 return New_T1;                               -- Overrides.

   ObjF : Arr_Of_New_T1;                    

   -- The declaration of ObjF above freezes its nominal subtype Arr_Of_New_T1, 
   -- which in turn freezes New_T1. Therefore, the declaration of Op12 is 
   -- illegal. 

   procedure Op12 (P : New_T1);                                      -- ERROR:
                             -- Primitive declaration of a frozen tagged type.

   type Tag_T6 is tagged null record;
   ObjG : Arr_Of_Not_Tag_W_T6;   

   -- The declaration of ObjG above freezes its nominal subtype
   -- Arr_Of_Not_Tag_W_T6, which in turn freezes Not_Tag_W_T6, and thus Tag_T6. 
   -- Therefore, the declaration of Op14 is illegal. Op13 is legal, however, 
   -- since Not_Tag_W_T6 is not a tagged type. 

   procedure Op13 (P : Not_Tag_W_T6);                                -- OK.
   procedure Op14 (P : access Tag_T6);                               -- ERROR:
                             -- Primitive declaration of a frozen tagged type.

   type New_T2 is new Tag_T1 with record
      S : String (1 .. 100);
   end record;
   function Func1 return New_T2;                               -- Overrides.

   type Rec_W_New_T2 is tagged record
      C : New_T2;
   end record;

   function Func6 return String;

   ObjH : Rec_W_New_T2;   

   -- The declaration of ObjH above freezes its nominal subtype Rec_W_New_T2, 
   -- which in turn freezes New_T2. Therefore, the declarations of Op17 and 
   -- Op18 are illegal. 

   procedure Op17 (P : New_T2);                                      -- ERROR:
                             -- Primitive declaration of a frozen tagged type.

   procedure Op18 (P : Rec_W_New_T2);                                -- ERROR:
                             -- Primitive declaration of a frozen tagged type.

end BDE0002;
