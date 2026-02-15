-- BDE0001.A
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
--      where the tagged type is frozen by:
--
--         - The declaration of a record extension (check also that a private
--           extension does not freeze the parent type, and that freezing
--           is deferred until the full type declaration).
--
--         - The declaration of an object of the type.
--
--         - An expression that is an allocator, the type of which designates
--           the tagged type.
--
--      Check that the tagged type is not frozen by a nonstatic expression 
--      that is part of a default expression.
--
-- TEST DESCRIPTION:
--      The test declares tagged types, type extensions, private extensions,
--      and primitive operations for these types.  The test verifies that 
--      explicit primitive subprogram declaration is illegal after the tagged 
--      type is frozen, and legal otherwise.
--
--
-- CHANGE HISTORY:
--      30 Mar 95   SAIC    Initial prerelease version.
--      01 Jun 95   SAIC    Modified test description per reviewers.
--      30 Jun 98   EDS     Revised types and operations used to correct
--                          unintended errors in the private part.
--!

package BDE0001 is
   type Tag_Type is tagged record
      I : Integer;
   end record;

   type Private_Tag is new Tag_Type with private;

   -- The declaration of Private_Tag above is a private extension, and does not
   -- freeze the parent type Tag_Type. The declaration of Op is therefore 
   -- legal.

   procedure Op (X : Tag_Type);                                       -- OK.

   type Tag_Type_2 is tagged record
      I : Integer;
   end record;

   type Private_Tag_2 is new Tag_Type_2 with private;

   -- The declaration of Private_Tag_2 above is a private extension, and
   -- does not freeze the parent type Tag_Type_2.  The declaration of
   -- Op_TT_2 is therefore legal.  (The full declaration of Private_Tag_2,
   -- in the private part, will freeze Tag_Type_2.)

   procedure Op_TT_2 (X : Tag_Type_2);                                  -- OK.

   type Tag_T1 is tagged record
      B : Boolean;
   end record;

   function Func return Tag_T1;

   -- type Tag_T1 is not yet frozen, so the declaration of Op_Tag_T1
   -- is legal.

   procedure Op_Tag_T1 ( X : Tag_T1 := Func );

   -- The use of Func in the default expression above does not freeze
   -- type Tag_T1, so the following declaration of Op_Tag_T1_2 is legal.

   procedure Op_Tag_T1_2 ( X : Tag_T1 );

   type New_T1 is new Tag_T1 with null record;
   function Func return New_T1;         -- Func must be overridden

   -- The declaration of New_T1 above freezes Tag_T1. The declaration of
   -- Op3 is therefore illegal. 

   procedure Op3 (X : Tag_T1);                                        -- ERROR:
                              -- Primitive declaration of a frozen tagged type.
 
   type Tag_T2 is tagged record
      S : String (1 .. 100);
   end record;
   function Func1 return Tag_T2;

   type Tag_T3 is tagged record
      I : Integer;
   end record;
   type Access_T3 is access Tag_T3;

   package BDE0001_0 is
      type New_T2 is new Tag_Type with null record;
      -- Freezes Tag_Type.
 
      -- Since Op4 is declared in a nested package, it is not a primitive
      -- subprogram of Tag_Type. Thus, even though Tag_Type has been frozen, 
      -- the declaration of Op4 is legal. 

     procedure Op4 (X : Tag_Type);                                   -- OK.
  
      ObjA : Tag_T2 := Func1;     
      -- Freezes its nominal subtype Tag_T2. 

      -- Since Func2 is declared in a nested package, it is not a primitive
      -- subprogram of Tag_T2. Thus, even though Tag_T2 has been frozen, the
      -- declaration of Func2 is legal. 

      function Func2 return Tag_T2;                                   -- OK.

      ObjB : Access_T3 := new Tag_T3;

      -- The allocator in the initialization expression for ObjB above freezes
      -- the designated subtype Tag_T3 of Access_T3. 

      -- Since Func3 and Op5 are declared in a nested package, they are not
      -- primitive subprograms of Tag_T3. Thus, even though Tag_T3 has been 
      -- frozen, the declarations of Func3 and Op5 are legal. 

      function Func3 return Tag_T3;                                   -- OK.

      procedure Op5 (X : access Tag_T3);                              -- OK.

   end BDE0001_0;   

   
   -- Tag_T2 was frozen by the declaration of ObjA in package BDE0001_0. The
   -- declaration of Func4 is therefore illegal. 
   -- Likewise, Op6 (also a primitive subprogram of Tag_T2) is illegal.

   function Func4 return Tag_T2;                                      -- ERROR:
                              -- Primitive declaration of a frozen tagged type.
 
   procedure Op6 (X : access Tag_T2);                                 -- ERROR:
                              -- Primitive declaration of a frozen tagged type.
 
   -- Tag_T3 was frozen by the allocator initializing ObjB in package 
   -- BDE0001_0. The declaration of Func5 is therefore illegal. 

   function Func5 return Tag_T3;                                      -- ERROR:
                              -- Primitive declaration of a frozen tagged type.
 
private

   type Private_Tag is new Tag_Type with null record;   -- Required completion

   type Private_Tag_2 is new Tag_Type_2 with null record;

   -- The (full) declaration of Private_Tag above freezes Tag_Type. The 
   -- declarations of Op7 and Op8 are therefore illegal. 

   procedure Op7 (X : Tag_Type_2);                                      -- ERROR:
                              -- Primitive declaration of a frozen tagged type.

   procedure Op8 (X : access Tag_Type_2);                               -- ERROR:
                              -- Primitive declaration of a frozen tagged type.

end BDE0001;
