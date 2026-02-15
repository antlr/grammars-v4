-- BDE0006.A
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
--      type is frozen.  Check for cases where the type is frozen by:
--         - The declaration of an object of the type.
--         - The declaration of an object with a component of the type.
--         - The declaration of a record extension of the type.
--         - An expression that is an allocator, the type of which designates
--           the type.
--
-- TEST DESCRIPTION:
--
--      The test declares tagged types, type extensions, objects of those
--      types and representation clauses for these types.  The test verifies
--      that representation clause declaration is illegal after the type is
--      frozen, and legal otherwise.
--
--
-- CHANGE HISTORY:
--      19 Apr 95   SAIC    Initial prerelease version.
--      17 Apr 96   SAIC    Modified test description.  Modified representation
--                          clauses of Obj2 and Tag_T4.
--
--!

with System.Storage_Pools;
package BDE0006 is
   type Tag_T1 is tagged record
      C : Boolean;
   end record;

   Obj1 : Tag_T1;

   -- The declaration of Obj1 above freezes Tag_T1. The representation clause 
   -- is therefore illegal.  

   for Tag_T1'Size use 40;                                            -- ERROR:
                              -- Representation clause of a frozen tagged type.

   type Type_T2 is range 0 .. 10;
   type Rec_W_T2 is record
      C : Type_T2;
   end record;

   Obj2 : Rec_W_T2;   
   -- Does not cause freezing of Obj2.

   for Obj2'Size use 40;                                              -- OK.

   -- The declaration of Obj2 above freezes its nominal subtype Rec_W_T2, which
   -- in turn freezes Type_T2. The representation clause is therefore illegal.  

   for Type_T2'Alignment use 2;                                       -- ERROR:
                                     -- Representation clause of a frozen type.
                               
   type Type_T3 is delta 0.01 range -10.0 .. 10.0;
   type Arr_Of_T3 is array (1 .. 5) of Type_T3;
   type Rec3 is tagged record
      C : Arr_Of_T3;
   end record;

   Obj3 : Rec3;    

   -- The declaration of Obj3 above freezes its nominal subtype Rec3, which
   -- in turn freezes Arr_Of_T3, and thus Type_T3. Therefore, the 
   -- representation clause is illegal.  

   for Type_T3'Size use 112;                                          -- ERROR:
                                     -- Representation clause of a frozen type.
                              
   type Tag_T4 is tagged record
      S : String (1 .. 100);
   end record;
   type New_Tag_T4 is new Tag_T4 with record
      I : Integer;
   end record;

   -- The declaration of New_Tag_T4 above freezes its parent type Tag_T4.  The 
   -- representation clause is therefore illegal.  

   for Tag_T4'Alignment use 8;                                        -- ERROR:
                              -- Representation clause of a frozen tagged type.

   type Tag_T5 is tagged record
      I : Integer;
   end record;
   type Access_Tag_T5 is access Tag_T5;

   Obj5 : Access_Tag_T5 := new Tag_T5;

   -- The allocator in the initialization expression for Obj5 above freezes
   -- the designated subtype Tag_T5 of Access_Tag_T5. Therefore, the 
   -- representation clause is illegal.  

   for Tag_T5'Size use 64;                                            -- ERROR:
                              -- Representation clause of a frozen tagged type.

   type Access_Pool is access
     System.Storage_Pools.Root_Storage_Pool'Class;

   function Func return Access_Pool;

   type Access_Boolean is access Boolean;
   type New_Access_Boolean is new Access_Boolean;
   type New_New_Access_Boolean is new New_Access_Boolean;
   Obj6 : New_New_Access_Boolean := new Boolean;          

   -- The allocator in the initialization expression for Obj6 above freezes 
   -- New_Acess_Boolean and Access_Boolean. Therefore, the representation 
   -- clause is illegal.  

   for Access_Boolean'Storage_Pool use Func.all;                      -- ERROR:
                                     -- Representation clause of a frozen type.

end BDE0006;
