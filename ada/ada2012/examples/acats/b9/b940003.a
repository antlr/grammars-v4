-- B940003.A
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
--      Check that protected declarations (in a normal procedure) require
--      completion by a protected body and vice versa.
--
-- TEST DESCRIPTION:
--      Instances of a protected object are defined in separate declarative
--      regions.  The first two each have a protected object which has a
--      specification and a body defined but the individual components are
--      missing either their specifications or their bodies.  The second two
--      are identical to the first two except that the protected object is
--      declared as a type.  In the next two, either the whole of the
--      specification or the body of the complete protected object is missing
--      and in the last case the body of an object declared as a type is
--      missing
--
-- PASS/FAIL CRITERIA:
--      The test contains several lines marked POSSIBLE ERROR: [Setn].
--      For each value of n, the implementation must detect one or more of
--      these possible errors. For instance, an error must be detected on
--      at least one of the lines labeled POSSIBLE ERROR: [Set1] for an
--      implementation to pass.
--
--      The ERROR (optional):  An implementation must report that
--      Whole_Object_No_Body and Whole_Object_Type_No_Body are both protected
--      object declarations without their associated bodies.  An implementation
--      may additionally report that Might_Not_Be_Flagged is a subprogram
--      specification within these objects which is missing the associated
--      body.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      03 Feb 17   RLB     Added location indicators to allow the errors
--                          to be reported anywhere in the appropriate body.
--                          Eliminated the bogus discussion about needing a
--                          specification for a subprogram declared in a
--                          protected body.
--
--!


procedure B940003 is

begin

   --==============================================================

 -- Check for incomplete specifications within the object

   declare

      protected Object_No_Internal_Bodies is   -- POSSIBLE ERROR: [Set1] {7}
         function  Func_No_Body return Boolean;-- POSSIBLE ERROR: [Set1] {10}
         procedure Proc_No_Body;               -- POSSIBLE ERROR: [Set1] {10}
         entry     Entry_No_Body;              -- POSSIBLE ERROR: [Set1] {10}
         --=======
         -- one valid subprogram to avoid null Object body
         function Good_One return boolean;     -- OK. {10}
      private
         function  Func_No_Body_in_Private
                                return Boolean;-- POSSIBLE ERROR: [Set1] {1:10}
         procedure Proc_No_Body_in_Private;    -- POSSIBLE ERROR: [Set1] {10}
         entry     Entry_No_Body_in_Private;   -- POSSIBLE ERROR: [Set1] {10}
      end Object_No_Internal_Bodies;

      protected body Object_No_Internal_Bodies is
         -- internal bodies missing
         --=======
         function Good_One return boolean is
         begin
            return true;
         end Good_One;
      end Object_No_Internal_Bodies;           -- POSSIBLE ERROR: [Set1] {7:7}
                                               -- Missing bodies for:
                                               --      Func_No_Body
                                               --      Proc_No_Body
                                               --      Entry_No_Body
                                               --      Func_No_Body_in_Private
                                               --      Proc_No_Body_in_Private
                                               --      Entry_No_Body_in_Private


   begin
      null;
   end;

   --==============================================================

   -- Check for bodies with no specifications within the object

   declare

      protected Object_No_Internal_Specs is
         -- one valid subprogram to avoid null Object spec
         function Good_One return boolean;
      end Object_No_Internal_Specs;


      protected body Object_No_Internal_Specs is
         -- an entry body must have a specification
         entry Entry_No_Spec when true is                  -- ERROR: {10}
                                                      -- Missing Specification
         begin
            null;
         end Entry_No_Spec;
         -- Note: there are no corresponding subprograms with missing
         --       specifications - they just are just treated as local
         --       subprograms.
         --=======
         function Good_One return boolean is               -- OK. {10}
         begin
            return true;
         end Good_One;
      end Object_No_Internal_Specs;

   begin
      null;
   end;

   --==============================================================

   -- Check for incomplete specifications within the object when the
   -- object is declared as a type

   declare

      protected type
             Object_Type_No_Internal_Bodies is -- POSSIBLE ERROR: [Set2] {1:7}
         function  Func_No_Body return Boolean;-- POSSIBLE ERROR: [Set2] {10}
         procedure Proc_No_Body;               -- POSSIBLE ERROR: [Set2] {10}
         entry     Entry_No_Body;              -- POSSIBLE ERROR: [Set2] {10}
         --=======
         -- one valid subprogram to avoid null Object body
         function Good_One return boolean;     -- OK.
      private
         function  Func_No_Body_in_Private
                               return Boolean; -- POSSIBLE ERROR: [Set2] {1:10}
         procedure Proc_No_Body_in_Private;    -- POSSIBLE ERROR: [Set2] {10}
         entry     Entry_No_Body_in_Private;   -- POSSIBLE ERROR: [Set2] {10}
      end Object_Type_No_Internal_Bodies;

      protected body Object_Type_No_Internal_Bodies is
         -- internal bodies missing
         --=======
         function Good_One return boolean is
         begin
            return true;
         end Good_One;
      end Object_Type_No_Internal_Bodies;      -- POSSIBLE ERROR: [Set2] {7:7}
                                              -- Missing bodies for:
                                              --      Func_No_Body
                                              --      Proc_No_Body
                                              --      Entry_No_Body
                                              --      Func_No_Body_in_Private
                                              --      Proc_No_Body_in_Private
                                              --      Entry_No_Body_in_Private


   begin
      null;
   end;

   --==============================================================

   -- Check for bodies with no specifications within the object when
   -- the object is declared as a type

   declare

      protected Object_Type_No_Internal_Specs is
         -- one valid subprogram to avoid null Object spec
         function Good_One return boolean;
      end Object_Type_No_Internal_Specs;


      protected body Object_Type_No_Internal_Specs is
         -- an entry body must have a specification
         entry Entry_No_Spec when true is                  -- ERROR: {10}
                                                      -- Missing Specification
         begin
            null;
         end Entry_No_Spec;
         -- Note: there are no corresponding subprograms with missing
         --       specifications - they are just treated as local
         --       subprograms.
         --=======
         function Good_One return boolean is               -- OK. {10}
         begin
            return true;
         end Good_One;
      end Object_Type_No_Internal_Specs;

   begin
      null;
   end;

   --==============================================================

   -- Check the object (as a whole) with missing body

   declare

      protected Whole_Object_No_Body is
         function Ought_Not_Be_Flagged return boolean;
      end Whole_Object_No_Body;

   begin                                                   -- ERROR: {4:7}
                                      -- Missing body for Whole_Object_No_Body
                                      -- (optional) missing body for
                                      -- Might_Not_Be_Flagged
      null;
   end;

   --==============================================================

   -- Check the object (as a whole) with missing spec.

   declare

      protected body Whole_Object_No_Spec is               -- ERROR: {7}
                                                        -- No specification
         function Ought_Not_Be_Flagged return boolean is

         begin
            return true;
         end Ought_Not_Be_Flagged;
      end Whole_Object_No_Spec;

   begin
      null;
   end;

   --==============================================================

   -- Check the object (as a whole) with missing body when the object is
   -- declared as a type

   declare

      protected type Whole_Object_Type_No_Body is
         function Might_Not_Be_Flagged return boolean;
      end Whole_Object_Type_No_Body;

   begin                                                   -- ERROR: {4:7}
                                                  -- Missing body for
                                                  -- Whole_Object_Type_No_Body
                                                  -- (optional) missing body
                                                  -- for Might_Not_Be_Flagged
          null;
   end;

   --========================

   -- Note: there is no corresponding Body without a spec as that
   --       is covered in Whole_Object_No_Spec;
   --==============================================================

   null;

end B940003;
