-- B392001.A
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
--      Check that a default_expression for a controlling formal parameter
--      of a dispatching operation may not be statically tagged.
--
-- TEST DESCRIPTION:
--      This test declares a tagged type with some primitive operations.
--      Verify that compiler generates errors when a tag-determinate expression
--      is used as a default expression for a controlling formal parameter of a
--      dispatching operation.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      23 Mar 07   RLB     Updated objective to remove part about access
--                          parameter defaults, and added test cases for
--                          controlling access results.
--      18 Aug 07   RLB     Fixed declaration of Proc20 to fix type error.
--
--!

package B392001 is

   type Tagged_Type is tagged
     record
        First_Component : Boolean;
     end record;

   type Tagged_Ptr is access all Tagged_Type;

   function No_Parm return Tagged_Type;
   function One_Parm (P : Tagged_Type) return Tagged_Type;
   function Two_Parms (P1, P2 : Tagged_Type) return Tagged_Type;
   function CW_One_Parm (P : Tagged_Type'Class) return Tagged_Type;
   function CW_Two_Parms (P1, P2 : Tagged_Type'Class) return Tagged_Type;

   function Acc_One_Parm (P : Tagged_Type) return not null access Tagged_Type;

   procedure Proc1 (T : Tagged_Type'Class := No_Parm);               -- OK.
                                     -- Not a controlling formal parameter.

   ---------------------------------------------------------------------
   Deferred_Cons : constant Tagged_Type;

   Acc_Const : constant Tagged_Ptr;

   type Descendant is new Tagged_Type with private;

   Var_Desc_Type : constant Descendant;

   procedure Proc2 (T : Tagged_Type := Deferred_Cons);               -- ERROR:
                             -- Default expression may not be a specific type.

   procedure Proc3 (T : Tagged_Type'Class := Deferred_Cons);         -- OK.
                                     -- Not a controlling formal parameter.

   procedure Proc4 (T : Descendant := Var_Desc_Type);                -- ERROR:
                             -- Default expression may not be a specific type.

   procedure Proc5 (T : Tagged_Type'Class := Var_Desc_Type);         -- OK.
                                     -- Not a controlling formal parameter.

   ----------------------------------------------------------------
   procedure Proc6 (T : Tagged_Type
                      := CW_One_Parm (Deferred_Cons));               -- OK.
                       -- CW_One_Parm (Deferred_Cons) is tag indeterminate.

   procedure Proc7 (T : Tagged_Type := One_Parm (Deferred_Cons));    -- ERROR:
                                -- One_Parm (Deferred_Cons) statically tagged.

   procedure Proc8 (T : Tagged_Type'Class
                      := One_Parm (Deferred_Cons));                  -- OK.
                                     -- Not a controlling formal parameter.

   procedure Proc9 (T : Tagged_Type
     := CW_Two_Parms (Deferred_Cons, Deferred_Cons));                -- OK.
                                           -- Default is tag indeterminate.

   procedure Proc10 (T : Tagged_Type
                       := Two_Parms (Deferred_Cons, Deferred_Cons)); -- ERROR:
                                              -- Default is statically tagged.

   procedure Proc11 (T : Tagged_Type
     := Two_Parms (One_Parm (Deferred_Cons), Deferred_Cons));        -- ERROR:
                                              -- Default is statically tagged.

   procedure Proc12 (T : Tagged_Type := One_Parm (No_Parm));         -- OK.
                                -- One_Parm (No_Parm) is tag indeterminate.

   procedure Proc13 (T : Tagged_Type :=
                     Two_Parms (No_Parm, One_Parm (No_Parm)));       -- OK.
           -- Two_Parms (No_Parm, One_Parm (No_Parm)) is tag indeterminate.

   ---------------------------------------------------------------------
   procedure Proc14 (T : access Tagged_Type := new Tagged_Type);     -- ERROR:
                                              -- Default is statically tagged.

   procedure Proc15 (T : access Tagged_Type := Acc_One_Parm (No_Parm));-- OK.
                              -- Acc_One_Parm (No_Parm) is tag indeterminate.

   procedure Proc16 (T : access Tagged_Type :=
                            Acc_One_Parm (Deferred_Cons));           -- ERROR:
                                              -- Default is statically tagged.

   procedure Proc17 (T : access Tagged_Type := Acc_Const);           -- ERROR:
                                              -- Default is statically tagged.

   procedure Proc18 (T : access Tagged_Type'Class :=
                            Acc_One_Parm (Deferred_Cons));           -- OK.
                                     -- Not a controlling formal parameter.

   procedure Proc19 (T : Tagged_Ptr := new Tagged_Type);             -- OK.

   procedure Proc20 (T : Tagged_Ptr := Tagged_Ptr(
                                Acc_One_Parm (Deferred_Cons)));      -- OK.
                                     -- Not a controlling formal parameter.

private

   Deferred_Cons : constant Tagged_Type := (First_Component => False);
   type Descendant is new Tagged_Type with
     record
        Added_Component : Integer;
     end record;
   -- Need to override No_Parm, CW_One_Parm, One_Parm, Two_Parms,
   -- CW_Two_Parms, and Acc_One_Parm.
   function No_Parm return Descendant;
   function One_Parm (P : Descendant) return Descendant;
   function Two_Parms (P1, P2 : Descendant) return Descendant;
   function CW_One_Parm (P : Tagged_Type'Class) return Descendant;
   function CW_Two_Parms (P1, P2 : Tagged_Type'Class) return Descendant;
   function Acc_One_Parm (P : Descendant) return not null access Descendant;

   Var_Desc_Type  : constant Descendant := (First_Component => True,
                                            Added_Component => 1);
   Acc_Const : constant Tagged_Ptr :=
     new Tagged_Type'(First_Component => False);

end B392001;
