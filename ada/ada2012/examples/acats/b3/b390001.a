-- B390001.A
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
--      Check that:
--      Class wide objects are required to be initialized (whether created
--      by object declaration or an allocator).  
--      Aggregates of a class wide type are required to be qualified with a
--      specific type when their expected type is class-wide.
--      Tagged private and tagged limited private require the full type to be 
--      a tagged record type.
--      The attribute 'Class is not defined for untagged types.
--      The Class attribute is defined for untagged private types whose full
--      type is tagged, but only in the private part of the package in which
--      the type is declared.
--
-- TEST DESCRIPTION:
--      This test declares tagged (limited) types, simple types, array types, 
--      a protected type, and a task type.  Verify that compiler generates 
--      errors for all cases as described in the objective.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      12 Dec 94   SAIC    Changed declaration of Private_Var1 to access type
--
--!

package B390001 is

   type Tagged_Private is tagged private;

   type Tagged_Limit_Private is tagged limited private;

   type Private_Type is private;

   function Init_Func return Private_Type;

   type Private_Limit is limited private;

   type Visible_Record is 
     record
        Int_Comp : integer;
   end record;

   type Visible_Array is array (1 .. 2) of integer;

   task type Visible_Task is
      entry Do_Nothing;
   end Visible_Task;

   type Visible_Tagged is tagged 
     record
        Comp1 : boolean;
     end record;

   type Visible_Extended is new Visible_Tagged with
     record
        Comp2 : integer;
     end record;

   type Next_Extended is new Visible_Extended with
     record
        Comp3 : boolean;
     end record;

   type Tagged_Ptr is access Visible_Tagged;


   -------------------------------------------------------------------------
   Tagged_Var1   : Visible_Tagged'Class;                            -- ERROR: 
                                   -- Class type objects must be initialized.

   Tagged_Var2   : Tagged_Ptr := new Visible_Tagged'Class;          -- ERROR: 
                                   -- Class type objects must be initialized. 

   -------------------------------------------------------------------------
   Tagged_Var3   : Visible_Tagged'Class 
                 := (Comp1 => true, Comp2 => 5, Comp3 => false);    -- ERROR: 
                                   -- Must be qualified with a specific type.

   -------------------------------------------------------------------------
   Untagged_Var1 : Visible_Record'Class;                            -- ERROR:
                                   -- Illegal use of 'Class for record type. 

   Untagged_Var2 : Visible_Array'Class;                             -- ERROR:
                                   -- Illegal use of 'Class for array type. 

   Untagged_Var3 : Visible_Task'Class;                              -- ERROR:
                                   -- Illegal use of 'Class for task type. 

   -------------------------------------------------------------------------
   Not_Vis_Var1  : Private_Type'Class;                              -- ERROR:
                                                       -- 'Class not visible.

   Not_Vis_Var2  : Private_Limit'Class;                             -- ERROR:
                                                       -- 'Class not visible.
private

   type Private_Acc is access Private_Type'Class;                   -- OK.

   procedure Private_Proc (P : Private_Type'Class);                 -- OK.

   type Private_Type is tagged null record;

   type Private_Limit is tagged limited null record;

   Private_Var2 : Private_Type;

   Private_Var3 : Private_Type'Class := Private_Var2;               -- OK.

   -------------------------------------------------------------------------
   type Tagged_Private is null record;                              -- ERROR:
                                   -- Full type must be tagged record type.

   type Tagged_Limit_Private is null record;                        -- ERROR:
                                   -- Full type must be tagged record type.
end B390001;

-- No bodies required for B390001.
