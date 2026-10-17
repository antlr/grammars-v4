-- BC70009.A
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
--      Check that an actual instance of a generic formal package is rejected
--      if its actuals do not match the corresponding actuals in the formal
--      package actual part. Specifically, check that, for formal subprograms
--      and packages, the actuals must statically denote the same entity.
--
-- TEST DESCRIPTION:
--      Declare templates for formal packages which declare formal subprograms
--      and formal packages. Declare generics containing formal package
--      declarations, where each formal package has an explicit actual part.
--
--      Instantiate the template packages with the same and different
--      entities (both directly- and re-named) and pass them as actuals to the
--      formal packages. Verify that all cases in which the actuals of these
--      instances do not statically denote the same entities as the actuals of
--      the corresponding formal package are illegal.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

package BC70009 is

--
-- CASE A: Formal subprograms
--

   -- Support declarations:

   subtype My_Float is Float range 0.0 .. 100.0;

   function Reciprocal          (Item : Float)    return Float;
   function Positive_Reciprocal (Item : My_Float) return My_Float;

   function Inverse (I : Float)    return Float    renames Reciprocal;
   function Pos_Inv (I : My_Float) return My_Float renames Positive_Reciprocal;


   -- Template for formal package:

   generic                                          -- Template declaring a
      with function Invert (Value : Float)          -- formal subprogram.
        return Float;
   package Subprogram_Template is end;


   -- Generic declaring a formal package:

   generic                                          -- Formal package actual is
      with package Formal_Package is new            -- the function Reciprocal.
        Subprogram_Template (Reciprocal);
   package Subprogram_Function is end;


   -- Instances to be passed as actuals to the formal package:

   package Subprogram_Instance_Same_Function        -- Instance actual is the
     is new Subprogram_Template (Reciprocal);       -- same as the formal
                                                    -- package actual.

   package Subprogram_Instance_Diff_Function        -- Instance actual is mode
     is new Subprogram_Template                     -- conformant with the
       (Positive_Reciprocal);                       -- formal package actual.

   package Subprogram_Instance_Rename_Same_Function -- Instance actual is a
     is new Subprogram_Template (Inverse);          -- rename of the formal
                                                    -- package actual.

   package Subprogram_Instance_Rename_Diff_Function -- Instance actual is a
     is new Subprogram_Template (Pos_Inv);          -- rename of the mode
                                                    -- conformant function.



   -- Instances of generics declaring formal packages:

   package Subprogram_Direct_Name_Same_Function is new
     Subprogram_Function (Subprogram_Instance_Same_Function);         -- OK.

   package Subprogram_Direct_Name_Diff_Function is new
     Subprogram_Function (Subprogram_Instance_Diff_Function);         -- ERROR:
                                    -- Actuals do not denote the same function.

   package Subprogram_Rename_Same_Function is new
     Subprogram_Function (Subprogram_Instance_Rename_Same_Function);  -- OK.

   package Subprogram_Rename_Diff_Function is new
     Subprogram_Function (Subprogram_Instance_Rename_Diff_Function);  -- ERROR:
                                    -- Actuals do not denote the same function.

--
-- CASE B: Formal packages
--

   -- Support declarations:

   package Sub_Same_Func renames Subprogram_Instance_Same_Function;
   package Sub_Diff_Func renames Subprogram_Instance_Diff_Function;


   -- Template for formal package:

   generic                                          -- Template declaring a
      with package Form_Pack is new                 -- formal package.
        Subprogram_Template (<>);
   package Package_Template is end;


   -- Generic declaring a formal package:

   generic                                          -- Formal package actual is
      with package Formal_Package is new            -- the package instance
        Package_Template                            -- Subprogram_Instance_
           (Subprogram_Instance_Same_Function);     -- Same_Function.
   package Package_Check is end;


   -- Instances to be passed as actuals to the formal package:

   package Package_Instance_Same_Package            -- Instance actual is the
     is new Package_Template                        -- same as the formal
       (Subprogram_Instance_Same_Function);         -- package actual.

   package Package_Instance_Diff_Package            -- Instance actual is NOT
     is new Package_Template                        -- the same as the formal
       (Subprogram_Instance_Diff_Function);         -- package actual.

   package Package_Instance_Rename_Same_Package     -- Instance actual is a
     is new Package_Template (Sub_Same_Func);       -- rename of the formal
                                                    -- package actual.

   package Package_Instance_Rename_Diff_Package     -- Instance actual is a
     is new Package_Template (Sub_Diff_Func);       -- rename of a different
                                                    -- package.



   -- Instances of generics declaring formal packages:

   package Package_Direct_Name_Same_Package is new
     Package_Check (Package_Instance_Same_Package);                   -- OK.

   package Package_Direct_Name_Diff_Package is new
     Package_Check (Package_Instance_Diff_Package);                   -- ERROR:
                                     -- Actuals do not denote the same package.

   package Package_Rename_Same_Package is new
     Package_Check (Package_Instance_Rename_Same_Package);            -- OK.

   package Package_Rename_Diff_Package is new
     Package_Check (Package_Instance_Rename_Diff_Package);            -- ERROR:
                                     -- Actuals do not denote the same package.


end BC70009;
