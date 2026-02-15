-- B730001.A
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
--      Full type of a tagged private type must be a tagged type.  This means 
--      that the full type must either be declared using a tagged record 
--      definition, or else derived from some other tagged type, in which 
--      case it must include a record_extension_part.
--      Full type of a nonlimited tagged private type must be a nonlimited 
--      tagged type. 
--      Full type of a limited tagged private type must be a limited tagged 
--      type. 
--      A tagged record type must be a limited type if one of its record 
--      components is limited.
--      A record extension must be extended from a limited parent type if one 
--      of its record components is limited.
--
-- TEST DESCRIPTION:
--      This test declares private tagged (limited) types. Then it completes
--      the full types in the private part.  Verify that compiler generates 
--      errors for all cases as described in the objective.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

package B730001 is

   type Tagged_Visible is tagged null record;

   type Limit_Private is limited private;

   type Tagged_Nonlimit_Private is tagged private;

   type Tagged_Limit_Private is tagged limited private;

   type Tagged_Private_1 is tagged private;

   type Tagged_Private_2 is tagged private;

   type Tagged_Private_3 is tagged private;

   type Tagged_Private_4 is tagged private;

   type Tagged_Private_5 is tagged private;

   type Tagged_Pri_Ext is new Tagged_Visible with private;

   -------------------------------------------------------------------------
   type Tagged_Limit_Record is tagged
     record                                  
        First_Comp : Tagged_Visible;
        Next_Comp  : Limit_Private;                                 -- ERROR:
     end record;                             -- Type of component is limited.

   type Other_Limit_Record is tagged
     limited record                                                 -- OK
        First_Comp : Tagged_Visible;
        Next_Comp  : Limit_Private;
     end record;

   type Tagged_Nonlimit_Ext is new Tagged_Visible with 
     record                                                         
        New_Comp   : Limit_Private;                                 -- ERROR:
     end record;                                     -- Parent is nonlimited.
     

private

   type Limit_Private is new Integer;

   type Tagged_Nonlimit_Private is tagged limited null record;      -- ERROR:
                          -- Full type must be nonlimited tagged record type.

   type Tagged_Limit_Private is tagged null record;                 -- ERROR:
                           -- Full type must be limited tagged record type.

   type Tagged_Private_1 is null record;                            -- ERROR:
                                     -- Full type must be tagged record type.

   type Tagged_Private_2 is new Tagged_Visible with null record;    -- OK

   type Tagged_Private_3 is record                                  -- ERROR:
      Some_Comp : Integer;           -- Full type must be tagged record type.
   end record;

   type Tagged_Private_4 is tagged null record;                     -- OK

   type Tagged_Private_5 is new Tagged_Private_4 with null record;  -- OK

   type Tagged_Pri_Ext is new Tagged_Visible;                       -- ERROR:
                                -- Full type must include a record extension.

end B730001;

-- No body allowed for B730001.
