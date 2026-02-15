-- BXE2007.A
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
--      Check that a declared Shared_Passive library unit may not contain:
--      objects that are not preelaborable, library level task object
--      declarations, protected objects with entries, access types 
--      that designate a class-wide type, access types that designate 
--      a task type, or access types that designate a protected type 
--      with entries.
--
--      Check that a declared Shared_Passive library unit may contain:
--      objects that are preelaborable, protected objects without entries, 
--      protected types with entries, and task types.
--
-- TEST DESCRIPTION:
--      This test declares one of each of the items that should not be
--      allowed and one of each of the specific items that should be
--      allowed.
--
-- APPLICABILITY CRITERIA:
--      This test applies only to implementations supporting the
--      Distribution Annex.
--
--
-- CHANGE HISTORY:
--      20 FEB 95   SAIC    Initial version
--      23 OCT 95   SAIC    Incorporated comments.
--
--!

package BXE2007 is
  pragma Shared_Passive;

  -----
  -- first, some legal declarations 
  -----

  Preelaborated_Object : Integer := 12;                              -- OK.
 
  -- protected object without entries
  protected Shared_Counter is                                        -- OK.
    procedure Increment;
  private
    The_Counter : Integer := 2;
  end Shared_Counter;

  -- protected type with entries
  protected type Resource is                                         -- OK.
    entry Sieze;
    procedure Release;
  private
    Busy : Boolean := False;
  end Resource;

  -- task types
  task type Helper is                                                -- OK.
    entry Start_Work;
  end Helper;

  -- tagged types
  type Coordinate is tagged                                          -- OK.
    record
      X, Y : Float;
    end record;

  -----
  -- now, some illegal declarations 
  -----

  Not_Preelaborable : Integer := Preelaborated_Object;               -- ERROR:
                            -- object that is not preelaborable is not allowed

  My_Helper : Helper;                                                -- ERROR:
                            -- library level task declarations are not allowed

  Printer_Resource : Resource;                                       -- ERROR:
                              -- protected object with entries are not allowed

  type Bat1 is access Coordinate'Class;                              -- ERROR:
              -- access types that designate a class-wide type are not allowed

  type Bat2 is access Helper;                                        -- ERROR:
                     -- access types that designate a task type is not allowed

  type Bat3 is access Resource;                                      -- ERROR:
                          -- access types that designate a protected type with 
                          -- entries are not allowed

end BXE2007;
