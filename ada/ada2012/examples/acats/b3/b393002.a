-- B393002.A
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
--      Check that incorrect orderings of reserved words in a tagged type 
--      declaration are flagged as illegal.
--
-- TEST DESCRIPTION:
--      These are syntax checks. 
--      The correct order of reserved words for tagged types is:
--         [ [ abstract ]  tagged ]  [ limited ] record_definition
--
--      This test checks a selection of likely mis-orderings. Illegal
--      statements have been surrounded by correct code to ease error 
--      recovery.
--
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

package B393002 is

   type Illegal_Abstract_Limit1 is abstract limited record            -- ERROR:
      Field : Integer;                        -- Missing reserved word "tagged"
   end record;

   type Abstract_Limit1 is abstract tagged limited record             -- OK.
      Field : Integer;                    
   end record;

   type Illegal_Limit1 is limited tagged record                       -- ERROR:
      Field : Boolean;                             -- "limited" before "tagged"
   end record;

   type Tagged_Limit1 is tagged limited record                        -- OK.
      Field : Boolean;                    
   end record;
     
   type Illegal_Abstract is tagged abstract record                    -- ERROR:
      Field : Integer;                            -- "tagged" before "abstract"
   end record;

   type Abstract_Tagged is abstract tagged record                     -- OK.
      Field : Integer;                    
   end record;

   type Illegal_Limit2 is limited abstract tagged record              -- ERROR:
      Field : Boolean;                           -- "limited" before "abstract"
   end record;

   type Abstract_Limit2 is abstract tagged limited record             -- OK.
      Field : Boolean;
   end record;

   type Illegal_Limit3 is tagged abstract limited record              -- ERROR:
      Field : Integer;                            -- "tagged" before "abstract"
   end record;

   type Abstract_Limit3 is abstract tagged limited record             -- OK.
      Field : Integer;
   end record;

   type Illegal_Limit4 is abstract tagged limited;                    -- ERROR:
                                                  -- Missing record definition.

   type Abstract_Limit4 is abstract tagged limited record             -- OK.
      Field : Boolean;
   end record;

end B393002;
