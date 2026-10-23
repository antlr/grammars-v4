-- BC51017.A
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
--      Check that alternative orderings of reserved words in a formal private
--      type declaration are illegal.
--
-- TEST DESCRIPTION:
--      The correct order of reserved words is:
--         [ [ abstract ]  tagged ]  [ limited ]  private
--
--      This test checks a selection of likely mis-orderings. Illegal
--      statements have been surrounded by correct code to ease error
--      recovery.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

package BC51017 is

   type Abstract_Type is abstract tagged null record;


   generic
      type OK is tagged limited private;                              -- OK.
      type FP is limited tagged private;                              -- ERROR:
                                       -- "Tagged" must occur before "limited."
      type DT is (<>);
   package One is
      function A_OK return OK;
   end One;


   type Discrete is range 0 .. 100;
   Ten : constant Discrete := 10;


   generic
      type OK is abstract tagged private;                             -- OK.
      type FP is tagged abstract private;                             -- ERROR:
                                       -- "Tagged" must occur before "limited."
      FO : in Discrete;
   package Two is
      function A_OK return OK is abstract;
   end Two;


   type DD is new Discrete;


   generic
      type OK is tagged private;                                      -- OK.
      type FP is private tagged;                                      -- ERROR:
                                       -- "Tagged" must occur before "private."
      type AR is array (DD) of OK;
   procedure Three (P: in OK);


   type Acc_Class is access Abstract_Type'Class;

   type Node is new Abstract_Type with record
     Next : Acc_Class;
   end record;


   generic
      type OK is abstract tagged private;                             -- OK.
      type FP is private abstract tagged;                             -- ERROR:
                              -- "Abstract tagged" must occur before "private."
      type DP is access Discrete;
   package Four is
      function A_OK return OK is abstract;
   end Four;


   type Private_Type is private;


   generic
      type OK is tagged limited private;                              -- OK.
      type FP is tagged limited;                                      -- ERROR:
                                            -- Missing reserved word "private."
      type IT is range <>;
   function Five (A: OK) return OK;

private
   type Private_Type is (G1, G2, G3, G4, G5);
end BC51017;
