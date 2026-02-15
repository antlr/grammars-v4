-- BXC6A04.A
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
--     Check that if a pragma Volatile, Volatile_Components, Atomic, or
--     Atomic_Components applies to a stand-alone constant object, then a
--     pragma Import must also apply to it. Check that if a stand-alone
--     constant object is atomic or volatile solely because of its type, a
--     pragma Import need not apply to it.
--    
-- TEST DESCRIPTION:
--     The test applies each pragma to various stand-alone constant objects
--     and verifies that the constant is rejected if a pragma Import does
--     not apply to it. In addition, the test verifies that if the constant
--     object is atomic or volatile solely because of its type (i.e., none
--     of the pragmas applies to it), then a pragma Import is not required.
--
-- TEST FILES:
--      This test consists of the following files:
--
--         FXC6A00.A
--      -> BXC6A04.A
--
-- APPLICABILITY CRITERIA:
--      This test is only applicable for implementations validating the
--      Systems Programming Annex.
--
--      In addition, this test is only applicable for implementations which
--      support stand-alone constant objects as arguments in a pragma Import.
--
-- PASS/FAIL CRITERIA:
--      An implementation may reject one or more of the pragmas Atomic and
--      Atomic_Components in this test if it cannot support indivisible reads
--      and updates for the specified object or type. If a pragma is rejected,
--      certain ERROR cases (as noted in the body of the test) need not be
--      reported as errors.
--
--
-- CHANGE HISTORY:
--      16 Feb 96   SAIC    Initial version for ACVC 2.1.
--      21 Feb 96   SAIC    Added Pass/Fail criteria.
--      25 Aug 96   SAIC    Changed Pass/Fail criterion and comments following
--                          certain error cases. Reworded comments following
--                          pragmas. Corrected prologue.
--
--!

with FXC6A00;
package BXC6A04 is

   use type FXC6A00.Roman;      -- Makes enumeration literals directly visible.

   type Atomic_Type is range -128 .. 127;
   for Atomic_Type'Size use 8;
   pragma Atomic (Atomic_Type);                                -- N/A => ERROR.
        -- Implementation must reject this pragma if it does not support
        -- indivisible read/write for objects of this type. 

   type Roman_Array is array (1 .. 3) of FXC6A00.Roman;
   pragma Atomic_Components (Roman_Array);                     -- N/A => ERROR.
          -- Implementation must reject this pragma if it does not support
          -- indivisible read/write for Roman objects.  


   -- The following four cases are OK because the constants are atomic solely
   -- because of their types:

   Zero  : constant Atomic_Type := 0;                                 -- OK.
   Tag1  : constant FXC6A00.Volatile_Tagged;                          -- OK.
   RArr  : constant Roman_Array;                                      -- OK.
   VArr  : constant FXC6A00.Array_Type := (others => (C => 0));       -- OK.



   Atom0 : constant Atomic_Type;                                      -- ERROR:
                                       -- Pragma Import must apply to constant.
   pragma Atomic (Atom0);

   Atom1 : constant Boolean;                                          -- OK.
   pragma Import (Ada, Atom1);
   pragma Atomic (Atom1);                                      -- N/A => ERROR.
          -- Implementation must reject this pragma if it does not support
          -- indivisible read/write for Boolean objects.  


   Vol0  : constant FXC6A00.Record_Type := (C => "Hi");               -- ERROR:
                                       -- Pragma Import must apply to constant.
   pragma Volatile (Vol0);

   Vol1  : constant FXC6A00.Record_Type;                              -- OK.
   pragma Import (Ada, Vol1);
   pragma Volatile (Vol1);
   

   ACmp0 : constant array (1 .. 2) of Boolean := (True, True);        -- ERROR:
                                       -- Pragma Import must apply to constant.
                      -- NOTE: if pragma Atomic_Components rejected for ACmp0,
                      -- this case need NOT be flagged as an error.
                               

   pragma Atomic_Components (ACmp0);                           -- N/A => ERROR.
          -- Implementation must reject this pragma if it does not support
          -- indivisible read/write for Boolean objects.  

   ACmp1 : constant array (1 .. 5) of Boolean;                        -- OK.
   pragma Import (Ada, ACmp1);
   pragma Atomic_Components (ACmp1);                           -- N/A => ERROR.
          -- Implementation must reject this pragma if it does not support
          -- indivisible read/write for Boolean objects.  

   

   VCmp0 : constant array (1 .. 2) of Float := (-1.0, 0.0);           -- ERROR:
                                       -- Pragma Import must apply to constant.

   pragma Volatile_Components (VCmp0);

   VCmp1 : constant array (1 .. 2) of Float;                          -- OK.
   pragma Import (Ada, VCmp1);
   pragma Volatile_Components (VCmp1);
   
   
private

   Tag1  : constant FXC6A00.Volatile_Tagged := (50, 'L');
   RArr  : constant Roman_Array := ('X', 'X', 'V');

   Atom0 : constant Atomic_Type := 8;                        -- OPTIONAL ERROR:

end BXC6A04;
