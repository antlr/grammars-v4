-- BXC6003.A
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
--      Check that the implementation rejects a pragma Atomic
--      when it cannot support indivisible reads or updates of
--      the object.
--      Check that the implementation rejects a pragma Atomic_Components
--      when it cannot support indivisible reads or updates of
--      the components of the array object.
--
-- TEST DESCRIPTION:
--      A pragma Atomic is applied to a very large object and an
--      array with very large components.
--
-- APPLICABILITY CRITERIA:
--      This test applies only to implementations supporting
--      the Systems Programming Annex.
--      If the implementation supports indivisible reads and
--      updates on arbitrarly large objects then this test is
--      not applicable.
--
--
-- CHANGE HISTORY:
--       8 FEB 96   SAIC    Initial release for 2.1
--
--!


-----------------------------------------------------------------------

procedure BXC6003 is
   type Big is array (1..100) of Integer;

   type Atomic_Type is array (1..100) of Float;
   pragma Atomic (Atomic_Type);                             -- ERROR:
      -- indivisible reads and updates are not possible for this type

   Atomic_Object : Big;
   pragma Atomic (Atomic_Object);                           -- ERROR:
      -- indivisible reads and updates are not possible for this type

   type Atomic_Array_Type is array (1..10) of Big;
   pragma Atomic_Components (Atomic_Array_Type);            -- ERROR:
     -- indivisible reads and updates are not possible for components
     -- of this array.

   Atomic_Array_Object : array (1..10) of Big; 
   pragma Atomic_Components (Atomic_Array_Object);          -- ERROR:
     -- indivisible reads and updates are not possible for components
     -- of this array.
begin
   null;
end BXC6003;
