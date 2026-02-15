-- B4310071.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
--
-- OBJECTIVE:
--      See B4310070.A.
--
-- TEST DESCRIPTION:
--      See B4310070.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         B4310070.A
--      -> B4310071.A
--         B4310072.A
--
-- PASS/FAIL CRITERIA:
--      See B4310070.A.
--
-- CHANGE HISTORY:
--      30 Dec 19   RLB     Created test.
--
--!

package body B431007.Child2 is

   type Ext7 is new B431007.Child1.Ext1 with record
      C3 : Integer;
   end record;

   type Ext8 is new Ext3 with record
      C4 : Integer;
   end record;

   type Ext9 is new Ext4 with record
      C4 : Integer;
   end record;

   type ExtA is new Ext6 with record
      C3 : Integer;
   end record;

   X01 : Ext3 := (others => 0);                             -- ERROR: {18;1}
      -- Ext1 is a private type here.

   X02 : Ext3 := (C2 | C3 => 0, others => 0);               -- ERROR: {18;1}
      -- Ext1 is a private type here; C2 is not visible here.

   X03 : Ext3 := (C1 | C2 | C3 => 0);                       -- ERROR: {18;1}
      -- Ext1 is a private type here; C1 and C2 are not visible here.


   X11 : Ext4 := (others => 0);                             -- ERROR: {18;1}
      -- Ext2 is a private type here.

   X12 : Ext4 := (C1 | C2 | C3 => 0);                       -- ERROR: {18;1}
      -- Ext2 is a private type here; C1 and C2 are not visible here.


   X21 : Ext5 := (others => 0);                             -- OK.    {18;1}
      -- Root and Ext5 are record types here.

   X22 : Ext5 := (C1 | C2 => 0);                            -- OK.    {18;1}
      -- Root and Ext5 are record types here.


   X31 : Ext6 := (others => 0);                             -- OK.    {18;1}
      -- Root and Ext6 are record types here.

   X32 : Ext6 := (C1 | C2 => 0);                            -- OK.    {18;1}
      -- Root and Ext6 are record types here.


   X41 : Ext7 := (others => 0);                             -- ERROR: {18;1}
      -- Ext1 is a private type here.


   X51 : Ext8 := (others => 0);                             -- ERROR: {18;1}
      -- Ext1 is a private type here.

   X52 : Ext8 := (C3 | C4 => 0, others => 0);               -- ERROR: {18;1}
      -- Ext1 is a private type here.

   X53 : Ext8 := (C1 | C2 | C3 | C4 => 0);                  -- ERROR: {18;1}
      -- Ext1 is a private type here; C1 and C2 are not visible here.


   X61 : Ext9 := (others => 0);                             -- ERROR: {18;1}
      -- Ext2 is a private type here.

   X62 : Ext9 := (C3 | C4 => 0, others => 0);               -- ERROR: {18;1}
      -- Ext2 is a private type here.

   X63 : Ext9 := (C1 | C2 | C3 | C4 => 0);                  -- ERROR: {18;1}
      -- Ext2 is a private type here; C1 and C2 are not visible here.



   X71 : ExtA := (others => 0);                             -- OK.    {18;1}
      -- Root, Ext6, and ExtA are record types here.

   X72 : ExtA := (C3 => 0, others => 0);                    -- OK.    {18;1}
      -- Root, Ext6, and ExtA are record types here.

   X73 : ExtA := (C1 | C2 | C3 => 0);                       -- OK.    {18;1}
      -- Root, Ext6, and ExtA are record types here.

   -- Basic cases:
   X81 : Root := (others => 0);                             -- OK.    {18;1}

   X82 : B431007.Child1.Ext1 := (others => 0);              -- ERROR: {33;1}

   X83 : B431007_Unrelated.Ext2 := (others => 0);           -- ERROR: {36;1}

   procedure Force_Body is
   begin
      null; -- Force this package to have (and allow) a body.
   end Force_Body;

end B431007.Child2;


