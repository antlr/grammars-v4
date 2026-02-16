-- B4310072.A
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
--         B4310071.A
--      -> B4310072.A
--
-- PASS/FAIL CRITERIA:
--      See B4310070.A.
--
-- CHANGE HISTORY:
--      30 Dec 19   RLB     Created test.
--
--!

with B431007_Unrelated;
with B431007.Child1;
with B431007.Child2;
package body B431007 is

   type ExtB is new B431007.Child1.Ext1 with record
      C3 : Integer;
   end record;

   type ExtC is new B431007.Child2.Ext3 with record
      C4 : Integer;
   end record;

   type ExtD is new B431007.Child2.Ext4 with record
      C4 : Integer;
   end record;

   type ExtE is new B431007.Child2.Ext6 with record
      C3 : Integer;
   end record;

   X01 : B431007.Child2.Ext3 := (others => 0);              -- ERROR: {33;1}
      -- Ext1 is a private type here.

   X02 : B431007.Child2.Ext3 := (C2 | C3 => 0, others => 0);-- ERROR: {33;1}
      -- Ext1 is a private type here; C2 is not visible here.

   X03 : B431007.Child2.Ext3 := (C1 | C2 | C3 => 0);        -- ERROR: {33;1}
      -- Ext1 is a private type here; C1 and C2 are not visible here.

   X11 : B431007.Child2.Ext4 := (others => 0);              -- ERROR: {33;1}
      -- Ext2 is a private type here.

   X12 : B431007.Child2.Ext4 := (C1 | C2 | C3 => 0);        -- ERROR: {33;1}
      -- Ext2 is a private type here; C1 and C2 are not visible here.


   X21 : B431007.Child2.Ext5 := (others => 0);              -- ERROR: {33;1}
      -- Ext5 is a private type here.

   X22 : B431007.Child2.Ext5 := (C1 | C2 => 0);             -- ERROR: {33;1}
      -- Ext5 is a private type here.


   X31 : B431007.Child2.Ext6 := (others => 0);              -- ERROR: {33;1}
      -- Root and Ext6 are record types here, however, the
      -- view of Root from Ext6 is that of a partial view. This is
      -- arguably different than the result for Ada 2005. (See AI05-0115-1
      -- for details.) Note that component C1 is not visible here, and it
      -- makes no sense to allow aggregates for invisible components.

   X32 : B431007.Child2.Ext6 := (C1 | C2 => 0);             -- ERROR: {33;1}
      -- Root and Ext6 are record types here, but C1 is not visible; also
      -- see above.


   X41 : ExtB := (others => 0);                             -- ERROR: {18;1}
      -- Ext1 is a private type here.


   X51 : ExtC := (others => 0);                             -- ERROR: {18;1}
      -- Ext1 is a private type here.

   X52 : ExtC := (C3 | C4 => 0, others => 0);               -- ERROR: {18;1}
      -- Ext1 is a private type here.

   X53 : ExtC := (C1 | C2 | C3 | C4 => 0);                  -- ERROR: {18;1}
      -- Ext1 is a private type here; C1 and C2 are not visible here.


   X61 : ExtD := (others => 0);                             -- ERROR: {18;1}
      -- Ext2 is a private type here.

   X62 : ExtD := (C3 | C4 => 0, others => 0);               -- ERROR: {18;1}
      -- Ext2 is a private type here.

   X63 : ExtD := (C1 | C2 | C3 | C4 => 0);                  -- ERROR: {18;1}
      -- Ext2 is a private type here; C1 and C2 are not visible here.


   X71 : ExtE := (others => 0);                             -- ERROR: {18;1}
      -- Root, Ext6, and ExtA are record types here, however, the
      -- view of Root from Ext6 is that of a partial view. This is
      -- arguably different than the result for Ada 2005. (See AI05-0115-1
      -- for details.) Note that component C1 is not visible here, and it
      -- makes no sense to allow aggregates for invisible components.

   X72 : ExtE := (C3 => 0, others => 0);                    -- ERROR: {18;1}
      -- Root, Ext6, and ExtA are record types here, but see above.

   X73 : ExtE := (C1 | C2 | C3 => 0);                       -- ERROR: {18;1}
      -- Root, Ext6, and ExtA are record types here, but see above.


   -- Basic cases:
   X81 : Root := (others => 0);                             -- OK.    {18;1}

   X82 : B431007.Child1.Ext1 := (others => 0);              -- ERROR: {33;1}

   X83 : B431007_Unrelated.Ext2 := (others => 0);           -- ERROR: {36;1}


   procedure Force_Body is
   begin
      null; -- Force this package to have (and allow) a body.
   end Force_Body;

end B431007;


