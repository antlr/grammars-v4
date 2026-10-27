-- B4320022.A
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
--      See B4320020.A.
--
-- TEST DESCRIPTION:
--      See B4320020.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         B4320020.A
--         B4320021.A
--      -> B4320022.A
--
-- PASS/FAIL CRITERIA:
--      See B4320020.A.
--
-- CHANGE HISTORY:
--      30 Dec 19   RLB     Created test.
--
--!

with B432002_Unrelated;
with B432002.Child1;
with B432002.Child2;
package body B432002 is

   type ExtB is new B432002.Child1.Ext1 with record
      C3 : Integer;
   end record;

   type ExtC is new B432002.Child2.Ext3 with record
      C4 : Integer;
   end record;

   type ExtD is new B432002.Child2.Ext4 with record
      C4 : Integer;
   end record;

   type ExtE is new B432002.Child2.Ext6 with record
      C3 : Integer;
   end record;

   X01 : B432002.Child2.Ext3 :=
      (B432002.Root with others => 0);                      -- ERROR: {7;1}
      -- Ext1 is a private type here.

   X02 : B432002.Child2.Ext3 :=
      (B432002.Child1.Ext1 with others => 0);               -- ERROR: {7;1}
      -- Ext3 is a private type here.

   X03 : B432002.Child2.Ext3 :=
      (B432002.Root with C2 | C3 => 0);                     -- ERROR: {7;1}
      -- Ext1 is a private type here; C2 and C3 are not visible here.

   X04 : B432002.Child2.Ext3 :=
      (B432002.Child1.Ext1 with C3 => 0);                   -- ERROR: {7;1}
      -- Ext3 is a private type here; C3 is not visible here.


   X11 : B432002.Child2.Ext4 :=
      (B432002.Root with others => 0);                      -- ERROR: {7;1}
      -- Ext2 and Ext4 are private types here.

   X12 : B432002.Child2.Ext4 :=
      (B432002_Unrelated.Ext2 with others => 0);            -- ERROR: {7;1}
      -- Ext4 is a private type here.

   X13 : B432002.Child2.Ext4 :=
      (B432002.Root with C2 | C3 => 0);                     -- ERROR: {7;1}
      -- Ext2 is a private type here; C2 and C3 are not visible here.

   X14 : B432002.Child2.Ext4 :=
      (B432002_Unrelated.Ext2 with C3 => 0);                -- ERROR: {7;1}
      -- Ext4 is a private type here; C3 is not visible here.


   X21 : B432002.Child2.Ext5 :=
      (B432002.Root with others => 0);                      -- ERROR: {7;1}
      -- Ext5 is a private type here.

   X22 : B432002.Child2.Ext5 :=
      (B432002.Root with C2 => 0);                          -- ERROR: {7;1}
      -- Ext5 is a private type here; C2 is not visible here.


   X31 : B432002.Child2.Ext6 :=
      (B432002.Root with others => 0);                      -- OK.    {7;1}
      -- Ext6 is a record type.

   X32 : B432002.Child2.Ext6 := (B432002.Root with C2 => 0);-- OK.    {18;1}
      -- Root is a record type.


   X41 : ExtB := (B432002.Root with others => 0);           -- ERROR: {18;1}
      -- Ext1 is a private type here.

   X42 : ExtB := (B432002.Child1.Ext1 with others => 0);    -- OK.    {18;1}
      -- ExtB is a record type.


   X51 : ExtC := (B432002.Root with others => 0);           -- ERROR: {18;1}
      -- Ext1 and Ext3 are private types here.

   X52 : ExtC := (B432002.Child1.Ext1 with others => 0);    -- ERROR: {18;1}
      -- Ext3 is a private type here.

   X53 : ExtC := (B432002.Child1.Ext1 with C3 | C4 => 0);   -- ERROR: {18;1}
      -- Ext3 is a private type here; C3 is not visible here.

   X54 : ExtC := (B432002.Child2.Ext3 with others => 0);    -- OK.    {18;1}
      -- ExtC is a record type.

   X55 : ExtC := (B432002.Child2.Ext3 with C4 => 0);        -- OK.    {18;1}
      -- ExtC is a record type.


   X61 : ExtD := (B432002.Root with others => 0);           -- ERROR: {18;1}
      -- Ext2 and Ext4 are private types here.

   X62 : ExtD := (B432002_Unrelated.Ext2 with others => 0); -- ERROR: {18;1}
      -- Ext4 is a private type here.

   X63 : ExtD := (B432002_Unrelated.Ext2 with C3 | C4 => 0);-- ERROR: {18;1}
      -- Ext4 is a private type here; C4 is not visible here.

   X64 : ExtD := (B432002.Child2.Ext4 with others => 0);    -- OK.    {18;1}
      -- ExtD is a record type.

   X65 : ExtD := (B432002.Child2.Ext4 with C4 => 0);        -- OK.    {18;1}
      -- ExtD is a record type.


   X71 : ExtE := (B432002.Root with others => 0);           -- OK.    {18;1}
      -- Ext6 and ExtE are record types here.

   X72 : ExtE := (B432002.Child2.Ext6 with others => 0);    -- OK.    {18;1}
      -- ExtE is a record type.

   X73 : ExtE := (B432002.Root with C2 | C3 => 0);          -- OK.    {18;1}
      -- Ext6 and ExtE are record types here.

   X74 : ExtE := (B432002.Child2.Ext6 with C3 => 0);        -- OK.    {18;1}
      -- ExtE is a record type.


   procedure Force_Body is
   begin
      null; -- Force this package to have (and allow) a body.
   end Force_Body;

end B432002;


