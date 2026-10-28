-- B4320021.A
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
--      -> B4320021.A
--         B4320022.A
--
-- PASS/FAIL CRITERIA:
--      See B4320020.A.
--
-- CHANGE HISTORY:
--      30 Dec 19   RLB     Created test.
--
--!

package body B432002.Child2 is

   type Ext7 is new B432002.Child1.Ext1 with record
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

   X01 : Ext3 := (B432002.Root with others => 0);           -- ERROR: {18;1}
      -- Ext1 is a private type here.

   X02 : Ext3 := (B432002.Child1.Ext1 with others => 0);    -- OK.    {18;1}
      -- Ext3 is a record type here.

   X03 : Ext3 := (B432002.Root with C2 | C3 => 0);          -- ERROR: {18;1}
      -- Ext1 is a private type here; C2 is not visible here.

   X04 : Ext3 := (B432002.Child1.Ext1 with C3 => 0);        -- OK.    {18;1}
      -- Ext3 is a record type here.


   X11 : Ext4 := (B432002.Root with others => 0);           -- ERROR: {18;1}
      -- Ext2 is a private type here.

   X12 : Ext4 :=
      (B432002_Unrelated.Ext2 with others => 0);            -- OK.    {7;1}
      -- Ext4 is a record type here.

   X13 : Ext4 := (B432002.Root with C2 | C3 => 0);          -- ERROR: {18;1}
      -- Ext2 is a private type here; C2 is not visible here.

   X14 : Ext4 :=
      (B432002_Unrelated.Ext2 with C3 => 0);                -- OK.    {7;1}
      -- Ext4 is a record type here.


   X21 : Ext5 := (B432002.Root with others => 0);           -- OK.    {18;1}
      -- Root and Ext5 are record types here.

   X22 : Ext5 := (B432002.Root with C2 => 0);               -- OK.    {18;1}
      -- Root and Ext5 are record types here.


   X31 : Ext6 := (B432002.Root with others => 0);           -- OK.    {18;1}
      -- Root and Ext6 are record types here.

   X32 : Ext6 := (B432002.Root with C2 => 0);               -- OK.    {18;1}
      -- Root and Ext6 are record types here.


   X41 : Ext7 := (B432002.Root with others => 0);           -- ERROR: {18;1}
      -- Ext1 is a private type here.

   X42 : Ext7 := (B432002.Child1.Ext1 with others => 0);    -- OK.    {18;1}
      -- Ext7 is a record type.


   X51 : Ext8 := (B432002.Root with others => 0);           -- ERROR: {18;1}
      -- Ext1 is a private type here.

   X52 : Ext8 := (B432002.Child1.Ext1 with others => 0);    -- OK.    {18;1}
      -- Ext3 and Ext8 are record types here.

   X53 : Ext8 := (B432002.Child1.Ext1 with C3 | C4 => 0);   -- OK.    {18;1}
      -- Ext3 and Ext8 are record types here.

   X54 : Ext8 := (Ext3 with others => 0);                   -- OK.    {18;1}
      -- Ext8 is a record type.

   X55 : Ext8 := (Ext3 with C4 => 0);                       -- OK.    {18;1}
      -- Ext8 is a record type.


   X61 : Ext9 := (B432002.Root with others => 0);           -- ERROR: {18;1}
      -- Ext2 is a private type here.

   X62 : Ext9 := (B432002_Unrelated.Ext2 with others => 0); -- OK.    {18;1}
      -- Ext4 and Ext9 are record types here.

   X63 : Ext9 := (B432002_Unrelated.Ext2 with C3 | C4 => 0);-- OK.    {18;1}
      -- Ext4 and Ext9 are record types here.

   X64 : Ext9 := (Ext4 with others => 0);                   -- OK.    {18;1}
      -- Ext9 is a record type.

   X65 : Ext9 := (Ext4 with C4 => 0);                       -- OK.    {18;1}
      -- Ext9 is a record type.


   X71 : ExtA := (B432002.Root with others => 0);           -- OK.    {18;1}
      -- Ext6 and ExtA are record types here.

   X72 : ExtA := (Ext6 with others => 0);                   -- OK.    {18;1}
      -- ExtA is a record type.

   X73 : ExtA := (B432002.Root with C2 | C3 => 0);          -- OK.    {18;1}
      -- Root and Ext6 are record types here.

   X74 : ExtA := (Ext6 with C3 => 0);                       -- OK.    {18;1}
      -- ExtA is a record type.

   procedure Force_Body is
   begin
      null; -- Force this package to have (and allow) a body.
   end Force_Body;

end B432002.Child2;


