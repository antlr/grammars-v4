--  BDB3A01.A
--
--                            Grant of Unlimited Rights
--
--    The Ada Conformity Assessment Authority (ACAA) holds unlimited
--    rights in the software and documentation contained herein. Unlimited
--    rights are the same as those granted by the U.S. Government for older
--    parts of the Ada Conformity Assessment Test Suite, and are defined
--    in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--    intends to confer upon all recipients unlimited rights equal to those
--    held by the ACAA. These rights include rights to use, duplicate,
--    release or disclose the released technical data and computer software
--    in whole or in part, in any manner and for any purpose whatsoever, and
--    to have or permit others to do so.
--
--                                    DISCLAIMER
--
--    ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--    DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--    WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--    SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--    OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--    PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                    Notice
--
--    The ACAA has created and maintains the Ada Conformity Assessment Test
--    Suite for the purpose of conformity assessments conducted in accordance
--    with the International Standard ISO/IEC 18009 - Ada: Conformity
--    assessment of a language processor. This test suite should not be used
--    to make claims of conformance unless used in accordance with
--    ISO/IEC 18009 and any applicable ACAA procedures.
--*
--  OBJECTIVE:
--    Check that the expected type for the Default_Storage_Pool pragma is
--    Root_Storage_Pool'Class, and that the pragma argument must denote
--    a variable.
--
--    Check that the expected type for the Default_Storage_Pool aspect is
--    Root_Storage_Pool'Class, and that the aspect definition must denote
--    a variable.
--
--  CHANGE HISTORY:
--     09 Oct 2014 BM  Initial Version.
--     20 Nov 2014 RLB Readied for release, adjusted objective and added
--                     aspect test cases.
--     12 Mar 2015 RLB Added missing discriminant to Pool.
--!

with FDB3A00;
with System.Storage_Elements;

procedure BDB3A01 is
   Pool : aliased FDB3A00.Stack_Heap (Water_Line => 24, TC_Id => 'B');

   type Frozen_Pool_Type is access constant FDB3A00.Stack_Heap;

   Frozen_Pool : Frozen_Pool_Type := Pool'Access;

   type Non_Pool_Type is tagged null record;

   Non_Pool : Non_Pool_Type;

   pragma Default_Storage_Pool (Pool);                  -- OK.

   pragma Default_Storage_Pool (Frozen_Pool.all);       -- ERROR: Not variable

   pragma Default_Storage_Pool (Non_Pool_Type);         -- ERROR: Not pool type

   procedure Constant_Pool (Local_Pool : in FDB3A00.Stack_Heap) is
      pragma Default_Storage_Pool (Local_Pool);         -- ERROR: Not variable

      type String_Access is access String;
      S : String_Access;
   begin
      S := new String'("Hello");
   end Constant_Pool;

   pragma Default_Storage_Pool (null);                  -- OK.

   generic
      type Priv is private;
   package Gen is
      type Acc is access Priv;
   end Gen;

   package P1 is new Gen(Integer)
      with Default_Storage_Pool => Pool;                -- OK.

   package P2 is new Gen(Float)
      with Default_Storage_Pool => Frozen_Pool.all;     -- ERROR: Not variable

   package P3 is new Gen(Boolean)
      with Default_Storage_Pool => Non_Pool_Type;       -- ERROR: Not pool type

begin

   Constant_Pool (Pool);

end BDB3A01;
