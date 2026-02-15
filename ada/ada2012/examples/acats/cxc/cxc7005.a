-- CXC7005.A
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
--  OBJECTIVE:
--      Check that an object of a task interface type can be the prefix of the
--      Identity attribute.
--
-- APPLICABILITY CRITERIA:
--      This test is only applicable for a compiler attempting validation
--      for the Systems Programming Annex.
--
--  CHANGE HISTORY:
--      28 Oct 2007 RLB Created test based on outline from test C394001.
--!

with Impdef;
with Ada.Task_Identification;
with Report; use Report;

procedure CXC7005 is

   use type Ada.Task_Identification.Task_Id;

   package Pkg is
      ---------------
      -- Interface --
      ---------------

      type Task_Attribute_Iface is task interface;
      procedure Stop (Obj : in out Task_Attribute_Iface) is abstract;

      -------------------------------
      -- Interface implementations --
      -------------------------------

      task type Task_Typ_Task_Attribute_Iface is new
         Task_Attribute_Iface
      with
         entry Stop;
      end Task_Typ_Task_Attribute_Iface;
   end Pkg;

   package body Pkg is

      ------------
      -- Bodies --
      ------------

      task body Task_Typ_Task_Attribute_Iface is
         Terminated : Boolean := False;
      begin
         while not Terminated loop
            accept Stop do
               Terminated := True;
            end Stop;
         end loop;
      end Task_Typ_Task_Attribute_Iface;
   end Pkg;

   use Pkg;

   -----------
   -- Tests --
   -----------

   procedure Check_Value (Value, Expected : Boolean; Attribute : String) is
   begin
      if Value /= Expected then
         Failed (Attribute & " is " & Boolean'Image(Value) & " but " &
                 Boolean'Image(Expected) & " was expected");
      end if;
   end Check_Value;


   procedure Check_Id (Id, Expected : Ada.Task_Identification.Task_Id;
                       Task_Name : String) is
   begin
      if Id /= Expected then
         Failed (Task_Name & " is " & Ada.Task_Identification.Image(Id) &
                 " but " & Ada.Task_Identification.Image(Expected) &
                 " was expected");
      end if;
   end Check_Id;


   procedure Check_Attributes_And_Stop
        (Obj : in out Task_Attribute_Iface'Class;
         Obj_Id : in Ada.Task_Identification.Task_Id) is
   begin
      delay Impdef.Switch_to_New_Task;
      Check_Id (Id => Obj'Identity, Expected => Obj_Id,
                Task_Name => "Stop_Task");
      Check_Value (Value => Ada.Task_Identification.Is_Callable(Obj'Identity),
                   Expected => True,
                   Attribute => "Is_Callable");
      Check_Value (Value => Ada.Task_Identification.Is_Terminated(Obj'Identity),
                   Expected => False,
                   Attribute => "Is_Terminated");

      Comment ("  Stop task");
      Obj.Stop;

      delay Impdef.Minimum_Task_Switch;
      Check_Value (Value => Ada.Task_Identification.Is_Callable(Obj'Identity),
                   Expected => False,
                   Attribute => "Is_Callable");
      Check_Value (Value => Obj'Terminated,
                   Expected => True,
                   Attribute => "Is_Terminated");

   end Check_Attributes_And_Stop;


   procedure Check_Attributes_And_Abort
        (Obj : in out Task_Attribute_Iface'Class;
         Obj_Id : in Ada.Task_Identification.Task_Id) is
   begin
      delay Impdef.Switch_to_New_Task;
      Check_Id (Id => Obj'Identity, Expected => Obj_Id,
                Task_Name => "Abort_Task");
      Check_Value (Value => Ada.Task_Identification.Is_Callable(Obj'Identity),
                   Expected => True,
                   Attribute => "Is_Callable");
      Check_Value (Value => Ada.Task_Identification.Is_Terminated(Obj'Identity),
                   Expected => False,
                   Attribute => "Is_Terminated");

      Comment ("  Abort task");
      abort Obj;

      delay Impdef.Minimum_Task_Switch;
      Check_Value (Value => Ada.Task_Identification.Is_Callable(Obj'Identity),
                   Expected => False,
                   Attribute => "Is_Callable");
      Check_Value (Value => Ada.Task_Identification.Is_Terminated(Obj'Identity),
                   Expected => True,
                   Attribute => "Is_Terminated");
   end Check_Attributes_And_Abort;

   --  Local variables

   TTTAI1 : Task_Typ_Task_Attribute_Iface;
   TTTAI2 : Task_Typ_Task_Attribute_Iface;

begin
   Test ("CXC7005", "Check that an object of a task interface type can be " &
                    "the prefix of the Identity attribute");

   Check_Attributes_And_Stop  (TTTAI1, TTTAI1'Identity);
   Check_Attributes_And_Abort (TTTAI2, TTTAI2'Identity);

   Result;
end CXC7005;
