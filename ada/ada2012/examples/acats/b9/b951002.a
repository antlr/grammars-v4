-- B951002.A
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
--      Check that the current instance of a protected type (and components
--      of that protected type) are constants within the body of a
--      protected function or the body of a function declared immediately
--      within a protected body.
--
-- TEST DESCRIPTION:
--      Ada 95 and Ada 2005 give this rule in 9.5.1(2). Ada 2012 moves the
--      rule to 3.3(21.2/3), as the lead-in to the list of bullets in 3.3(15)
--      says that "The following (and no others) represent constants:", and
--      thus defining something to be a constant somewhere else is wrong.
--      We locate this test in 9.5.1 and base it on the old rule (which was
--      left in the Standard even in Ada 2012), although the test might be
--      better classified as belonging to 3.3 for Ada 2012.
--
--      We test both direct uses of the current instance, and uses of
--      components declared directly in the current instance.
--
--      We try to test at least one case of every place that a constant
--      is not allowed (which is usually worded as "a variable view is
--      required"):
--          * the target of an assignment;
--          * the prefix of 'Access when the expected type is
--            an access-to-variable type;
--          * the actual for a parameter of mode in out or out;
--          * the target object for a protected procedure or entry;
--          * the actual for a generic formal object of mode in out.
--
--      We don't try internal calls to protected procedures or entries
--      because those were tested in test B950001.

-- CHANGE HISTORY:
--       3 MAR 13   RLB     Created test from submitted test.
--
--!


procedure B951002 is

   generic
      Int_Obj : in out Natural;
   procedure Gen_Obj (Val : in Natural);

   procedure Gen_Obj (Val : in Natural) is
   begin
      Int_Obj := Val;
   end Gen_Obj;

   protected type Buffer is
      procedure Set (Value : in Natural);
      function Get return Natural;
   private
      Data : Natural;
   end Buffer;

   protected body Buffer is
      procedure Set (Value : in Natural) is
      begin
         Data := Value;
      end Set;

      function Get return Natural is
      begin
         return Data;
      end Get;
   end Buffer;

begin
   declare
      type Some_Type;
      type ST_Acc is access all Some_Type;
      type Some_Type is record
         Data : Natural;
         Next : ST_Acc;
      end record;

      protected type Stack is
         function Is_Empty return Boolean;
         procedure Push (Item : in ST_Acc);
         function Pop return ST_Acc;
      private
         Stack_Head : ST_Acc := null;
         Sentinel   : aliased Some_Type := (Data => 12, Next => null);
         Common     : Buffer;
      end Stack;

      procedure Stack_It (My_Stack : in out Stack);

      procedure Stock_It (My_Stack : in Stack);

      procedure Make (Item : out ST_Acc);

      protected body Stack is

         function Is_Empty return Boolean is
         begin
             return Stack_Head = null;
         end Is_Empty;

         function Pop return ST_Acc is
             Temp : ST_Acc := Stack_Head;                   -- OK.
             T2   : access Some_Type := Sentinel'Access;    -- ERROR:
         begin
             Stack_Head := Stack_Head.Next;                 -- ERROR:
             Sentinel.Next := Temp;                         -- ERROR:
             Stack_Head.Next.Data := 52;                    -- OK.
             Make (Stack_Head);                             -- ERROR:
             Make (Temp);                                   -- OK.
             Stack_It (Stack);                              -- ERROR:
             Stock_It (Stack);                              -- OK.
             Common.Set (6);                                -- ERROR:
             Stack_Head.Next.Data := Common.Get;            -- OK.
             declare
                 procedure Doer is
                     new Gen_Obj (Sentinel.Data);           -- ERROR:
             begin
                 null; -- Doer (10); -- Don't call bad declaration.
             end;
             return Temp;
         end Pop;

         procedure Push (Item : in ST_Acc) is
             Temp : ST_Acc := Stack_Head;                   -- OK.
             T2   : access Some_Type := Sentinel'Access;    -- OK.
	 begin
             Item.Next := Stack_Head;
             Stack_Head := Item;                            -- OK.
             Sentinel.Next := Temp;                         -- OK.
             Stack_Head.Next.Data := 52;                    -- OK.
             Make (Stack_Head);                             -- OK.
             Make (Temp);                                   -- OK.
             Stack_It (Stack);                              -- OK.
             Stock_It (Stack);                              -- OK.
             Common.Set (6);                                -- OK.
             Stack_Head.Next.Data := Common.Get;            -- OK.
             declare
                 procedure Doer is
                    new Gen_Obj (Sentinel.Data);            -- OK.
             begin
                 Doer (10);
             end;
         end Push;

         function Local return ST_Acc is
             -- A function declared immediately within a protected body.
             -- This is not (formally) a protected function.
             Temp : ST_Acc := Stack_Head;                   -- OK.
             T2   : access Some_Type := Sentinel'Access;    -- ERROR:
         begin
             Stack_Head := Stack_Head.Next;                 -- ERROR:
             Sentinel.Next := Temp;                         -- ERROR:
             Stack_Head.Next.Data := 52;                    -- OK.
             Make (Stack_Head);                             -- ERROR:
             Make (Temp);                                   -- OK.
             Stack_It (Stack);                              -- ERROR:
             Stock_It (Stack);                              -- OK.
             Common.Set (6);                                -- ERROR:
             Stack_Head.Next.Data := Common.Get;            -- OK.
             declare
                 procedure Doer is
                     new Gen_Obj (Sentinel.Data);           -- ERROR:
             begin
                 null; -- Doer (10); -- Don't call bad declaration.
             end;
             return Temp;
         end Local;

      end Stack;

      procedure Stack_It (My_Stack : in out Stack) is
      begin
         My_Stack.Push (new Some_Type'(80, null));         -- OK.
      end Stack_It;

      procedure Stock_It (My_Stack : in Stack) is
      begin
         if My_Stack.Is_Empty then
            null;
         end if;
      end Stock_It;

      procedure Make (Item : out ST_Acc) is
      begin
         Item := new Some_Type'(Data => 44, Next => null);
      end Make;

      Some_Stack : Stack;
   begin
      if Some_Stack.Is_Empty then
         null;
      end if;
   end;
end B951002;


