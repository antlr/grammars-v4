--  B552A04.A
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
--     Check Legality Rules for container element iterators:
--
--     (A) Check that a container element iterator is illegal if there is a
--         subtype_indication, and it does not statically match the default
--         element subtype of the type of the iterable_name.
--
--     (B) Check that a container element iterator is illegal if the
--         iterable_name is a constant of type T, and Constant_Indexing is not
--         specified for T.
--
--     (C) Check that the iterable_name of a container element iterator does
--         not denote a subcomponent that depends on discriminants of an
--         object whose nominal subtype is unconstrained and which is not
--         known to be constrained.
--
--     (D) Check that a container element iterator loop is illegal if the call
--         to the default element iterator is illegal.
--
--     (E) Check that a container element iterator loop is illegal if the
--         default cursor subtype is limited at the point of the loop.
--
--  CHANGE HISTORY:
--     10 Feb 2015  BM   Initial Version.
--     18 Mar 2015  RLB  Split into several smaller tests with fewer
--                       objectives.
--!

with F552A00_Sparse_Arrays;
with F552A00_Bingo_Balls;
with Ada.Iterator_Interfaces;
with Report;

procedure B552A04 is

   Total : Integer := 0;

begin

   Objective_A :
   --  Check that a container element iterator is illegal if there is a
   --  subtype_indication, and it does not statically match the default element
   --  type of the type of the iterable_name.
   declare
      type Small is range 1 .. 100;
      subtype Tiny is Small range 1 .. 10;
      Small_Var : Small := Small(Report.Ident_Int(10));
      subtype Dyn_Tiny is Small range 1 .. Small_Var;

      package Sparse_Tiny_Arrays is new F552A00_Sparse_Arrays
        (Sparse_Array_Index => Natural,
         Element_Type       => Tiny);

      Sparse_Data : Sparse_Tiny_Arrays.Sparse_Array (Max_Elements => 10);

      Bingo : F552A00_Bingo_Balls.Bingo_Game;
      subtype Balls is F552A00_Bingo_Balls.Bingo_Call range
                       F552A00_Bingo_Balls.B_1 .. F552A00_Bingo_Balls.O_75;
      subtype N_Balls is F552A00_Bingo_Balls.Bingo_Call range
                         F552A00_Bingo_Balls.N_31 .. F552A00_Bingo_Balls.N_45;
      subtype Dyn_Balls is F552A00_Bingo_Balls.Bingo_Call range
                       F552A00_Bingo_Balls.B_1 ..
            F552A00_Bingo_Balls.Bingo_Call'Val(
               Report.Ident_Int (F552A00_Bingo_Balls.Bingo_Call'Pos(
                  F552A00_Bingo_Balls.O_75)));

   begin -- Objective_A

      for Item : Tiny of Sparse_Data loop                      -- OK.
         Item := Item + 1;
      end loop;

      for Item : Dyn_Tiny of Sparse_Data loop                  -- ERROR:
         Item := Item + 1;
      end loop;

      for Item : Small of Sparse_Data loop                     -- ERROR:
         Item := Item + 1;
      end loop;

      for Item : Tiny'Base of Sparse_Data loop                 -- ERROR:
         Item := Item + 1;
      end loop;

      for Item : Small range 1 .. 10 of Sparse_Data loop       -- OK.
         Item := Item + 1;
      end loop;

      for Item : Small range 1 .. Small_Var of Sparse_Data loop-- ERROR:
         Item := Item + 1;
      end loop;

      for Item : F552A00_Bingo_Balls.Bingo_Call of Bingo loop  -- OK.
         Total := Total + 1;
      end loop;

      for Item : Balls of Bingo loop                           -- OK.
         Total := Total + 1;
      end loop;

      for Item : N_Balls of Bingo loop                         -- ERROR:
         Total := Total + 1;
      end loop;

      for Item : Dyn_Balls of Bingo loop                       -- ERROR:
         Total := Total + 1;
      end loop;

   end Objective_A;

   -------------------------------------------------------------

   Objective_B :
   --  Check that a container element iterator is illegal if the iterable_name
   --  is a constant of type T, and Constant_Indexing is not specified for T.

   declare
      package Container_Test is

         type Container_Without_Constant_Indexing is tagged private with
            Variable_Indexing => Reference,
            Default_Iterator  => Iterate,
            Iterator_Element  => Natural;

         type Cursor is private;

         function Has_Element (Position : Cursor) return Boolean;

         package Test_Iterator_Interfaces is new Ada.Iterator_Interfaces
           (Cursor,
            Has_Element);

         function Iterate
           (Container : Container_Without_Constant_Indexing)
            return Test_Iterator_Interfaces.Reversible_Iterator'Class;

         type Reference_Type
           (Element : not null access Natural) is private with
            Implicit_Dereference => Element;

         function Reference
           (Container : aliased in out Container_Without_Constant_Indexing;
            Position  :                Cursor) return Reference_Type;

      private

         type Data_Array is array (1 .. 10) of aliased Natural;

         type Container_Without_Constant_Indexing is tagged record
            Data : Data_Array := (others => <>);
         end record;

         type Test_Container_Access is
           access constant Container_Without_Constant_Indexing;
         for Test_Container_Access'Storage_Size use 0;

         type Cursor is record
            Index     : Positive;
            Container : Test_Container_Access;
         end record;

         type Iterator is new Test_Iterator_Interfaces.Reversible_Iterator with
         record
            Index     : Positive;
            Container : Test_Container_Access;
         end record;

         overriding function First (Object : Iterator) return Cursor;

         overriding function Next
           (Object   : Iterator;
            Position : Cursor) return Cursor;

         overriding function Last (Object : Iterator) return Cursor;

         overriding function Previous
           (Object   : Iterator;
            Position : Cursor) return Cursor;

         function First
           (Object : Iterator) return Cursor is
           (Index => 1, Container => Object.Container);

         function Next
           (Object   : Iterator;
            Position : Cursor) return Cursor is
           ((Index => Position.Index + 1, Container => Object.Container));

         function Last
           (Object : Iterator) return Cursor is
           ((Index     => Object.Container.Data'Last,
             Container => Object.Container));

         function Previous
           (Object   : Iterator;
            Position : Cursor) return Cursor is
           ((Index => Position.Index - 1, Container => Object.Container));

         type Reference_Type
           (Element : not null access Natural) is null record;

         function Has_Element
           (Position : Cursor) return Boolean is
           (Position.Index in 1 .. 10);

         function Iterate
           (Container : Container_Without_Constant_Indexing)
            return Test_Iterator_Interfaces.Reversible_Iterator'Class is
           (Iterator'(Index => 1, Container => Container'Unchecked_Access));

         function Reference
           (Container : aliased in out Container_Without_Constant_Indexing;
            Position  :                Cursor) return Reference_Type is
           ((Element => Container.Data (Position.Index)'Access));

      end Container_Test;

      Test_Container : Container_Test.Container_Without_Constant_Indexing;
      Constant_Test_Container : constant Container_Test
        .Container_Without_Constant_Indexing :=
        Test_Container;

      function Get_Container return
         Container_Test.Container_Without_Constant_Indexing is
      begin
         return Test_Container;
      end Get_Container;

      procedure Test_It (Param_Container : in
          Container_Test.Container_Without_Constant_Indexing) is
      begin
         for Item of Param_Container loop                      -- ERROR:
            Total := Total + Item;
         end loop;
      end Test_It;

   begin -- Objective_B

      for Item of Test_Container loop                          -- OK.
         Item := Item + 1;
      end loop;

      for Item of Constant_Test_Container loop                 -- ERROR:
         Total := Total + Item;
      end loop;

      Test_It (Test_Container);

      for Item of Get_Container loop                           -- ERROR:
         Total := Total + Item;
      end loop;

   end Objective_B;

   -------------------------------------------------------------------

   Objective_C :
   --  Check that the iterable_name of a container element iterator does
   --  not denote a subcomponent that depends on discriminants of an object
   --  whose nominal subtype is unconstrained and which is not known to be
   --  constrained.
   declare

      package Sparse_Integer_Arrays is new F552A00_Sparse_Arrays
        (Sparse_Array_Index => Natural,
         Element_Type       => Integer);

      type Rec (Has_Iterator : Boolean := True) is record
         case Has_Iterator is
            when True =>
               Sparse_Data : aliased Sparse_Integer_Arrays
                 .Sparse_Array(Max_Elements => 10);

            when False =>
               null;
         end case;
      end record;

      X : Rec;
      Y : Rec (Has_Iterator => True);
      Z : access Rec := new Rec;

   begin -- Objective_C
      Total := 0;

      for I of Y.Sparse_Data loop                               -- OK.
         Total := Total + I;
         Y     := Rec'(Has_Iterator => True, Sparse_Data => <>);
      end loop;

      for I of X.Sparse_Data loop                               -- ERROR:
         Total := Total + I;
         X     := Rec'(Has_Iterator => False);
      end loop;

      for I of Z.all.Sparse_Data loop                           -- ERROR:
         Total := Total + I;
         Z.all := Rec'(Has_Iterator => False);
      end loop;

      for I of Z.Sparse_Data loop                               -- ERROR:
         Total := Total + I;
         Z.all := Rec'(Has_Iterator => False);
      end loop;

   end Objective_C;

   ---------------------------------------------------------------

   Objective_D :
   --  Check that a container element iterator loop is illegal if the call
   --  to the default element iterator is illegal.
   declare
      package Bad_Container_Test is

         type Container_With_Illegal_Iterate is tagged private with
            Constant_Indexing => Constant_Reference,
            Variable_Indexing => Reference,
            Default_Iterator  => Iterate,
            Iterator_Element  => Natural;

         type Cursor is private;

         function Has_Element (Position : Cursor) return Boolean;

         package Container_Iterator_Interfaces is new Ada.Iterator_Interfaces
           (Cursor,
            Has_Element);

         function Iterate
           (Container : aliased in out Container_With_Illegal_Iterate)
            return Container_Iterator_Interfaces.Reversible_Iterator'Class;

         type Constant_Reference_Type
           (Element : not null access constant Natural) is private with
            Implicit_Dereference => Element;

         type Reference_Type
           (Element : not null access Natural) is private with
            Implicit_Dereference => Element;

         function Constant_Reference
           (Container : aliased Container_With_Illegal_Iterate;
            Position  :         Cursor) return Constant_Reference_Type;

         function Reference
           (Container : aliased in out Container_With_Illegal_Iterate;
            Position  :                Cursor) return Reference_Type;

      private

         type Data_Array is array (1 .. 10) of aliased Natural;

         type Container_With_Illegal_Iterate is tagged record
            Data : Data_Array := (others => <>);
         end record;

         type Container_Access is
           access constant Container_With_Illegal_Iterate;
         for Container_Access'Storage_Size use 0;

         type Cursor is record
            Index     : Positive;
            Container : Container_Access;
         end record;

         type Iterator is new Container_Iterator_Interfaces
           .Reversible_Iterator with
         record
            Index     : Positive;
            Container : Container_Access;
         end record;

         overriding function First (Object : Iterator) return Cursor;

         overriding function Next
           (Object   : Iterator;
            Position : Cursor) return Cursor;

         overriding function Last (Object : Iterator) return Cursor;

         overriding function Previous
           (Object   : Iterator;
            Position : Cursor) return Cursor;

         function First
           (Object : Iterator) return Cursor is
           (Index => 1, Container => Object.Container);

         function Next
           (Object   : Iterator;
            Position : Cursor) return Cursor is
           ((Index => Position.Index + 1, Container => Object.Container));

         function Last
           (Object : Iterator) return Cursor is
           ((Index     => Object.Container.Data'Last,
             Container => Object.Container));

         function Previous
           (Object   : Iterator;
            Position : Cursor) return Cursor is
           ((Index => Position.Index - 1, Container => Object.Container));

         type Constant_Reference_Type
           (Element : not null access constant Natural) is null record;

         type Reference_Type
           (Element : not null access Natural) is null record;

         function Has_Element
           (Position : Cursor) return Boolean is
           (Position.Index in 1 .. 10);

         function Iterate
           (Container : aliased in out Container_With_Illegal_Iterate)
            return Container_Iterator_Interfaces.Reversible_Iterator'Class is
           (Iterator'(Index => 1, Container => Container'Unchecked_Access));

         function Constant_Reference
           (Container : aliased Container_With_Illegal_Iterate;
            Position  :         Cursor) return Constant_Reference_Type is
           ((Element => Container.Data (Position.Index)'Access));

         function Reference
           (Container : aliased in out Container_With_Illegal_Iterate;
            Position  :                Cursor) return Reference_Type is
           ((Element => Container.Data (Position.Index)'Access));

      end Bad_Container_Test;

      function Get_Bad_Container
        return Bad_Container_Test.Container_With_Illegal_Iterate
      is
         Result : Bad_Container_Test.Container_With_Illegal_Iterate;
      begin
         return Result;
      end Get_Bad_Container;

      Variable_Container : Bad_Container_Test.Container_With_Illegal_Iterate;
      Constant_Container : constant Bad_Container_Test
        .Container_With_Illegal_Iterate :=
        Variable_Container;

   begin -- Objective_D
      Total := 0;

      for I of Variable_Container loop                        -- OK.
         Total := Total + I;
      end loop;

      for I of Constant_Container loop                        -- ERROR:
         Total := Total + I;
      end loop;

      for I of Get_Bad_Container loop                         -- ERROR:
         Total := Total + I;
      end loop;

      -- The calls below here are just to demonstrate the problem:

      for I in Variable_Container.Iterate loop                -- OK.
         Total := Total + Variable_Container (I);
      end loop;

      for I in Constant_Container.Iterate loop                -- ERROR:
         Total := Total + Constant_Container (I);
      end loop;

      for I in Get_Bad_Container.Iterate loop                 -- ERROR:
         Total := Total + Constant_Container (I);
      end loop;
   end Objective_D;

   Objective_E :
   --  Check that a container element iterator loop is illegal if the default
   --  cursor subtype is limited at the point of the loop.
   declare
      package Partial_Limited_Cursor_Test is

         type Limited_Cursor_Container is tagged private with
            Constant_Indexing => Constant_Reference,
            Variable_Indexing => Reference,
            Default_Iterator  => Iterate,
            Iterator_Element  => Natural;

            --  Cursor is limited for instantiation of Iterator_Interfaces
         type Cursor is limited private;

         function Create return Limited_Cursor_Container;

         function Has_Element (Position : Cursor) return Boolean;

         package Container_Iterator_Interfaces is new Ada.Iterator_Interfaces
           (Cursor,
            Has_Element);

         function Iterate
           (Container : Limited_Cursor_Container)
            return Container_Iterator_Interfaces.Reversible_Iterator'Class;

         type Constant_Reference_Type
           (Element : not null access constant Natural) is private with
            Implicit_Dereference => Element;

         type Reference_Type
           (Element : not null access Natural) is private with
            Implicit_Dereference => Element;

         function Constant_Reference
           (Container : aliased Limited_Cursor_Container;
            Position  :         Cursor) return Constant_Reference_Type;

         function Reference
           (Container : aliased in out Limited_Cursor_Container;
            Position  :                Cursor) return Reference_Type;

         function Do_Loops_With_Non_Limited_Constant_View
           (Container : Limited_Cursor_Container) return Natural;

         function Do_Loops_With_Non_Limited_Variable_View
           (Container : in out Limited_Cursor_Container) return Natural;

      private

         type Data_Array is array (1 .. 10) of aliased Natural;

         type Limited_Cursor_Container is tagged record
            Data : Data_Array := (others => <>);
         end record;

         type Container_Access is access constant Limited_Cursor_Container;
         for Container_Access'Storage_Size use 0;

         type Cursor is record
            Index     : Positive;
            Container : Container_Access;
         end record;

         type Iterator is new Container_Iterator_Interfaces
           .Reversible_Iterator with
         record
            Index     : Positive;
            Container : Container_Access;
         end record;

         overriding function First (Object : Iterator) return Cursor;

         overriding function Next
           (Object   : Iterator;
            Position : Cursor) return Cursor;

         overriding function Last (Object : Iterator) return Cursor;

         overriding function Previous
           (Object   : Iterator;
            Position : Cursor) return Cursor;

         function First
           (Object : Iterator) return Cursor is
           (Index => 1, Container => Object.Container);

         function Next
           (Object   : Iterator;
            Position : Cursor) return Cursor is
           ((Index => Position.Index + 1, Container => Object.Container));

         function Last
           (Object : Iterator) return Cursor is
           ((Index     => Object.Container.Data'Last,
             Container => Object.Container));

         function Previous
           (Object   : Iterator;
            Position : Cursor) return Cursor is
           ((Index => Position.Index - 1, Container => Object.Container));

         type Constant_Reference_Type
           (Element : not null access constant Natural) is null record;

         type Reference_Type
           (Element : not null access Natural) is null record;

         function Has_Element
           (Position : Cursor) return Boolean is
           (Position.Index in 1 .. 10);

         function Iterate
           (Container : Limited_Cursor_Container)
            return Container_Iterator_Interfaces.Reversible_Iterator'Class is
           (Iterator'(Index => 1, Container => Container'Unchecked_Access));

         function Constant_Reference
           (Container : aliased Limited_Cursor_Container;
            Position  :         Cursor) return Constant_Reference_Type is
           ((Element => Container.Data (Position.Index)'Access));

         function Reference
           (Container : aliased in out Limited_Cursor_Container;
            Position  :                Cursor) return Reference_Type is
           ((Element => Container.Data (Position.Index)'Access));

      end Partial_Limited_Cursor_Test;

      package body Partial_Limited_Cursor_Test is

         function Create return Limited_Cursor_Container is
            Result : Limited_Cursor_Container;
         begin
            return Result;
         end Create;

         function Do_Loops_With_Non_Limited_Constant_View
           (Container : Limited_Cursor_Container) return Natural
         is
            Total : Natural := 0;

            X : constant Cursor :=
              (Index => 1, Container => Container'Unchecked_Access);
            Y : Cursor;
         begin

            Y := X;                                        -- OK.
                -- We have a non-limited view of the cursor here, so
                -- loops are allowed.

            for I of Container loop                        -- OK.
               Total := Total + I;
            end loop;

            for I of Create loop                           -- OK.
               Total := Total + I;
            end loop;

            return Total;

         end Do_Loops_With_Non_Limited_Constant_View;

         function Do_Loops_With_Non_Limited_Variable_View
           (Container : in out Limited_Cursor_Container) return Natural
         is
            Total : Natural := 0;
         begin
            for I of Container loop                           -- OK.
               Total := Total + I;
            end loop;

            return Total;
         end Do_Loops_With_Non_Limited_Variable_View;

      end Partial_Limited_Cursor_Test;

      Variable_Container : Partial_Limited_Cursor_Test
        .Limited_Cursor_Container;
      Constant_Container : constant Partial_Limited_Cursor_Test
        .Limited_Cursor_Container :=
        Variable_Container;

   begin  -- Objective_E

      Total :=
        Variable_Container.Do_Loops_With_Non_Limited_Constant_View;

      Total :=
        Constant_Container.Do_Loops_With_Non_Limited_Constant_View;

      Total :=
        Variable_Container.Do_Loops_With_Non_Limited_Variable_View;

      Total := 0;

      for I of Variable_Container loop                        -- ERROR:
         Total := Total + I;
      end loop;

      for I of Constant_Container loop                        -- ERROR:
         Total := Total + I;
      end loop;

      for I of Partial_Limited_Cursor_Test.Create loop        -- ERROR:
         Total := Total + I;
      end loop;

   end Objective_E;

end B552A04;
