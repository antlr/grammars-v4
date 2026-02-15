--  B552A05.A
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
--     Check that the iterator_name of a generalized iterator does not
--     denote a subcomponent that depends on discriminants of an object
--     whose nominal subtype is unconstrained and which is not known to be
--     constrained.
--
--     Check that a generalized iterator loop is illegal if the
--     cursor subtype is limited at the point of the loop.
--
--  CHANGE HISTORY:
--     10 Feb 2015  BM   Initial Version.
--     19 Mar 2015  RLB  Split into several smaller tests with fewer
--                       objectives.
--!

with F552A00_Prime_Numbers;
with Ada.Iterator_Interfaces;

procedure B552A05 is

   Total : Integer := 0;

begin

   Objective_1 :
   --  Check that the iterator_name of a generalized iterator does not
   --  denote a subcomponent that depends on discriminants of an object
   --  whose nominal subtype is unconstrained and which is not known to be
   --  constrained.
   declare

      type Rec (Has_Iterator : Boolean := True) is record
         case Has_Iterator is
            when True =>
               Primes : F552A00_Prime_Numbers
                 .Prime_Number_Set
               (Max_Value => 15);

            when False =>
               null;
         end case;
      end record;

      X : Rec;
      Y : Rec (Has_Iterator => True);
      Z : access Rec := new Rec;

   begin -- Objective_1
      Total := 0;

      for I in Y.Primes.Iterate loop                            -- OK.
         Total := Total + I;
         Y     := Rec'(Has_Iterator => True, Primes => <>);
      end loop;

      for I in X.Primes.Iterate loop                            -- ERROR:
         Total := Total + I;
         X     := Rec'(Has_Iterator => False);
      end loop;

      for I in Z.all.Primes.Iterate loop                        -- ERROR:
         Total := Total + I;
         Z.all := Rec'(Has_Iterator => False);
      end loop;

      for I in Z.Primes.Iterate loop                            -- ERROR:
         Total := Total + I;
         Z.all := Rec'(Has_Iterator => False);
      end loop;

   end Objective_1;

   ----------------------------------------------------------------

   Objective_2 :
   --  Check that a generalized iterator loop is illegal if the default
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

            for I in Container.Iterate loop                -- OK.
               Total := Total + Container (I);
            end loop;

            return Total;

         end Do_Loops_With_Non_Limited_Constant_View;

         function Do_Loops_With_Non_Limited_Variable_View
           (Container : in out Limited_Cursor_Container) return Natural
         is
            Total : Natural := 0;
         begin
            for I in Container.Iterate loop                -- OK.
               Total := Total + Container (I);
            end loop;

            return Total;
         end Do_Loops_With_Non_Limited_Variable_View;

      end Partial_Limited_Cursor_Test;

      Variable_Container : Partial_Limited_Cursor_Test
        .Limited_Cursor_Container;
      Constant_Container : constant Partial_Limited_Cursor_Test
        .Limited_Cursor_Container :=
        Variable_Container;

   begin  -- Objective_2

      Total :=
        Variable_Container.Do_Loops_With_Non_Limited_Constant_View;

      Total :=
        Constant_Container.Do_Loops_With_Non_Limited_Constant_View;

      Total :=
        Variable_Container.Do_Loops_With_Non_Limited_Variable_View;

      Total := 0;

      for I in Variable_Container.Iterate loop                   -- ERROR:
         Total := Total + Variable_Container (I);
      end loop;

      for I in Constant_Container.Iterate loop                   -- ERROR:
         Total := Total + Constant_Container (I);
      end loop;

      for I in Partial_Limited_Cursor_Test.Create.Iterate loop   -- ERROR:
         Total := Total + Constant_Container (I);
      end loop;

   end Objective_2;

end B552A05;
