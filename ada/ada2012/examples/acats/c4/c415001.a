-- C415001.A
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
-- OBJECTIVE:
--     Check that the reference_object_name is evaluated by the evaluation of
--     a generalized reference and that Constraint_Error is raised by a
--     generalized reference whose value is null.
--
--     Check that a generalized reference denotes the object denoted by the
--     value of the reference discriminant.
--
--     Check that the object designated by a generalized reference can be
--     modified if the discriminant has an access-to-variable type.
--
-- TEST DESCRIPTION:
--     This test declares a collection type, represented as a record with
--     a component of an array of elements of aliased records that have
--     a numeric key and a character value field, and defines a reference type
--     to denote elements in the collection. An operation Fetch is defined that
--     returns generalized references denoting objects in the collection. We
--     test that the result of calls to Fetch properly allow implicit
--     dereferencing, for both component access and element objects as a
--     whole, and that the returned reference values in fact denote the
--     collection elements.
--
--     A generalized reference is most useful for an abstract data type,
--     (which is usually represented by a private type in Ada) as one could
--     directly reference the array components for a non-private type making
--     the generalized reference unnecessary. While actually using a private
--     type here would be an extra complication to the test, the test needs to
--     use a type that is likely to be the full type declaration for an ADT.
--     Thus we wrap the array in a record rather than directly using an array
--     type.
--
-- CHANGE HISTORY:
--     27 Sep 2013 GJD Created test.
--      3 Apr 2014 GJD Corrected minor test error.
--      4 Apr 2014 RLB Renamed test for inclusion in ACATS 4.0.
--      9 Apr 2015 RLB Corrected a violation of the anti-aliasing rules.
--     26 Sep 2019 RLB Corrected an accessibility error in Fetch.
--     21 Nov 2019 RLB Revised test to be more realistic, fixing a new
--                     error in the process.
--
--!
with Report;
procedure C415001 is

    type Key_Range is new Natural;

    Null_Key : constant Key_Range := 0;

    type Element_Type is
      record
         Key   : Key_Range := Null_Key;
         Value : aliased Character;
      end record;

    type Char_Access is access all Character;

    type Element_Range is new Natural;

    subtype Element_Index is Element_Range range 1 .. Element_Range'Last;

    type Data_Array is array (Element_Index range <>) of aliased Element_Type;

    type Collection (Size : Element_Range) is record
       Data : Data_Array (1 .. Size);
       Other_Components : Float;
    end record;

    type Reference (Ref : access Element_Type) is null record
      with Implicit_Dereference => Ref;

    Null_Reference : constant Reference := (Ref => null);

    Collection_Overflow : exception;

    procedure Update_Collection
      (C : in out Collection; Key : Key_Range; Val : Character) is

        Elt_Index : Element_Index := C.Data'First;

    begin
        while Elt_Index <= C.Size loop
            if C.Data(Elt_Index).Key = Null_Key then

                C.Data(Elt_Index) := (Key => Key, Value => Val);
                return;

            elsif C.Data(Elt_Index).Key = Key then

                C.Data(Elt_Index).Value := Val;
                return;

            end if;

            Elt_Index := Elt_Index + 1;
        end loop;

        raise Collection_Overflow;
    end Update_Collection;

    function Fetch (C   : aliased in out Collection;
                    Key : Key_Range) return Reference is
    begin
        for Elt_Index in C.Data'range loop
            if C.Data(Elt_Index).Key = Key then
                return Reference'(Ref => C.Data(Elt_Index)'Access);
            end if;
        end loop;

        return Null_Reference;
    end Fetch;

    My_Colln : aliased Collection(10);

begin

    Report.Test ("C415001", "Check that the reference_object_name is " &
                 "evaluated by the evaluation of a generalized reference " &
                 "and that Constraint_Error is raised by a generalized " &
                 "reference whose value is null. Check that a generalized " &
                 "reference denotes the object denoted by the value of the " &
                 "reference discriminant. Check that the object designated " &
                 "by a generalized reference can be modified if the " &
                 "discriminant has an access-to-variable type");

    Update_Collection (My_Colln, 1, 'a');
    Update_Collection (My_Colln, 2, 'b');
    Update_Collection (My_Colln, 3, 'c');
    Update_Collection (My_Colln, 4, 'd');
    Update_Collection (My_Colln, 5, 'e');

    declare
       Result : Char_Access;
    begin
       for Key in Key_Range range 1 .. 5 loop
          if Fetch (My_Colln, Key).Value
               /= Character'Val (Character'Pos ('a') + Key - 1)
          then
              Report.Failed
                ("Implicit dereference of element with key"
                   & Key_Range'Image (Key)
                   & " failed");
          end if;

          Result := My_Colln.Data(Element_Range (Key)).Value'Access;
          if Char_Access'(Fetch (My_Colln, Key).Value'Access) /= Result
          then
              Report.Failed
                ("Object denoted by result of dereference differs from "
                 & "collection element");
          end if;
       end loop;

    exception
        when others =>
            Report.Failed ("Unexpected exception from dereference operation");
    end;

    begin
       if Fetch (My_Colln, 6).Value = 'f' then
            Report.Failed
              ("Implicit dereference of element with key 6 "
               & "should have failed");
       end if;

       Report.Failed
         ("Implicit dereference of element with key 6 should have failed");

    exception
        when Constraint_Error =>
            null;
        when others =>
            Report.Failed ("Unexpected exception from dereference operation");
    end;

    begin
       Fetch (My_Colln, 4).Value := '*';

       if Fetch (My_Colln, 4).Value /= '*' then
           Report.Failed
             ("Implicit dereference of Value component of element "
              & "with key 4 failed");
       end if;

       Fetch (My_Colln, 2) := Element_Type'(2, '$');

       if Fetch (My_Colln, 2) /= Element_Type'(2, '$') then
           Report.Failed
             ("Implicit dereference of element with key 4 failed");
       end if;

    exception
        when others =>
            Report.Failed ("Unexpected exception from dereference operation");
    end;

    Report.Result;

end C415001;
