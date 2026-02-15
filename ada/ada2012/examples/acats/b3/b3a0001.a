-- B3A0001.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--
--    Check that objects defined to be of a general access type may not
--    designate an object or component which is not defined to be
--    aliased.
--    Check that a renaming of an aliased view is also defined to be
--    aliased.
--    Check that an array slice may not be aliased.
--    Check that the general access modifiers "all" and "constant" are
--    allowed.
--    Check that an object designated by an access-to-constant type object
--    cannot be updated through a value of that type.
--    Check that an object designated by a value of an access-to-variable
--    type can be both read and updated.
--
-- TEST DESCRIPTION:
--    This test defines several access types and aliased objects, and
--    attempts to misuse them in several different ways.  Attempts are made to
--    update constant objects via access values; as well as an attempt to
--    determine the access value of an array slice.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      03 Dec 95   SAIC    ACVC 2.0.1 fixes: Corrected mislabelings of cases.
--                          Corrected case causing undesired error.
--      26 Oct 07   RLB     Corrected description of error for "implicitly
--                          limited" type; eliminated accessibility errors;
--                          removed unneeded types; added limited record
--                          subtests; added illegal renames cases; added
--                          view and value conversion cases (comments
--                          implied that they were tested, but no tests
--                          found).
--
--!

procedure B3A0001 is

  package Implicit_References is

    type Tagger is tagged null record;
    procedure Has_Formal_Tag_Parm( Formal: in out Tagger );
    procedure No_Formal_Tag_Parm( Formal: in out Integer );

    type Limited_Tagger is tagged limited null record;

    type Limited_With_Current_Instance is limited private;

    type Simple_Limited is limited record
      Tagponent : Tagger;
    end record;

    procedure Encapsulated_Tag_Parm( Formal: in out Simple_Limited );

    type UnLimited_With_Current_Instance is private;

    type Implicitly_Limited_With_Current_Instance is
      record
        Component: Simple_Limited;
        Self     : access Implicitly_Limited_With_Current_Instance :=
           Implicitly_Limited_With_Current_Instance'Unchecked_Access; -- ERROR:
                            -- Current instance is not aliased
      end record;

    type Limited_Record_With_Current_Instance is limited
      record
        Self     : access Limited_Record_With_Current_Instance
           := Limited_Record_With_Current_Instance'Unchecked_Access;  -- OK.
      end record;

    type Limited_Extension_With_Current_Instance is new Limited_Tagger with
      record
        Self     : access Limited_Extension_With_Current_Instance :=
           Limited_Extension_With_Current_Instance'Unchecked_Access;  -- OK.
      end record;

  private

    -- Partial view is limited, but full view is not:
    type Limited_With_Current_Instance is record
      Self : access Limited_With_Current_Instance
           := Limited_With_Current_Instance'Unchecked_Access;         -- ERROR:
                            -- Current instance is not aliased.
    end record;

    type UnLimited_With_Current_Instance is record
      Self : access UnLimited_With_Current_Instance
           := UnLimited_With_Current_Instance'Unchecked_Access;       -- ERROR:
                            -- Current instance is not aliased.
    end record;

  end Implicit_References;

  package body Implicit_References is


    procedure Has_Formal_Tag_Parm( Formal: in out Tagger ) is
      type Tagger_Ref  is access all Tagger;
      TagRef : Tagger_Ref;
    begin
      TagRef := Formal'Access;                                -- OK.
    end Has_Formal_Tag_Parm;

    procedure No_Formal_Tag_Parm( Formal: in out Integer ) is
      type Integer_Ref is access all Integer;
      Ref : Integer_Ref;
    begin
      Ref := Formal'Access;                                   -- ERROR:
                                     -- Formal parameter is not aliased
    end No_Formal_Tag_Parm;

    procedure Encapsulated_Tag_Parm( Formal: in out Simple_Limited ) is
      type Limited_Ref is access all Simple_Limited;
      Ref : Limited_Ref;
    begin
      Ref := Formal'Access;                                   -- ERROR:
                                     -- Formal parameter is not aliased
    end Encapsulated_Tag_Parm;

  end Implicit_References;

  -- 3.10.(6) general access modifier all

  type General_Access_Type is access all Integer;
  type Indirect is access General_Access_Type;

  type Pool_Acc_Int is access Integer;

  General_Access           : General_Access_Type;
  Non_Aliased_Integer      :                  Integer;
  Aliased_Integer          : aliased          Integer;
  Constant_Integer         :         constant Integer := 42;
  Aliased_Constant_Integer : aliased constant Integer := 43;

  Pool_Acc_Obj : Pool_Acc_Int := new Integer'(10);

  -- 3.10.(6) general access modifier constant

  type Access_Constant_Type is access constant Integer;
  Access_Constant  : Access_Constant_Type
                     := Aliased_Constant_Integer'Access;

  type Access_to_Acc_Const is access Access_Constant_Type;
  Handle : Access_to_Acc_Const := new Access_Constant_Type'(new Integer'(5));
  Double_Indirect : Indirect
                    := new General_Access_Type'(Aliased_Integer'Access);

  type Rec is record
    Field : Integer;
  end record;
  A_Rec : Rec;
  type Access_Constant_Rec_Type is access constant Rec;
  Access_Const_Rec : Access_Constant_Rec_Type := new Rec'(Field => 42);

  type Arr is array (1..10) of Rec;
  type Access_Constant_Arr is access constant Arr;
  Access_Arr : Access_Constant_Arr := new Arr'(others => Rec'(Field=>13));

  type Embedded_Access_Constant is record
    Access_Field : Access_Constant_Rec_Type;
  end record;
  Rec_With_Const_Comp : Embedded_Access_Constant;

  type Array_of_Access_Constant
       is array ( 1..10 ) of Access_Constant_Rec_Type;
  Arr_Of_Acc_Const : Array_of_Access_Constant;

  type Arr_of_Rec_with_Acc_Const
       is array (Natural range <>) of Embedded_Access_Constant;
  Arr_Rec_Acc_Const : Arr_of_Rec_with_Acc_Const (1..39);
  type Arr_of_Acc_Const_to_Rec
       is array (Natural range <>) of Access_Constant_Rec_Type;
  Arr_Acc_Const_Rec : Arr_of_Acc_Const_to_Rec(1..100);

  type Bounds          is range 1..100;
  type Array_Type      is array(Bounds range <>) of Integer;
  subtype Array_Constrained is Array_Type(1..100);
  type Array_Reference is access all Array_Constrained;

  An_Aliased_Array     : aliased Array_Constrained;
  An_Array_Reference   : Array_Reference := An_Aliased_Array'Access;

  type Array_Of_Aliased is array(Bounds range 1..100) of aliased Integer;
  An_Array_Of_Aliased   : Array_Of_Aliased;

  type Aliased_Array_Reference is access all Array_Of_Aliased;
  Aliased_Aliased_Array : aliased Array_Of_Aliased;
  Aliased_Array_Ref     : Aliased_Array_Reference
                          := Aliased_Aliased_Array'Access;

  type Record_Type(Size: Bounds := 1) is record
    A_Component : aliased Integer;
    B_Component : aliased Array_Of_Aliased;
  end record;

  A_Record : Record_Type(50);

  procedure Update( V: out Integer ) is
  begin
    V := V+1;
  end Update;

begin  -- Main test procedure.

  General_Access := Non_Aliased_Integer'Access;               -- ERROR:
     -- general access type object may not reference object not defined
                                                      -- to be aliased.
  General_Access := Constant_Integer'Access;                  -- ERROR:
     -- general access type object may not reference object not defined
                                                      -- to be aliased.

  General_Access      := Aliased_Integer'Access;              -- OK
  General_Access      := An_Array_Of_Aliased(1)'Access;       -- OK
  General_Access      := A_Record.A_Component'Access;         -- OK
  General_Access.all  := 1;                                   -- OK
  Non_Aliased_Integer := General_Access.all;                  -- OK

  -- access-to-variable may not designate a constant
  General_Access := Aliased_Constant_Integer'Access;          -- ERROR:
               -- attempt to designate constant with access-to-variable

  Access_Constant := new Integer'(10);                        -- OK
  Access_Constant := Aliased_Integer'Access;                  -- OK
  Access_Constant.all := 12;                                  -- ERROR:
                               -- access-to-constant value is read only

  Access_Constant := Aliased_Constant_Integer'Access;         -- OK

  Handle.all.all          := 15;                              -- ERROR:
                               -- access-to-constant value is read only

  Double_Indirect.all.all := 42;                              -- OK

  Access_Const_Rec.Field     := 12345;                        -- ERROR:
                               -- access-to-constant value is read only
  Access_Const_Rec.all.Field := 14321;                        -- ERROR:
                               -- access-to-constant value is read only
  Access_Const_Rec.all       := A_Rec;                        -- ERROR:
                               -- access-to-constant value is read only
  Access_Arr.all(1)          := A_Rec;                        -- ERROR:
                               -- access-to-constant value is read only

  Rec_With_Const_Comp.Access_Field := Access_Const_Rec;          -- OK.
  Arr_Of_Acc_Const (1)             := Access_Const_Rec;
  Arr_Acc_Const_Rec (1)            := Access_Const_Rec;

  Rec_With_Const_Comp.Access_Field.all := (Field => 10101);   -- ERROR:
                               -- access-to-constant value is read only
  Arr_Of_Acc_Const (1).all := (Field => 12525);               -- ERROR:
                               -- access-to-constant value is read only
  Arr_Rec_Acc_Const (1).Access_Field.all := (Field => 12312); -- ERROR:
                              -- access-to-constant value is read only

  Arr_Acc_Const_Rec (1).all.Field := 10100;                   -- ERROR:
                               -- access-to-constant value is read only

  An_Array_Reference := An_Aliased_Array(1..5)'Access;        -- ERROR:
       -- A slice is not permitted as the prefix of an Access attribute
                                                          -- reference.
  Aliased_Array_Ref  := Aliased_Aliased_Array(5..10)'Access;  -- ERROR:
       -- A slice is not permitted as the prefix of an Access attribute
                                                          -- reference.

  I_Do: declare
    Slice : Array_Type renames An_Aliased_Array(1..50);
    Copy  : Array_Type renames An_Aliased_Array;
    Array_Element  : Integer renames An_Array_Of_Aliased(1);
    Record_Element : Integer renames A_Record.A_Component;
    Standalone : Integer renames Non_Aliased_Integer;
    NA_Array_Component : Integer renames An_Aliased_Array(1);
    NA_Record_Component : Integer renames A_Rec.Field;

    -- view conversions
    package P is

      type T is tagged record
	     F : Integer;
      end record;

      type New_T is new T with record
	     New_F : Boolean;
      end record;

      A_T           : aliased T;
      A_New_T       : aliased New_T;
      Another_New_T : New_T;  -- not aliased

      type T_Acc is access all T'Class;

      A_T_Acc : T_Acc;
    end P;

  begin
    Access_Constant := Array_Element'Access;                  -- OK
    General_Access  := Record_Element'Access;                 -- OK
    General_Access  := Standalone'Access;                     -- ERROR:
       -- The renamed object is not aliased
    General_Access  := NA_Array_Component'Access;             -- ERROR:
       -- The renamed component is not aliased
    General_Access  := NA_Record_Component'Access;            -- ERROR:
       -- The renamed component is not aliased
    An_Array_Reference := Slice'Access;                       -- ERROR:
       -- A slice is not permitted as the prefix of an Access attribute
                                                          -- reference.
    An_Array_Reference := Copy'Access;                        -- OK

    -- Implicit view conversions:
    P.A_T_Acc := P.A_T'Access;                                -- OK
    P.A_T_Acc := P.Another_New_T'Access;                      -- ERROR:
                                            -- Object is not aliased
    P.A_T_Acc := P.A_New_T'Access;                            -- OK

    -- Explicit view conversions:
    P.A_T_Acc := P.T'Class(P.A_T)'Access;                     -- OK
    P.A_T_Acc := P.T'Class(P.Another_New_T)'Access;           -- ERROR:
                                            -- Object is not aliased
    P.A_T_Acc := P.T'Class(P.A_New_T)'Access;                 -- OK

    -- Value conversions:
    General_Access := Integer(Aliased_Integer)'Access;        -- ERROR:
                                            -- Value conversion
    An_Array_Reference :=
                   Array_Type(An_Aliased_Array)'Access;       -- ERROR:
                                            -- Value conversion
    declare
        subtype Con_Rec is Record_Type(10);
        My_Rec : aliased Con_Rec;
        Sink : access Con_Rec;
    begin
        Sink := Con_Rec(My_Rec)'Access;                       -- ERROR:
                                            -- Value conversion
    end;

    -- Dereferences:
    Access_Constant := General_Access.all'Access;             -- OK.
    General_Access := Aliased_Array_Ref.all(1)'Access;        -- OK.
    General_Access := Pool_Acc_Obj.all'Access;                -- OK.

  end I_Do;

  Update( Access_Const_Rec.all.Field );                       -- ERROR:
                               -- access-to-constant value is read only
end B3A0001;
