-- B3A2002.A
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
--      Check that:
--      'Access is not defined for non-aliased objects.
--      For X'Access of a general access type A, if A is an access-to-constant
--      type, X can be either a constant or a variable.
--      For X'Access of a general access type A, if A is an access-to-variable
--      type, X must denote the view of a variable.
--
--      Check for cases where X is a:
--         (a) Formal in parameter of a tagged type.
--         (b) Generic formal in parameter of a tagged type.
--         (c) Formal in parameter of a composite type with aliased components.
--         (d) Function return value of a composite type with aliased
--             components.
--
-- TEST DESCRIPTION:
--      Declare a package which declares non-aliased views.  The non-aliased
--      view include non-aliased constant, non-aliased scalar, non-aliased 
--      record declarations, renaming of non-aliased views, non tagged formal
--      parameter, and view conversions of non-aliased view.  Declare 
--      access-to-constant types.  The access-to-constant types include a 
--      formal in parameter of a tagged type, a generic formal in parameter
--      of a tagged type, and a function return value of a composite type with
--      aliased components. Verify that compiler generates errors for all 
--      cases as described in the objective.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      21 Apr 95   SAIC    Added formal tagged parameter cases.
--      20 Nov 95   SAIC    Update and repair for ACVC 2.0.1
--
--!

procedure B3A2002 is  

   -- 'Access is not defined for non-aliased objects. 

   -- Non-aliased constant.
   Constant_Int : constant Integer := 23;       
   type Acc_To_Constant_Int is access constant Integer;   
   Acc_To_Constant_Int_Var :  Acc_To_Constant_Int 
                           := Constant_Int'Access;                    -- ERROR:
                                                -- Constant_Int is not aliased.
   -- Non-aliased scalar.
   type Color is (Black, Yellow, White);
   type Acc_To_Color is access all Color;
   type Rec_Of_Color (D : access Color) is limited
     record
        Field : Integer;
     end record;
   Color_Var : Color;
   Rec_Of_Color_Var :  Rec_Of_Color (D => Color_Var'Access);          -- ERROR:
                                                   -- Color_Var is not aliased.

   -- Non-aliased record.
   type Rec is
     record
        Field : Boolean;
   end record;
   Rec_Var : Rec;   
   type Acc_To_Rec is access all Rec;
   Acc_To_Rec_Var :  Acc_To_Rec 
                  := Rec_Var'Access;                                  -- ERROR:
                                                  -- Record_Var is not aliased.
   -- Renaming of a non-aliased record.
   Renamed_Rec_Var : Rec renames Rec_Var;   
   Acc_To_Renamed_Rec_Var :  Acc_To_Rec 
                          := Renamed_Rec_Var'Access;                  -- ERROR:
                                    -- Renamed object (Rec_Var) is not aliased.
   
   -- Renaming of an aliased record.
   Aliased_Rec_Var : aliased Rec;
   Renamed_Aliased_Rec_Var : Rec renames Aliased_Rec_Var;
   Acc_To_Renamed_Aliased_Rec_Var :  Acc_To_Rec
                                  := Renamed_Aliased_Rec_Var'Access;  -- OK.
                                   -- Renaming of an aliased object is aliased.

   -- Aliased record component.
   type Location is range 1 .. 100;
   type Handle is access all Location;
   type Record_With_Aliased_Component is record
      Handle_Field : aliased Handle;
   end record;
   type Acc_To_Handle is access all Handle;
   Rec_Obj_With_Aliased_Component : Record_With_Aliased_Component;
   Acc_To_Handle_Obj : Acc_To_Handle 
     := Rec_Obj_With_Aliased_Component.Handle_Field'Access;           -- OK.
                                                    -- Record field is aliased.
   type Acc_To_Record is access all Record_With_Aliased_Component;
   Acc_To_Record_Obj :  Acc_To_Record 
                     := Rec_Obj_With_Aliased_Component'Access;        -- ERROR:
                                           -- Only record component is aliased.

   -- Non-aliased record component.
   type Rec_2 is record
      Component : Record_With_Aliased_Component;
   end record;
   Rec_Obj_2 : aliased Rec_2;
   Acc_to_Record_Obj_2 : Acc_To_Record := Rec_Obj_2.Component'Access; -- ERROR:
                                        -- Referenced component is not aliased.
   type Rec_With_Acc_Component is record
      Component : Acc_To_Handle;
   end record;
   Rec_With_Acc_Comp_Obj : Rec_With_Acc_Component := (Component => new Handle);
   Acc_To_Handle_Obj_3 :  Acc_To_Handle
                       := Rec_With_Acc_Comp_Obj.Component.all'Access; -- OK.
                           -- Dereference of access-to-object value is aliased.

   type Record_With_Nested_Aliased_Component is tagged record
      Component : Record_With_Aliased_Component;
   end record;
   Rec_W_Nested_Aliased_Comp : Record_With_Nested_Aliased_Component;
   Acc_To_Handle_Obj_4 : Acc_To_Handle 
     := Rec_W_Nested_Aliased_Comp.Component.Handle_Field'Access;      -- OK.
                                             -- Referenced component is aliased.

   type Array_Handle is array (1 .. 5) of Handle;
   type Acc_to_Arr_Handle is access all Array_Handle;
   type Rec_With_Arr_Aliased_Component is record
      Arr_Aliased_Component : aliased Array_Handle;
   end record;
   type Acc_To_Rec_W_Aliased_Comp is
                      access all Rec_With_Arr_Aliased_Component;
   Rec_With_Arr_Aliased_Component_Var : Rec_With_Arr_Aliased_Component;
   type Acc_To_Array_Handle is access all Array_Handle;
   Acc_To_Handle_Obj_5 : Acc_To_Array_Handle := 
     Rec_With_Arr_Aliased_Component_Var.Arr_Aliased_Component'Access; -- OK.
                                             -- Referenced component is aliased.
   type Arr_Of_Aliased_Rec is array (1 .. 15) of 
                                Rec_With_Arr_Aliased_Component;
   type Acc_To_Arr_Of_Aliased_Rec is access all Arr_of_Aliased_Rec;
   type Array_Of_Aliased_Ptrs is array (1 .. 5) of aliased Arr_Of_Aliased_Rec;
   type Acc_to_Aliased_Ptr_Arr is access all Array_Of_Aliased_Ptrs;
   type Rec_3 is record
      Field3  : Array_Of_Aliased_Ptrs;
   end record;
   type Array_of_Records is array (1 .. 30) of Rec_3;
   type Rec_4 is record
      Field4 : Array_of_Records;
    end record;
   Rec_Obj4 : Rec_4;
   Acc_To_Aliased_Ptr_Arr_Obj_1 : Acc_To_Arr_Of_Aliased_Rec :=
                   Rec_Obj4.Field4(15).Field3(4)'Access;              -- OK.
                                             -- Referenced component is aliased.
   Acc_To_Record_With_Aliased_Comp : Acc_To_Rec_W_Aliased_Comp :=
                   Rec_Obj4.Field4(15).Field3(3)(4)'Access;           -- ERROR:
                                             -- Array elements are not aliased.
   Acc_To_Aliased_Ptr_Arr_Obj_2 : Acc_To_Aliased_Ptr_Arr :=
                   Rec_Obj4.Field4(15).Field3'Access;                 -- ERROR:
                                                -- Record field is not aliased.
   Acc_To_Arr_Handle_Obj : Acc_To_Arr_Handle :=
     Rec_Obj4.Field4(10).Field3(1)(5).Arr_Aliased_Component'Access;   -- OK.
                                             -- Referenced component is aliased.
   Acc_To_Handle_Obj_2 : Acc_To_Handle :=
    Rec_Obj4.Field4(10).Field3(1)(5).Arr_Aliased_Component(3)'Access; -- ERROR:
                                               -- Array element is not aliased.
   -------------------------------------------------------
   type Not_Tagged is
     record
        Field : Integer;
   end record;
   Not_Tagged_Var : Not_Tagged;   
   type Acc_To_Not_Tagged is access all Not_Tagged;

   -- Non tagged formal parameter.
   procedure Not_Tagged_Proc (P : in out Not_Tagged) is
      type Tag_Acc is access all Not_Tagged;            
      Local : Tag_Acc;                        
   begin                                      
      Local := P'Access;                                              -- ERROR:
   end Not_Tagged_Proc;      -- Parameter P is not aliased (not a tagged type).
                              
   type Tag is tagged record
      Component : Not_Tagged;
   end record;

   -- Tagged formal parameter.
   procedure Tagged_Proc (P : in out Tag) is
      type Acc_Tag is access all Tag; 
      Local : Acc_Tag;                        
   begin                                      
      Local := P'Access;                                              -- OK.
   end Tagged_Proc;                  -- Parameter P is a tagged type (aliased).

   type Acc_To_Tag is access all Tag;
   type New_Tagged is new Tag with record
      Component_2 : Acc_To_Tag;
   end record;
   type Acc_To_New_Tagged is access all New_Tagged;
   Acc_To_New_Tag_Obj : Acc_To_New_Tagged;
   Acc_To_Tag_Obj :  Acc_To_Tag 
                  := Tag (Acc_To_New_Tag_Obj.all)'Access;             -- OK.
                                       -- View conversion of an aliased object.

   -------------------------------------------------------
   -- Array slice.
   type Short is range 1 .. 50;
   type Array_Of_Aliased_Int is array (Short range <>) of aliased Integer;
   type Acc_To_Array_Of_Aliased_Int is access all Array_Of_Aliased_Int;
   type Acc_To_Slice_Aliased_Int is access all Array_Of_Aliased_Int (5..10);
   Array_Aliased :  aliased Array_Of_Aliased_Int
                 := (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20);
   Var_1         :  Acc_To_Array_Of_Aliased_Int  
                 := Array_Aliased'Access;                             -- OK.
   Var_2         :  Acc_To_Slice_Aliased_Int                 
                 := Array_Aliased(5..10)'Access;                      -- ERROR:
                                             -- Array slices are never aliased.
   Var_3         :  Acc_to_Array_Of_Aliased_Int
                 := Array_Aliased(5..10)'Access;                      -- ERROR:
                                             -- Array slices are never aliased.
   type Array_Of_Int is array (Short range <>) of Integer;
   type Acc_To_Array_Of_Int is access all Array_Of_Int;
   type Acc_To_Slice_Of_Int is access all Array_Of_Int (1..5);
   Aliased_Array_Var : aliased Array_Of_Int := (1,2,3,4,5,6,7,8,9,10);
   Var_4         : Acc_To_Array_Of_Int := Aliased_Array_Var'Access;   -- OK.
   Var_5         : Acc_To_Slice_Of_Int 
                 := Aliased_Array_Var(1..5)'Access;                   -- ERROR:
                                             -- Array slices are never aliased.
   -- Non-aliased array of aliased components.
   type Array_Array_Of_Aliased_Int is array (Short range <>) of 
     Array_Of_Aliased_Int (1 .. 7);
   type Acc_To_Array_Array_Of_Aliased_Int is 
     access all Array_Array_Of_Aliased_Int;
   Array_7             :  Array_Of_Aliased_Int       := (1,2,3,4,5,6,7);
   Array_Array_Aliased :  Array_Array_Of_Aliased_Int := (1 .. 15 => Array_7);
   Var_6               :  Acc_To_Array_Array_Of_Aliased_Int  
                       := Array_Array_Aliased'Access;                 -- ERROR:
                                         -- Array_Array_Aliased is not aliased.

   -------------------------------------------------------
   -- Array of pointers.
   type Array_Of_Acc_To_Tag is array (1 .. 5) of Acc_To_Tag;
   type Acc_To_Array_Of_Acc_To_Tag is access all Array_Of_Acc_To_Tag;
   Array_Of_Acc_To_Tag_Var        :  Array_Of_Acc_To_Tag;
   Acc_To_Array_Of_Acc_To_Tag_Var :  Acc_To_Array_Of_Acc_To_Tag
                                  := new Array_Of_Acc_To_Tag;

   Var_7               :  Acc_To_Array_Of_Acc_To_Tag
                       := Acc_To_Array_Of_Acc_To_Tag_Var.all'Access;  -- OK.

   Var_8               :  Acc_To_Array_Of_Acc_To_Tag
                       := Array_Of_Acc_To_Tag_Var'Access;             -- ERROR:
                                     -- Array_Of_Acc_To_Tag_Var is not aliased.

   type Another_Tag is tagged record
      Component : Not_Tagged;
   end record;

   Aliased_Tag : aliased Another_Tag;

   type Acc_To_Another_Tag is access all Another_Tag;
   type Array_Acc_To_Another_Tag is array (Positive range <>) of 
     Acc_To_Another_Tag;

   Array_Of_Acc_To_Another_Tag_Var :  Array_Acc_To_Another_Tag (1 .. 5) 
                                   := (others => Aliased_Tag'Access); -- OK.

   Var_9  :  Acc_To_Another_Tag                                  
          := Array_Of_Acc_To_Another_Tag_Var(3).all'Access;           -- OK.

   Var_10 :  Acc_To_Another_Tag                                       
          := Array_Of_Acc_To_Another_Tag_Var(3)'Access;               -- ERROR:
                             -- Array_Of_Acc_To_Another_Tag_Var is not aliased.

   -------------------------------------------------------
   -- Objects of access to variable types may only be assigned references 
   -- to variables (not constants). 

   -- Access-to-variable type.
   type Acc_To_Int is access all Integer;   
   Aliased_Int : aliased Integer;       
   -- Access-to-constant type.
   type Acc_To_Const_Int is access constant Integer;   
   Aliased_Constant_Int : aliased constant Integer := 24;       
   Acc_To_Int_Var_1 :  Acc_To_Const_Int 
                    := Aliased_Int'Access;                            -- OK.
   Acc_To_Int_Var_2 :  Acc_To_Const_Int 
                    := Aliased_Constant_Int'Access;                   -- OK.
   Acc_To_Int_Var_3 : Acc_To_Int 
                    := Aliased_Int'Access;                            -- OK.
   Acc_To_Int_Var_4 : Acc_To_Int 
                    := Aliased_Constant_Int'Access;                   -- ERROR:
                      -- Aliased constant can't be assigned to object of access
                      -- to variable type.

   -- Access-to-variable as formal in parameter of a tagged type.
   -- Formal parameters of tagged type are aliased by definition.

   procedure Proc (P : Tag) is
      type Access_To_Tag is access all Tag;
      Ptr : Access_To_Tag;
   begin
      Ptr := P'Access;                                                -- ERROR:
    end Proc;                                          -- P is a constant view.


   -- Access-to-variable as generic formal in parameter of a tagged type.
   generic
      type Formal_Access_To_Tag is access all Tag;   
      FObj: in Tag;
   package Gen_Pkg is
      Ptr : Formal_Access_To_Tag := FObj'Access;                      -- ERROR:
    end Gen_Pkg;                                    -- FObj is a constant view.

   -- Access-to-variable as formal in parameter of a composite type with
   -- aliased components.
   type RWTag is record
      C : aliased Tag;
   end record;

   subtype Index is Natural range 1 .. 10;
   type Array_Of_Alliased_RWTag is array (Index) of aliased RWTag;

   function Func (P : RWTag) return Array_Of_Alliased_RWTag is
      type Access_To_Tag is access all Tag;
      Ptr : Access_To_Tag := P.C'Access;                              -- ERROR:
                                                     -- P.C is a constant view.
      Var : Array_Of_Alliased_RWTag := (others => P);
   begin
      return Var;
   end Func;

   type AccArray_Of_Alliased_RWTag is access all Array_Of_Alliased_RWTag;
   RPtr      : AccArray_Of_Alliased_RWTag;
   RWTag_Var : RWTag;

begin
   -- Access-to-variable as function return value of a composite type with
   -- aliased components.
   RPtr := Func(RWTag_Var)(3)'Access;                                 -- ERROR:
                                        -- Func(Rec_Var)(3) is a constant view.
end B3A2002;
