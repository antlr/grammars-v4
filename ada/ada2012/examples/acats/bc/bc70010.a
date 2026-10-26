-- BC70010.A
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
--      Check that an actual instance of a generic formal package is rejected
--      if its actuals do not match the corresponding actuals in the formal
--      package actual part. Specifically, check that, for formal subtypes,
--      the actuals must denote statically matching subtypes.
--
-- TEST DESCRIPTION:
--      Two subtypes of the same type statically match if their constraints
--      statically match. Two constraints statically match if they are both
--      null constraints, both are static and have equal corresponding bounds
--      or discriminant values, or both are nonstatic and result from the same
--      elaboration of a constraint of a subtype indication or the same
--      evaluation of a range of a discrete subtype definition.
--
--      This test checks matching for actual subtypes with range, index and
--      discriminant constraints.
--
--      The formal types used are private, derived, array, and discrete.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

package BC70010 is

--
-- CASE A:
--    Formal  - Private type
--    Actuals - Record types with defaulted discriminants
--

   -- Support declarations:

   subtype Length is Natural range 0 .. 80;

   type Rec_Type (Len : Length := Length'Last) is record
      Str : String (1 .. Len);
   end record;

   MaxLen : Length := 80;

   subtype Rec_Max_Length is Rec_Type (Length'Last);
   subtype Rec_Length_80  is Rec_Type (80);
   subtype Rec_Length_25  is Rec_Type (25);
   subtype Rec_80_Dyn     is Rec_Type (MaxLen);


   -- Template for formal package:

   generic                                          -- Template declaring a
      type Private_Type is private;                 -- formal private type.
   package Private_Type_Template is end;


   -- Generic declaring a formal package:

   generic                                          -- Formal package actual is
      with package Formal_Package is new            -- a record subtype with a
        Private_Type_Template (Rec_Max_Length);     -- discriminant (value 80).
   package Private_Type_Record is end;


   -- Instances to be passed as actuals to the formal package:

   package Private_Type_Instance_Record_Pass        -- Instance actual is a
     is new Private_Type_Template (Rec_Length_80);  -- record subtype with a
                                                    -- discriminant (value 80).

   package Private_Type_Instance_Record_Fail_1      -- Instance actual is a
     is new Private_Type_Template (Rec_Length_25);  -- record subtype with a
                                                    -- discriminant (value 25).

   package Private_Type_Instance_Record_Fail_2      -- Instance actual is a
     is new Private_Type_Template (Rec_Type);       -- record subtype with a
                                                    -- disc. (null constraint).

   package Private_Type_Instance_Record_Fail_Dyn    -- Instance actual is a
     is new Private_Type_Template (Rec_80_Dyn);     -- record subtype with a
                                                    -- discriminant (value 80).




   -- Instances of generics declaring formal packages:

   package Private_Type_Rec_Discrim_Matching_Constraint is new
     Private_Type_Record (Private_Type_Instance_Record_Pass);         -- OK.

   package Private_Type_Rec_Discrim_Mismatched_Constraint is new
     Private_Type_Record (Private_Type_Instance_Record_Fail_1);       -- ERROR:
                          -- Constraints do not have equal discriminant values.

   package Private_Type_Rec_Discrim_Default_Matches_Constraint is new
     Private_Type_Record (Private_Type_Instance_Record_Fail_2);       -- ERROR:
                              -- Formal package actual has null constraint, but
                              -- instance actual does not.

   package Private_Type_Rec_Discrim_Dynamic_Matches_Constraint is new
     Private_Type_Record (Private_Type_Instance_Record_Fail_Dyn);     -- ERROR:
                   -- Discriminant constraint of instance actual is non-static,
                   -- but that of formal package actual is static.

--
-- CASE B:
--    Formal  - Derived type
--    Actuals - Tagged types with discriminants
--

   -- Support declarations:

   type Tagged_Type (Len : Length) is tagged record
      Str : String (1 .. Len);
   end record;

   type Tagged_Derived is new Tagged_Type with null record;


   -- Template for formal package:

   generic                                          -- Template declaring a
      type Derived_Type (<>) is new Tagged_Type     -- formal derived type.
        with private;
   package Derived_Type_Template is end;


   -- Generic declaring a formal package:

   generic                                          -- Formal package actual is
      with package Formal_Package is new            -- a tagged subtype with a
        Derived_Type_Template (Tagged_Type);        -- disc. (null constraint).
   package Derived_Type_Tagged is end;


   -- Instances to be passed as actuals to the formal package:

   package Derived_Type_Instance_Tagged_Pass        -- Instance actual is a
     is new Derived_Type_Template (Tagged_Type);    -- tagged subtype with a
                                                    -- disc. (null constraint).

   package Derived_Type_Instance_Tagged_Fail        -- Instance actual is a
     is new Derived_Type_Template (Tagged_Derived); -- tagged derivative with
                                                    -- inherited disc.


   -- Instances of generics declaring formal packages:

   package Derived_Type_Tag_Discrim_Matching_Null_Constraint is new
     Derived_Type_Tagged (Derived_Type_Instance_Tagged_Pass);         -- OK.

   package Derived_Type_Tag_Discrim_Different_Type is new
     Derived_Type_Tagged (Derived_Type_Instance_Tagged_Fail);         -- ERROR:
                       -- Actuals are of different types in the same hierarchy.


--
-- CASE C:
--    Formal  - Array type
--    Actuals - Array types
--

   -- Support declarations:

   type Index is new Natural range 1 .. 10;

   N : Index := 3;

   subtype Index2 is Index range 1 .. 2;
   subtype Index3 is Index range 1 .. 3;
   subtype IndexD is Index range 1 .. N;   -- Nonstatic range.

   type Matrix       is array (Index range <>, Index range <>) of Integer;
   type Mat_DiffType is array (Index3, Index3) of Integer;

   subtype Matrix_3x3  is Matrix (Index3, Index3);
   subtype Matrix_2x2  is Matrix (Index2, Index2);
   subtype Mat_3x3_Dyn is Matrix (IndexD, IndexD);


   -- Template for formal package:

   generic
      type Indices is range <>;
      type Array_Type is array (Indices, Indices)  -- Template declaring a
        of Integer;                                -- formal array type.
   package Array_Type_Template is end;


   -- Generic declaring a formal package:

   generic                                       -- Formal package actual is
      with package Formal_Package is new         -- an array subtype with
        Array_Type_Template (Index3,Matrix_3x3); -- index constraints
   package Array_Type_Array is end;              -- (1..3, 1..3).


   -- Instances to be passed as actuals to the formal package:

   package Array_Type_Instance_Pass is new       -- Instance actual is an array
     Array_Type_Template (Index3, Matrix_3x3);   -- subtype with indices
                                                 -- (1..3, 1..3).

   package Array_Type_Instance_Fail is new       -- Instance actual is an array
     Array_Type_Template (Index2, Matrix_2x2);   -- subtype with indices
                                                 -- (1..2, 1..2).

   package Array_Type_Instance_Fail_Type is new  -- Instance actual is an array
     Array_Type_Template (Index3, Mat_DiffType); -- subtype with indices
                                                 -- (1..3, 1..3).

   package Array_Type_Instance_Fail_Dyn is new   -- Instance actual is an array
     Array_Type_Template (IndexD, Mat_3x3_Dyn);  -- subtype with indices
                                                 -- (1..3, 1..3).


   -- Instances of generics declaring formal packages:

   package Array_Type_Matching_Index_Constraints
     is new Array_Type_Array (Array_Type_Instance_Pass);              -- OK.

   package Array_Type_Mismatched_Index_Constraints
     is new Array_Type_Array (Array_Type_Instance_Fail);              -- ERROR:
                -- Range of scalar actual and index constraints of array actual
                -- do not match.

   package Array_Type_Actual_Has_Different_Type
     is new Array_Type_Array (Array_Type_Instance_Fail_Type);         -- ERROR:
                 -- Array actual has different type than formal package actual.

   package Array_Type_Actual_Has_Dynamic_Constraints
     is new Array_Type_Array (Array_Type_Instance_Fail_Dyn);          -- ERROR:
                    -- Index constraints of instance actual are non-static, but
                    -- those of formal package actual are static.

--
-- CASE D:
--    Formal  - Discrete type
--    Actuals - Enumeration types
--

   -- Support declarations:

   type Week is (Sat, Sun, Mon, Tue, Wed, Thu, Fri); 

   DayOff : Week := Sun;

   subtype Weekend  is Week range Sat .. Sun;
   subtype Workweek is Week range Mon .. Fri;
   subtype Days_Off is Week range Sat .. DayOff;


   -- Template for formal package:

   generic                                          -- Template declaring a
      type Discrete_Type is (<>);                   -- formal discrete type.
   package Discrete_Type_Template is end;


   -- Generic declaring a formal package:

   generic                                          -- Formal package actual is
      with package Formal_Package is new            -- an enumeration type with
        Discrete_Type_Template (Weekend);           -- a range constraint
   package Discrete_Type_Enum is end;               -- (Sat..Sun).


   -- Instances to be passed as actuals to the formal package:

   package Discrete_Type_Instance_Pass              -- Instance actual is an
     is new Discrete_Type_Template (Weekend);       -- an enumeration type with
                                                    -- range (Sat..Sun).

   package Discrete_Type_Instance_Fail              -- Instance actual is an
     is new Discrete_Type_Template (Workweek);      -- an enumeration type with
                                                    -- range (Mon..Fri).

   package Discrete_Type_Instance_Fail_Dyn          -- Instance actual is an
     is new Discrete_Type_Template (Days_Off);      -- an enumeration type with
                                                    -- range (Sat..Sun).


   -- Instances of generics declaring formal packages:

   package Discrete_Type_Matching_Range_Constraints is new
     Discrete_Type_Enum (Discrete_Type_Instance_Pass);                -- OK.

   package Discrete_Type_Mismatched_Range_Constraints is new
     Discrete_Type_Enum (Discrete_Type_Instance_Fail);                -- ERROR: 
                                             -- Range constraints do not match.

   package Discrete_Type_Dynamic_Constraint is new
     Discrete_Type_Enum (Discrete_Type_Instance_Fail_Dyn);            -- ERROR: 
                          -- Range constraint of instance actual is non-static,
                          -- but that of formal package actual is static.


end BC70010;
