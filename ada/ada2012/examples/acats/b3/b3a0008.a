-- B3A0008.A
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
--    Check that a null_exclusion cannot be given in a construct if the
--    subtype mark is a formal access type that excludes null. Check
--    in subtype indications (in objects, components, subtypes, derived types,
--    and allocators), discriminants, parameters, function result subtypes,
--    object renamings, and formal objects.
--
--    Note: We don't test return subtypes in extended returns, as these would
--    necessarily fail the static matching check, and thus we can't test
--    the intended error.
--
--  CHANGE HISTORY:
--     24 Apr 2008 RLB Created test.
--!

generic
   type Not_Null_Int_Ptr is not null access Integer;
package B3A0008 is
   type    New_Not_Null_Int_Ptr is new Not_Null_Int_Ptr;           -- OK.
   subtype Not_Null_Int_Ptr_S1  is Not_Null_Int_Ptr;               -- OK.
   subtype Not_Null_Int_Ptr_S2  is New_Not_Null_Int_Ptr;           -- OK.

   -- Object_Declaration tests:

   O_12 : not null Not_Null_Int_Ptr;                               -- ERROR:
   O_13 : not null New_Not_Null_Int_Ptr;                           -- ERROR:
   O_14 : not null Not_Null_Int_Ptr_S1;                            -- ERROR:
   O_15 : not null Not_Null_Int_Ptr_S2;                            -- ERROR:
   O_22 : Not_Null_Int_Ptr;                                        -- OK.
   O_23 : New_Not_Null_Int_Ptr;                                    -- OK.

   -- Component_Definition tests:

   type Rec_1 is record
      C2 : not null Not_Null_Int_Ptr;                              -- ERROR:
      C3 : not null New_Not_Null_Int_Ptr;                          -- ERROR:
      C4 : not null Not_Null_Int_Ptr_S1;                           -- ERROR:
      C5 : not null Not_Null_Int_Ptr_S2;                           -- ERROR:
   end record;

   type Arr_2 is array (1..4) of not null Not_Null_Int_Ptr;        -- ERROR:
   type Arr_3 is array (1..4) of not null New_Not_Null_Int_Ptr;    -- ERROR:
   type Arr_4 is array (1..4) of not null Not_Null_Int_Ptr_S1;     -- ERROR:
   type Arr_5 is array (1..4) of not null Not_Null_Int_Ptr_S2;     -- ERROR:

   protected Prot is
      procedure Check;
   private
      C2 : not null Not_Null_Int_Ptr;                              -- ERROR:
      C3 : not null New_Not_Null_Int_Ptr;                          -- ERROR:
      C4 : not null Not_Null_Int_Ptr_S1;                           -- ERROR:
      C5 : not null Not_Null_Int_Ptr_S2;                           -- ERROR:
   end Prot;

   -- Subtype_Declaration tests:

   subtype S_12 is not null Not_Null_Int_Ptr;                      -- ERROR:
   subtype S_13 is not null New_Not_Null_Int_Ptr;                  -- ERROR:
   subtype S_14 is not null Not_Null_Int_Ptr_S1;                   -- ERROR:
   subtype S_15 is not null Not_Null_Int_Ptr_S2;                   -- ERROR:

   -- Derived_Type_Definition tests:

   type T_12 is new not null Not_Null_Int_Ptr;                     -- ERROR:
   type T_13 is new not null New_Not_Null_Int_Ptr;                 -- ERROR:
   type T_14 is new not null Not_Null_Int_Ptr_S1;                  -- ERROR:
   type T_15 is new not null Not_Null_Int_Ptr_S2;                  -- ERROR:

   -- Allocator tests:

   type NN_Int_Ptr_Ptr is access Not_Null_Int_Ptr;

   IPP_2 : NN_Int_Ptr_Ptr := new not null Not_Null_Int_Ptr;        -- ERROR:
   IPP_4 : NN_Int_Ptr_Ptr := new not null Not_Null_Int_Ptr_S1;     -- ERROR:

   -- Discriminant tests:

   type Rec_2 (D2 : not null Not_Null_Int_Ptr;                     -- ERROR:
               D3 : not null New_Not_Null_Int_Ptr;                 -- ERROR:
               D4 : not null Not_Null_Int_Ptr_S1;                  -- ERROR:
               D5 : not null Not_Null_Int_Ptr_S2)                  -- ERROR:
   is record
     null;
   end record;

   -- Parameter tests:

   procedure Proc_1
              (P2 : not null Not_Null_Int_Ptr;                     -- ERROR:
               P3 : not null New_Not_Null_Int_Ptr;                 -- ERROR:
               P4 : not null Not_Null_Int_Ptr_S1;                  -- ERROR:
               P5 : not null Not_Null_Int_Ptr_S2) is null;         -- ERROR:

   -- Function result subtype tests:

   function Func_2 return not null Not_Null_Int_Ptr;               -- ERROR:
   function Func_3 return not null New_Not_Null_Int_Ptr;           -- ERROR:
   function Func_4 return not null Not_Null_Int_Ptr_S1;            -- ERROR:
   function Func_5 return not null Not_Null_Int_Ptr_S2;            -- ERROR:

   -- Object renaming tests:

   Ren_12 : not null Not_Null_Int_Ptr renames O_22;                -- ERROR:
   Ren_13 : not null New_Not_Null_Int_Ptr renames O_23;            -- ERROR:
   Ren_14 : not null Not_Null_Int_Ptr_S1 renames O_22;             -- ERROR:
   Ren_15 : not null Not_Null_Int_Ptr_S2 renames O_23;             -- ERROR:

   -- Formal object tests:

   generic
       O2 : not null Not_Null_Int_Ptr;                             -- ERROR:
       O3 : not null New_Not_Null_Int_Ptr;                         -- ERROR:
       O4 : not null Not_Null_Int_Ptr_S1;                          -- ERROR:
       O5 : not null Not_Null_Int_Ptr_S2;                          -- ERROR:
   package Gen is
   end Gen;

end B3A0008;
