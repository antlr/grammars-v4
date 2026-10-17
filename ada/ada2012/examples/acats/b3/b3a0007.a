-- B3A0007.A
--
--                            Grant of Unlimited Rights
--
--    AdaCore holds unlimited rights in the software and documentation
--    contained herein. Unlimited rights are the same as those granted
--    by the U.S. Government for older parts of the Ada Conformity
--    Assessment Test Suite, and are defined in DFAR 252.227-7013(a)(19).
--    By making this public release, AdaCore intends to confer upon all
--    recipients unlimited rights equal to those held by the Ada Conformity
--    Assessment Authority. These rights include rights to use, duplicate,
--    release or disclose the released technical data and computer software
--    in whole or in part, in any manner and for any purpose whatsoever,
--    and to have or permit others to do so.
--
--                                   DISCLAIMER
--
--    ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--    DISCLOSED ARE AS IS. ADACORE MAKES NO EXPRESS OR IMPLIED WARRANTY AS
--    TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE SOFTWARE,
--    DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE OR
--    DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
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
--
--    This test is based on one submitted by AdaCore; AdaCore retains the
--    copyright on the test.
--*
--  OBJECTIVE:
--    Check that a null_exclusion cannot be given in a construct if the
--    subtype mark is not an access type or if it excludes null. Check
--    in subtype indications (in objects, components, subtypes, derived types,
--    and allocators), discriminants, parameters, function result subtypes,
--    object renamings, and formal objects.
--
--    Note: We don't test return subtypes in extended returns, as these would
--    necessarily fail the static matching check, and thus we can't test
--    the intended error.
--
--  CHANGE HISTORY:
--     18 Mar 2004 JM  Initial Version.
--     02 Apr 2005 JM  Add more wrong cases.
--     24 Apr 2008 RLB Converted to ACATS test, adding test cases.
--     29 Apr 2008 RLB Added Priv_Ptr cases.
--     27 Dec 2013 RLB Commented out case which violates the binding
--                     interpretation AI05-0104-1.
--     28 Feb 2014 RLB Replaced test case for ACATS 4.0.
--!

procedure B3A0007 is
   type    Not_Ptr              is (Nil, Cold, Hot);
   package P is
      type Priv_Ptr is private;
      A_Priv_Ptr : constant Priv_Ptr;
   private
      type Priv_Ptr is access Character;
      A_Priv_Ptr : constant Priv_Ptr := new Character'('B');
   end P;
   type    Int_Ptr              is access Integer;
   type    Not_Null_Int_Ptr     is not null access Integer;
   type    New_Not_Null_Int_Ptr is new not null Int_Ptr;
   subtype Not_Null_Int_Ptr_S1  is not null Int_Ptr;
   subtype Not_Null_Int_Ptr_S2  is Not_Null_Int_Ptr;
   subtype Not_Null_Int_Ptr_S3  is New_Not_Null_Int_Ptr;

   -- Object_Declaration tests:

   O_10 : not null Int_Ptr;                                        -- OK.
   O_11 : not null Not_Ptr;                                        -- ERROR:
   O_12 : not null P.Priv_Ptr;                                     -- ERROR:
   O_13 : not null Not_Null_Int_Ptr;                               -- ERROR:
   O_14 : not null New_Not_Null_Int_Ptr;                           -- ERROR:
   O_15 : not null Not_Null_Int_Ptr_S1;                            -- ERROR:
   O_16 : not null Not_Null_Int_Ptr_S2;                            -- ERROR:
   O_17 : not null Not_Null_Int_Ptr_S3;                            -- ERROR:
   -- For renaming tests, below:
   O_21 : Not_Ptr;
   O_22 : P.Priv_Ptr;
   O_23 : Not_Null_Int_Ptr;
   O_24 : New_Not_Null_Int_Ptr;

   -- Component_Definition tests:

   type Rec_1 is record
      C0 : not null Int_Ptr;                                       -- OK.
      C1 : not null Not_Ptr;                                       -- ERROR:
      C2 : not null P.Priv_Ptr;                                    -- ERROR:
      C3 : not null Not_Null_Int_Ptr;                              -- ERROR:
      C4 : not null New_Not_Null_Int_Ptr;                          -- ERROR:
      C5 : not null Not_Null_Int_Ptr_S1;                           -- ERROR:
      C6 : not null Not_Null_Int_Ptr_S2;                           -- ERROR:
      C7 : not null Not_Null_Int_Ptr_S3;                           -- ERROR:
   end record;

   type Arr_0 is array (1..4) of not null Int_Ptr;                 -- OK.
   type Arr_1 is array (1..4) of not null Not_Ptr;                 -- ERROR:
   type Arr_2 is array (1..4) of not null P.Priv_Ptr;              -- ERROR:
   type Arr_3 is array (1..4) of not null Not_Null_Int_Ptr;        -- ERROR:
   type Arr_4 is array (1..4) of not null New_Not_Null_Int_Ptr;    -- ERROR:
   type Arr_5 is array (1..4) of not null Not_Null_Int_Ptr_S1;     -- ERROR:
   type Arr_6 is array (1..4) of not null Not_Null_Int_Ptr_S2;     -- ERROR:
   type Arr_7 is array (1..4) of not null Not_Null_Int_Ptr_S3;     -- ERROR:

   protected Prot is
      procedure Check;
   private
      C0 : not null Int_Ptr;                                       -- OK.
      C1 : not null Not_Ptr;                                       -- ERROR:
      C2 : not null P.Priv_Ptr;                                    -- ERROR:
      C3 : not null Not_Null_Int_Ptr;                              -- ERROR:
      C4 : not null New_Not_Null_Int_Ptr;                          -- ERROR:
      C5 : not null Not_Null_Int_Ptr_S1;                           -- ERROR:
      C6 : not null Not_Null_Int_Ptr_S2;                           -- ERROR:
      C7 : not null Not_Null_Int_Ptr_S3;                           -- ERROR:
   end Prot;

   protected body Prot is
      procedure Check is
      begin
         null;
      end Check;
   end Prot;

   -- Subtype_Declaration tests:

   subtype S_10 is not null Int_Ptr;                               -- OK.
   subtype S_11 is not null Not_Ptr;                               -- ERROR:
   subtype S_12 is not null P.Priv_Ptr;                            -- ERROR:
   subtype S_13 is not null Not_Null_Int_Ptr;                      -- ERROR:
   subtype S_14 is not null New_Not_Null_Int_Ptr;                  -- ERROR:
   subtype S_15 is not null Not_Null_Int_Ptr_S1;                   -- ERROR:
   subtype S_16 is not null Not_Null_Int_Ptr_S2;                   -- ERROR:
   subtype S_17 is not null Not_Null_Int_Ptr_S3;                   -- ERROR:

   -- Derived_Type_Definition tests:

   type T_10 is new not null Int_Ptr;                              -- OK.
   type T_11 is new not null Not_Ptr;                              -- ERROR:
   type T_12 is new not null P.Priv_Ptr;                           -- ERROR:
   type T_13 is new not null Not_Null_Int_Ptr;                     -- ERROR:
   type T_14 is new not null New_Not_Null_Int_Ptr;                 -- ERROR:
   type T_15 is new not null Not_Null_Int_Ptr_S1;                  -- ERROR:
   type T_16 is new not null Not_Null_Int_Ptr_S2;                  -- ERROR:
   type T_17 is new not null Not_Null_Int_Ptr_S3;                  -- ERROR:

   -- Allocator tests:

   type Int_Ptr_Ptr is access Int_Ptr;
   type Not_Ptr_Ptr is access Not_Ptr;
   type Priv_Ptr_Ptr is access P.Priv_Ptr;
   type NN_Int_Ptr_Ptr is access Not_Null_Int_Ptr;

   -- The following is made illegal by AI05-0104-1, included in Ada 2012.
   IPP_0 :    Int_Ptr_Ptr := new not null Int_Ptr;                 -- ERROR:
   IPP_1 :    Not_Ptr_Ptr := new not null Not_Ptr;                 -- ERROR:
   IPP_2 :   Priv_Ptr_Ptr := new not null P.Priv_Ptr;              -- ERROR:
   IPP_3 : NN_Int_Ptr_Ptr := new not null Not_Null_Int_Ptr;        -- ERROR:
   IPP_4 :    Int_Ptr_Ptr := new not null Not_Null_Int_Ptr_S1;     -- ERROR:
   IPP_5 : NN_Int_Ptr_Ptr := new not null Not_Null_Int_Ptr_S2;     -- ERROR:

   -- Discriminant tests:

   type Rec_2 (D0 : not null Int_Ptr;                              -- OK.
               D1 : not null Not_Ptr;                              -- ERROR:
               D2 : not null P.Priv_Ptr;                           -- ERROR:
               D3 : not null Not_Null_Int_Ptr;                     -- ERROR:
               D4 : not null New_Not_Null_Int_Ptr;                 -- ERROR:
               D5 : not null Not_Null_Int_Ptr_S1;                  -- ERROR:
               D6 : not null Not_Null_Int_Ptr_S2;                  -- ERROR:
               D7 : not null Not_Null_Int_Ptr_S3)                  -- ERROR:
   is record
     null;
   end record;

   -- Parameter tests:

   procedure Proc_1
              (P0 : not null Int_Ptr;                              -- OK.
               P1 : not null Not_Ptr;                              -- ERROR:
               P2 : not null P.Priv_Ptr;                           -- ERROR:
               P3 : not null Not_Null_Int_Ptr;                     -- ERROR:
               P4 : not null New_Not_Null_Int_Ptr;                 -- ERROR:
               P5 : not null Not_Null_Int_Ptr_S1;                  -- ERROR:
               P6 : not null Not_Null_Int_Ptr_S2;                  -- ERROR:
               P7 : not null Not_Null_Int_Ptr_S3) is null;         -- ERROR:

   -- Function result subtype tests:

   function Func_0 return not null Int_Ptr is                      -- OK.
   begin
       return new Integer'(0);
   end Func_0;
   function Func_1 return not null Not_Ptr is                      -- ERROR:
   begin
       return Hot;
   end Func_1;
   function Func_2 return not null P.Priv_Ptr is                   -- ERROR:
   begin
       return P.A_Priv_Ptr;
   end Func_2;
   function Func_3 return not null Not_Null_Int_Ptr is             -- ERROR:
   begin
       return new Integer'(0);
   end Func_3;
   function Func_4 return not null New_Not_Null_Int_Ptr is         -- ERROR:
   begin
       return new Integer'(0);
   end Func_4;
   function Func_5 return not null Not_Null_Int_Ptr_S1 is          -- ERROR:
   begin
       return new Integer'(0);
   end Func_5;
   function Func_6 return not null Not_Null_Int_Ptr_S2 is          -- ERROR:
   begin
       return new Integer'(0);
   end Func_6;
   function Func_7 return not null Not_Null_Int_Ptr_S3 is          -- ERROR:
   begin
       return new Integer'(0);
   end Func_7;

   -- Object renaming tests:

   Ren_10 : not null Int_Ptr renames O_10;                         -- OK.
   Ren_11 : not null Not_Ptr renames O_21;                         -- ERROR:
   Ren_12 : not null P.Priv_Ptr renames O_22;                      -- ERROR:
   Ren_13 : not null Not_Null_Int_Ptr renames O_23;                -- ERROR:
   Ren_14 : not null New_Not_Null_Int_Ptr renames O_24;            -- ERROR:
   Ren_15 : not null Not_Null_Int_Ptr_S1 renames O_10;             -- ERROR:
   Ren_16 : not null Not_Null_Int_Ptr_S2 renames O_23;             -- ERROR:
   Ren_17 : not null Not_Null_Int_Ptr_S3 renames O_24;             -- ERROR:

   -- Formal object tests:

   generic
       O0 : not null Int_Ptr;                                      -- OK.
       O1 : not null Not_Ptr;                                      -- ERROR:
       O2 : not null P.Priv_Ptr;                                   -- ERROR:
       O3 : not null Not_Null_Int_Ptr;                             -- ERROR:
       O4 : not null New_Not_Null_Int_Ptr;                         -- ERROR:
       O5 : not null Not_Null_Int_Ptr_S1;                          -- ERROR:
       O6 : not null Not_Null_Int_Ptr_S2;                          -- ERROR:
       O7 : not null Not_Null_Int_Ptr_S3;                          -- ERROR:
   package Gen is
   end Gen;

   -- Access-to-Subprogram tests:

   type T_31 is access procedure;
   type T_32 is new not null T_31;
   subtype T_33 is T_32;
   type T_34 is new not null T_33;                                 -- ERROR:
   subtype T_35 is not null T_32;                                  -- ERROR:
   Obj_36 : not null T_32;                                         -- ERROR:
   procedure Proc_37 (P : in not null T_33) is null;               -- ERROR:

   type T_41 is access function return Integer;
   type T_42 is new not null T_41;
   subtype T_43 is T_42;
   type T_44 is new not null T_43;                                 -- ERROR:
   subtype T_45 is not null T_42;                                  -- ERROR:
   Obj_46 : not null T_42;                                         -- ERROR:
   procedure Proc_47 (P : in not null T_43) is null;               -- ERROR:

begin
   null;
end B3A0007;
