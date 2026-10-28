--  B551002.A
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
--     Check that a Iterator_Element aspect cannot be specified on an
--     untagged type nor on a type that does not have one of the indexing
--     aspects.
--
--     Check that name of a Iterator_Element aspect cannot denote an entity
--     other than a subtype.
--
--  CHANGE HISTORY:
--     10 Feb 2015  BM   Initial Version.
--     19 Mar 2015  RLB  Split into several smaller tests with fewer
--                       objectives. Completed second objective.
--     26 Sep 2019  RLB  Removed dependence on Iterator_Interfaces as it is not
--                       needed for this aspect. Added error location
--                       indicators.
--!

procedure B551002 is

   Total : Integer := 0;

begin

   Objective_1 :
   --    Check that a Iterator_Element aspect cannot be specified on an
   --    untagged type nor on a type that does not have one of the indexing
   --    aspects.
   --
   declare
      package Pkg1 is
         type T1 is tagged null record with
            Constant_Indexing => Cnst_Index1,
            Iterator_Element  => Integer;                  -- OK. {2:10;1}

         function Cnst_Index1 (Container : T1; Ind : Natural) return Natural;

         function Iterate (Container : T1) return Integer;
         function Cnst_Index1 (Container : T1; Ind : Natural)
            return Natural is (Ind);

         function Iterate (Container : T1) return Integer is (42);

         -- Untagged type
         type T2 is null record with
            Constant_Indexing => Cnst_Index2,
            Iterator_Element  => Natural;                  -- ERROR: {2:10;1}
            -- Note: Constant_Indexing is also illegal here;
            -- either or both errors can be reported.

         function Cnst_Index2 (Container : T2; Ind : Natural) return Natural;

         function Iterate (Container : T2) return Integer;
         function Cnst_Index2 (Container : T2; Ind : Natural)
            return Natural is (Ind);

         function Iterate (Container : T2) return Integer is (42);

         --  Missing Indexing aspect
         type T3 is tagged null record with
            Iterator_Element  => Natural;                  -- ERROR: {1:10;1}

         -- Untagged type (full type is tagged, but that doesn't matter)
         type T4 is private with
            Constant_Indexing => Cnst_Index4,
            Iterator_Element  => Natural;                  -- ERROR: {2:10;1}
            -- Note: Constant_Indexing is also illegal here;
            -- either or both errors can be reported.

         function Cnst_Index4 (Container : T4; Ind : Natural) return Natural;

         function Iterate (Container : T4) return Integer;
         function Cnst_Index4 (Container : T4; Ind : Natural)
            return Natural is (Ind);

         function Iterate (Container : T4) return Integer is (42);

         --  Missing Indexing aspect
         type T5 is tagged private with
            Iterator_Element  => Natural;                  -- ERROR: {1:10;1}

         -- Untagged type without indexing aspect
         type T6 is private with
            Iterator_Element  => Natural;                  -- ERROR: {1:10;1}

      private
         type T4 is tagged null record;

         type T5 is tagged null record;

         type T6 is tagged null record;
      end Pkg1;

   begin -- Objective_1
      null;
   end Objective_1;


   Objective_2 :
   --    Check that name of a Iterator_Element aspect cannot denote an entity
   --    other than a subtype.
   declare
      package Pkg2 is
         type T1 is tagged null record with
            Constant_Indexing => Cnst_Index1,
            Iterator_Element  => Integer;                  -- OK. {2:10;1}

         function Cnst_Index1 (Container : T1; Ind : Natural)
            return Natural;

         function Iterate (Container : T1) return Integer;
         function Cnst_Index1 (Container : T1; Ind : Natural)
            return Natural is (Ind);

         function Iterate (Container : T1) return Integer is (42);

         Obj : Natural;

         -- Object instead of subtype.
         type T2 is tagged null record with
            Constant_Indexing => Cnst_Index2,
            Iterator_Element  => Obj;                      -- ERROR: {2:10;1}
            -- Note: Constant_Indexing is also illegal here.

         function Cnst_Index2 (Container : T2; Ind : Natural) return Natural;

         function Iterate (Container : T2) return Integer;
         function Cnst_Index2 (Container : T2; Ind : Natural)
            return Natural is (Ind);

         function Iterate (Container : T2) return Integer is (42);

         -- Subprogram instead of subtype
         type T3 is tagged private with
            Constant_Indexing => Cnst_Index3,
            Iterator_Element  => Iterate;                  -- ERROR: {2:10;1}

         function Cnst_Index3 (Container : T3; Ind : Natural) return Natural;

         function Iterate (Container : T3) return Integer;
         function Cnst_Index3 (Container : T3; Ind : Natural)
            return Natural is (Ind);

         function Iterate (Container : T3) return Integer is (42);

         -- Single protected object instead of subtype
         type T4 is tagged private with
            Constant_Indexing => Cnst_Index4,
            Iterator_Element  => Prot;                     -- ERROR: {2:10;1}

         function Cnst_Index4 (Container : T4; Ind : Natural) return Natural;

         function Iterate (Container : T4) return Integer;
         function Cnst_Index4 (Container : T4; Ind : Natural)
            return Natural is (Ind);

         function Iterate (Container : T4) return Integer is (42);

         protected Prot is -- Not "protected type"!
            function Get return Natural;
            procedure Put (Val : in Natural);
         private
            Comp : Natural;
         end Prot;

         -- Could try other kinds of entities (exception, package, single
         -- task) but these seem unlikely to occur and unlikely to be missed.
      private
         type T3 is tagged null record;

         type T4 is tagged null record;
      end Pkg2;

      package body Pkg2 is

         protected body Prot is
            function Get return Natural is
            begin
               return Comp;
            end Get;

            procedure Put (Val : in Natural) is
            begin
               Comp := Val;
            end Put;
         end Prot;

      end Pkg2;

   begin -- Objective_2
      null;
   end Objective_2;

end B551002;
