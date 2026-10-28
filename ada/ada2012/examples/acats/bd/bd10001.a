-- BD10001.A
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
-- OBJECTIVES:
--    Check that only one of a representation item, operational item, or
--    aspect specification can be specified for a single aspect of a
--    single entity.
--
-- TEST DESCRIPTION:
--    This test checks 13.1(9.2/4), as revised by AI12-0116-1 and included
--    in the Corrigendum for Ada 2012.
--
--    This is not an exhaustive test. We try a handful of different aspects 
--    that were available before Ada 2012 (and thus have items).
--
--    Note: Pragma Inline is a program unit pragma, which is not one of
--    the kinds of items mentioned in 13.1(9.2/4). AARM 13.1(9.c.2/4) makes
--    it clear that obsolescent pragmas are intended to be included. The
--    wording wasn't complicated solely to support obsolescent features.
--
--    Pragma Priority has to be tested separately as that is an Annex D
--    feature.

-- PASS/FAIL CRITERIA:
--    The test contains several lines marked POSSIBLE ERROR: [Setnn].
--    For each value of nn, the implementation must detect one or more of
--    these possible errors. For instance, an error must be detected on
--    at least one of the lines labeled POSSIBLE ERROR: [Set01] for an
--    implementation to pass.
--
-- CHANGE HISTORY:
--     14 May 20   RLB     Created test.
--
--!
with Ada.Streams;
procedure BD10001 is

   type Basic_Int is range -99 .. 99;
   Specified_Size : constant := Basic_Int'Size;

   package Nest is

      type Check_Int is range -99 .. 99
         with Size => Specified_Size;        -- ANX-C RQMT. {1:7;1}

      -- Subtype-specific representation aspect:
               
      type Tst_Int is range -99 .. 99
         with Size => Specified_Size;        -- POSSIBLE ERROR: [Set01] {1:7;1}
         
      for Tst_Int'Size use Specified_Size;   -- POSSIBLE ERROR: [Set01] {7;1}


      -- Type-specific representation aspect (attribute definition clause):
      
      type Check_Arr is array (1..10) of Check_Int
         with Component_Size => Specified_Size; -- ANX-C RQMT. {1:7;1}
         
      type Tst_Arr is array (1..10) of Check_Int
         with Component_Size => 
                             Specified_Size; -- POSSIBLE ERROR: [Set02] {2:7;1}
     
      for Tst_Arr'Component_Size use 
                             Specified_Size; -- POSSIBLE ERROR: [Set02] {1:7;1}

      -- Type-specific representation aspect (pragma):
      type Check_Rec is record
          A, B : Check_Int;
      end record with Pack;                  -- ANX-C RQMT. {2:7;1}

      type Tst_Rec is record
          A, B : Check_Int;
      end record with Pack;                  -- POSSIBLE ERROR: [Set03] {2:7;1}
      
      pragma Pack (Tst_Rec);                 -- POSSIBLE ERROR: [Set03] {7;1}

      -- Operational aspect:

      type This_Rec is record
          A, B : Check_Int;
      end record with Write => Rec_Write;    -- POSSIBLE ERROR: [Set04] {7;1}

      procedure Rec_Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                           Item   : in This_Rec);
                           
      for This_Rec'Write use Rec_Write;      -- POSSIBLE ERROR: [Set04] {7;1}

      -- Subprogram representation aspect:
      
      procedure Do_It with Inline;           -- POSSIBLE ERROR: [Set05] {7;1}

      pragma Inline (Do_It);                 -- POSSIBLE ERROR: [Set05] {7;1}
      
   end Nest;
   
   package body Nest is
      procedure Rec_Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                           Item   : in This_Rec) is
      begin
         null; -- Dummy body.
      end Rec_Write;
      
      procedure Do_It is
      begin
         null; -- Dummy body.
      end Do_It;
   end Nest;
   
begin
   null;
end BD10001;
