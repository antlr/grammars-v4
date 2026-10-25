-- BC54004.A
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
--     Check that for a generic formal access type, either both or neither of
--     the formal and actual subtypes exclude null.
--
-- TEST DESCRIPTION:
--     This test checks the third sentence of 12.5.4(4/2). Both the formal
--     and actual types are named access types, so that reduces the
--     possibilities substantially. We do check both access-to-object and
--     access-to-subprogram types. We also check the various kinds of
--     general access types (but only try legal matching cases, so as to
--     avoid mixing other kinds of errors).
--
-- CHANGE HISTORY:
--     09 Jun 2018  RLB  Created test.

procedure BC54004 is

   -- Types to test with:

   type Rec is record
      I : Integer;
      C : Character;
   end record;

   type ARec is access Rec;

   subtype SARec is ARec;

   subtype SNARec is not null ARec;

   type ANRec is not null access Rec;

   subtype SANRec is ANRec;

   type AARec is access all Rec;

   subtype SAARec is AARec;

   subtype SNAARec is not null AARec;

   type AANRec is not null access all Rec;

   subtype SAANRec is AANRec;

   type CARec is access constant Rec;

   subtype SCARec is CARec;

   subtype SNCARec is not null CARec;

   type CANRec is not null access constant Rec;

   subtype SCANRec is CANRec;

   type AProc is access procedure;

   subtype SAProc is AProc;

   subtype SNAProc is not null AProc;

   type ANProc is not null access procedure;

   subtype SANProc is ANProc;

   -- Generics to try:

   generic
      type GARec is access Rec;
   package Gen1 is
      V : GARec;
   end Gen1;

   generic
      type GNARec is not null access Rec;
   package Gen2 is
      V : GNARec;
   end Gen2;

   generic
      type GAARec is access all Rec;
   package Gen3 is
      V : GAARec;
   end Gen3;

   generic
      type GNAARec is not null access all Rec;
   package Gen4 is
      V : GNAARec;
   end Gen4;

   generic
      type GACRec is access constant Rec;
   package Gen5 is
      V : GACRec;
   end Gen5;

   generic
      type GNACRec is not null access constant Rec;
   package Gen6 is
      V : GNACRec;
   end Gen6;

   generic
      type GAProc is access procedure;
   package Gen7 is
      V : GAProc;
   end Gen7;

   generic
      type GNAProc is not null access procedure;
   package Gen8 is
      V : GNAProc;
   end Gen8;

   -- The actual instances:

   package Inst01 is new Gen1 (ARec);                      -- OK. {4;1}

   package Inst02 is new Gen1 (SARec);                     -- OK. {4;1}

   package Inst03 is new Gen1 (SNARec);                    -- ERROR: {4;1}

   package Inst04 is new Gen1 (ANRec);                     -- ERROR: {4;1}

   package Inst05 is new Gen1 (SANRec);                    -- ERROR: {4;1}

   package Inst06 is new Gen1 (AARec);                     -- OK. {4;1}

   package Inst07 is new Gen1 (SAARec);                    -- OK. {4;1}

   package Inst08 is new Gen1 (SNAARec);                   -- ERROR: {4;1}

   package Inst09 is new Gen1 (AANRec);                    -- ERROR: {4;1}

   package Inst10 is new Gen1 (SAANRec);                   -- ERROR: {4;1}


   package Inst11 is new Gen2 (ARec);                      -- ERROR: {4;1}

   package Inst12 is new Gen2 (SARec);                     -- ERROR: {4;1}

   package Inst13 is new Gen2 (SNARec);                    -- OK. {4;1}

   package Inst14 is new Gen2 (ANRec);                     -- OK. {4;1}

   package Inst15 is new Gen2 (SANRec);                    -- OK. {4;1}

   package Inst16 is new Gen2 (AARec);                     -- ERROR: {4;1}

   package Inst17 is new Gen2 (SAARec);                    -- ERROR: {4;1}

   package Inst18 is new Gen2 (SNAARec);                   -- OK. {4;1}

   package Inst19 is new Gen2 (AANRec);                    -- OK. {4;1}

   package Inst20 is new Gen2 (SAANRec);                   -- OK. {4;1}


   package Inst21 is new Gen3 (AARec);                     -- OK. {4;1}

   package Inst22 is new Gen3 (SAARec);                    -- OK. {4;1}

   package Inst23 is new Gen3 (SNAARec);                   -- ERROR: {4;1}

   package Inst24 is new Gen3 (AANRec);                    -- ERROR: {4;1}

   package Inst25 is new Gen3 (SAANRec);                   -- ERROR: {4;1}


   package Inst26 is new Gen4 (AARec);                     -- ERROR: {4;1}

   package Inst27 is new Gen4 (SAARec);                    -- ERROR: {4;1}

   package Inst28 is new Gen4 (SNAARec);                   -- OK. {4;1}

   package Inst29 is new Gen4 (AANRec);                    -- OK. {4;1}

   package Inst30 is new Gen4 (SAANRec);                   -- OK. {4;1}


   package Inst31 is new Gen5 (CARec);                     -- OK. {4;1}

   package Inst32 is new Gen5 (SCARec);                    -- OK. {4;1}

   package Inst33 is new Gen5 (SNCARec);                   -- ERROR: {4;1}

   package Inst34 is new Gen5 (CANRec);                    -- ERROR: {4;1}

   package Inst35 is new Gen5 (SCANRec);                   -- ERROR: {4;1}


   package Inst36 is new Gen6 (CARec);                     -- ERROR: {4;1}

   package Inst37 is new Gen6 (SCARec);                    -- ERROR: {4;1}

   package Inst38 is new Gen6 (SNCARec);                   -- OK. {4;1}

   package Inst39 is new Gen6 (CANRec);                    -- OK. {4;1}

   package Inst40 is new Gen6 (SCANRec);                   -- OK. {4;1}


   package Inst41 is new Gen7 (AProc);                     -- OK. {4;1}

   package Inst42 is new Gen7 (SAProc);                    -- OK. {4;1}

   package Inst43 is new Gen7 (SNAProc);                   -- ERROR: {4;1}

   package Inst44 is new Gen7 (ANProc);                    -- ERROR: {4;1}

   package Inst45 is new Gen7 (SANProc);                   -- ERROR: {4;1}


   package Inst46 is new Gen8 (AProc);                     -- ERROR: {4;1}

   package Inst47 is new Gen8 (SAProc);                    -- ERROR: {4;1}

   package Inst48 is new Gen8 (SNAProc);                   -- OK. {4;1}

   package Inst49 is new Gen8 (ANProc);                    -- OK. {4;1}

   package Inst50 is new Gen8 (SANProc);                   -- OK. {4;1}


begin
   null;
end BC54004;
