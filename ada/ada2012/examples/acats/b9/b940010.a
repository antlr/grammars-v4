-- B940010.A
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
--     DISCLOSED ARE AS IS. ADACORE MAKES NO EXPRESS OR IMPLIED WARRANTY AS
--     TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE SOFTWARE,
--     DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE OR
--     DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
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
--
-- OBJECTIVE:
--    Check that a protected operation is illegal if it implements an inherited
--    subprogram and the subprogram is also overridden by a primitive
--    subprogram.
--
--    Check that a primitive subprogram of a tagged protected type is illegal
--    if it is type conformant with the prefixed view profile of a protected
--    operation of the type.
--
-- TEST DESCRIPTION:
--    We create a variety of interfaces along with a variety of protected
--    types and primitive subprograms to check that the rules are enforced
--    correctly. For the first case, we are checking the Legality Rule
--    9.4(11.5/2) ["at most one of the following shall apply"]; thus
--    we expect the error to be reported on either the protected operation
--    or the primitive operation. For the second case, we are checking
--    9.4(11.4/3); this rule clearly applies to the primitive operation
--    so we only expect the error to be reported there.
--
-- PASS/FAIL CRITERIA:
--    The test contains several lines marked POSSIBLE ERROR: [Setnn].
--    For each value of nn, the implementation must detect one or more of
--    these possible errors. For instance, an error must be detected on
--    at least one of the lines labeled POSSIBLE ERROR: [Set01] for an
--    implementation to pass.
--
-- CHANGE HISTORY:
--    27 June 2019 - RLB - Initial test.
--!

package B940010 is

   --------------------
   --  Parent Types  --
   --------------------

   type Prot1_Int is protected interface;
   function  A (PI : in Prot1_Int) return Integer   is abstract;
   function  B (PI : in Prot1_Int) return Integer   is abstract;

   type Prot2_Int is protected interface;
   procedure  J (PI : in out Prot2_Int; N : in Integer)  is null;
   procedure  K (PI : access Prot2_Int; N : in Integer)  is null;
   procedure  M (PI : in out Prot2_Int; N : in out Integer)  is null;

   -------------
   --  Types  --
   -------------

   protected type Protected_1 is new Prot1_Int with
      function A return Integer;              -- OK. {7;1}
      function B return Integer;              -- POSSIBLE ERROR: [Set1] {7;1}
      function C return Integer;              -- OK. {7;1}
   end Protected_1;
   function B (PI : in Protected_1)
                              return Integer; -- POSSIBLE ERROR: [Set1] {1:4;1}
   function C (PI : in Protected_1)
                              return Integer; -- ERROR: {1:4;1}

   protected type Protected_2 is -- Not tagged, rules do not apply.
      function A return Integer;              -- OK. {7;1}
      function C return Integer;              -- OK. {7;1}
   end Protected_2;
   function C (PI : in Protected_2) return Integer; -- OK. {4;1}


   protected type Protected_3 is new Prot2_Int with
      procedure J (N : in Integer);           -- OK. {7;1}
      procedure K (N : in Integer);           -- POSSIBLE ERROR: [Set2] {7;1}
      procedure L (N : in Integer);           -- OK. {7;1}
      procedure M (N : in Integer);           -- POSSIBLE ERROR: [Set3] {7;1}
      procedure N (N : in Integer);           -- OK. {7;1}
   end Protected_3;
   procedure  K (PI : access Protected_3;
                 N : in Integer);             -- POSSIBLE ERROR: [Set2] {1:4;1}
   procedure  L (PI : access Protected_3;
                 N : in Integer);             -- ERROR: {1:4;1}
   procedure  M (PI : in out Protected_3;
                 N : in out Integer);         -- POSSIBLE ERROR: [Set3] {1:4;1}
   procedure  N (PI : in out Protected_3;
                 N :    out Integer);         -- ERROR: {1:4;1}

   protected type Protected_4 is -- Not tagged, rules do not apply.
      procedure J (N : in Integer);           -- OK. {7;1}
      procedure K (N : in Integer);           -- OK. {7;1}
      procedure M (N : in Integer);           -- OK. {7;1}
   end Protected_4;
   procedure  K (PI : access Protected_4;
                 N : in Integer);             -- OK. {1:4;1}
   procedure  M (PI : in out Protected_4;
                 N : in out Integer);         -- OK. {1:4;1}

   protected type Protected_5 is new Prot2_Int with
      entry J (N : in Integer);               -- OK. {7;1}
      entry K (N : in Integer);               -- POSSIBLE ERROR: [Set4] {7;1}
      entry L (N : in Integer);               -- OK. {7;1}
      entry M (N : in Integer);               -- POSSIBLE ERROR: [Set5] {7;1}
      entry N (N : in Integer);               -- OK. {7;1}
   end Protected_5;
   procedure  K (PI : access Protected_5;
                 N : in Integer);             -- POSSIBLE ERROR: [Set4] {1:4;1}
   procedure  L (PI : access Protected_5;
                 N : in Integer);             -- ERROR: {1:4;1}
   procedure  M (PI : in out Protected_5;
                 N : in out Integer);         -- POSSIBLE ERROR: [Set5] {1:4;1}
   procedure  N (PI : in out Protected_5;
                 N :    out Integer);         -- ERROR: {1:4;1}

   protected type Protected_6 is -- Not tagged, rules do not apply.
      entry J (N : in Integer);               -- OK. {7;1}
      entry K (N : in Integer);               -- OK. {7;1}
      entry M (N : in Integer);               -- OK. {7;1}
   end Protected_6;
   procedure  K (PI : access Protected_6;
                 N : in Integer);             -- OK. {1:4;1}
   procedure  M (PI : in out Protected_6;
                 N : in out Integer);         -- OK. {1:4;1}

end B940010;
