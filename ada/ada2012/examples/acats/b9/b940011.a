-- B940011.A
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
--    Check that if an inherited subprogram is implemented by a protected
--    operation, the prefixed view profile of the inherited subprogram must be
--    subtype conformant with the protected operation.
--
--    Check that if a primitive subprogram overrides an inherited subprogrm
--    of a protected opration, the primitive and inherited subprogram must be
--    subtype conformant.
--
--    Check that if an inherited subprogram for a protected type is neither
--    inherited nor overridden, it must be a null subprogram.
--
-- TEST DESCRIPTION:
--    We create a variety of interfaces along with a variety of protected
--    types and primitive subprograms to check that the rules are enforced
--    correctly.
--
-- CHANGE HISTORY:
--    27 June 2019 - RLB - Initial test.
--!

package B940011 is

   --------------------
   --  Parent Types  --
   --------------------

   type Prot1_Int is protected interface;
   function  A (PI : in Prot1_Int; N : in Integer) return Integer is abstract;
   function  B (PI : in Prot1_Int; N : in Integer) return Integer is abstract;
   function  C (PI : in Prot1_Int; N : in Integer) return Integer is abstract;
   function  D (PI : in Prot1_Int; N : in Integer) return Integer is abstract;

   type Prot2_Int is protected interface;
   procedure  J (PI : in out Prot2_Int; N : in Integer)  is null;
   procedure  K (PI : access Prot2_Int; N : in Integer)  is abstract;
   procedure  L (PI : in out Prot2_Int; N : in Integer)  is abstract;
   procedure  M (PI : in out Prot2_Int; N : in Integer)  is null;

   -------------
   --  Types  --
   -------------

   protected type Protected_1 is new Prot1_Int with
      function A (N : in Integer) return Integer;           -- OK. {7;1}
      function B (N : in out Integer) return Integer;       -- ERROR: {7;1}
      function C (N :    out Integer) return Integer;       -- ERROR: {7;1}
      function D (N : in Natural) return Integer;           -- ERROR: {7;1}
      function E (N : in out Integer) return Integer;       -- OK. {7;1}
   end Protected_1;

   protected type Protected_2 is new Prot1_Int with
      function A (N : in Integer) return Integer;           -- OK. {7;1}
   end Protected_2;
   function B (PI : in Protected_2; N : in out Integer)
                              return Integer;               -- ERROR: {1:4;1}
   function C (PI : in Protected_2; N :    out Integer)
                              return Integer;               -- ERROR: {1:4;1}
   function D (PI : in Protected_2; N : in Natural)
                              return Integer;               -- ERROR: {1:4;1}
   function E (PI : in Protected_2; N : in out Integer)
                              return Integer;               -- OK. {1:4;1}

   protected Protected_3 is new Prot1_Int with
      function A (N : in Integer) return Integer;           -- OK. {7;1}
      function B (N : in out Integer) return Integer;       -- ERROR: {7;1}
      function C (N :    out Integer) return Integer;       -- ERROR: {7;1}
      function D (N : in Natural) return Integer;           -- ERROR: {7;1}
      function E (N : in out Integer) return Integer;       -- OK. {7;1}
   end Protected_3;

   protected type Protected_4 is new Prot2_Int with
      procedure J (N : in Integer);                         -- OK. {7;1}
      procedure K (N : in out Integer);                     -- ERROR: {7;1}
      procedure L (N :    out Integer);                     -- ERROR: {7;1}
      procedure M (N : in Natural);                         -- ERROR: {7;1}
      procedure N (N : in out Integer);                     -- OK. {7;1}
   end Protected_4;

   protected type Protected_5 is new Prot2_Int with
      procedure J (N : in Integer);                         -- OK. {7;1}
   end Protected_5;
   procedure  K (PI : access Protected_5;
                 N : in out Integer);                       -- ERROR: {1:4;1}
   procedure  L (PI : in out Protected_5;
                 N :    out Integer);                       -- ERROR: {1:4;1}
   procedure  M (PI : in out Protected_5;
                 N : in Natural);                           -- ERROR: {1:4;1}
   procedure  N (PI : in out Protected_5;
                 N :    out Integer);                       -- OK. {1:4;1}

   protected Protected_6 is new Prot2_Int with
      procedure J (N : in Integer);                         -- OK. {7;1}
      procedure K (N : in out Integer);                     -- ERROR: {7;1}
      procedure L (N :    out Integer);                     -- ERROR: {7;1}
      procedure M (N : in Natural);                         -- ERROR: {7;1}
      procedure N (N : in out Integer);                     -- OK. {7;1}
   end Protected_6;

   protected type Protected_7 is new Prot2_Int with
      entry J (N : in Integer);                             -- OK. {7;1}
      entry K (N : in out Integer);                         -- ERROR: {7;1}
      entry L (N :    out Integer);                         -- ERROR: {7;1}
      entry M (N : in Natural);                             -- ERROR: {7;1}
      entry N (N : in out Integer);                         -- OK. {7;1}
   end Protected_7;

   protected type Protected_8 is new Prot1_Int with
      function A (N : in Integer) return Integer;
      function C (N : in Integer) return Integer;
   end Protected_8; -- ERROR: B, D missing {3:4;1}

   protected type Protected_9 is new Prot1_Int with
      function A (N : in Integer) return Integer;
      function D (N : in Integer) return Integer;
   end Protected_9;
   function B (PI : in Protected_9; N : in Integer)         -- C missing.
                              return Integer;               -- ERROR: {5:4;1}

   protected type Protected_10 is new Prot2_Int with
      procedure K (N : in Integer);                         -- OK. {7;1}
      procedure L (N : in Integer);                         -- OK. {7;1}
   end Protected_10;                           -- J, M are null.

   protected type Protected_11 is new Prot2_Int with
      procedure J (N : in Integer);
      procedure L (N : in Integer);
   end Protected_11;                           -- ERROR: K missing {3:4;1}

   protected Protected_12 is new Prot2_Int with
      procedure K (N : in Integer);
      procedure M (N : in Integer);
   end Protected_12;                           -- ERROR: L missing {3:4;1}

   protected type Protected_13 is new Prot2_Int with
      procedure J (N : in Integer);
   end Protected_13;
   procedure  L (PI : in out Protected_13;
                 N  : in Integer);             -- ERROR: K missing {4:4;1}

   protected type Protected_14 is new Prot2_Int with
      procedure M (N : in Integer);
   end Protected_14;
   procedure  K (PI : access Protected_14;
                 N  : in Integer);
   procedure  L (PI : in out Protected_14;
                 N  : in Integer);             -- OK. J is null {6:4;1}

end B940011;
