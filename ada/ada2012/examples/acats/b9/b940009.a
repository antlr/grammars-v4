-- B940009.A
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
--    Check that a protected procedure or entry is illegal if it implements
--    an inherited procedure with a first parameter of mode in other than
--    an access-to-variable parameter.
--
--    Check that a protected function is illegal if it implements
--    an inherited function with a first parameter that is not mode in or
--    is an access-to-variable parameter.
--
-- TEST DESCRIPTION:
--    We create a variety of interfaces along with a variety of protected
--    types and single objects to check that the rules are enforced
--    correctly. Note that inherited subprograms such as these still are
--    considered to be "implemented" as that only requires type conformance
--    (modes are not considered for "implemented"). Here we are checking
--    the Legality Rule 9.4(11.9/2); thus we expect the error to be reported
--    on the offending protected entity.
--
-- CHANGE HISTORY:
--    27 June 2019 - RLB - Initial test.
--!

package B940009 is

   --------------------
   --  Parent Types  --
   --------------------

   type Prot1_Int is protected interface;
   function  A (PI : in     Prot1_Int) return Integer   is abstract;
   function  B (PI : in out Prot1_Int) return Integer   is abstract;
   function  C (PI :    out Prot1_Int) return Integer   is abstract;
   function  D (PI : access Prot1_Int) return Integer   is abstract;
   function  E (PI : access constant Prot1_Int) return Integer   is abstract;

   type Prot2_Int is protected interface;
   procedure  J (PI : in     Prot2_Int; N : in Integer)  is null;
   procedure  K (PI : in out Prot2_Int; N : in Integer)  is null;
   procedure  L (PI :    out Prot2_Int; N : in Integer)  is null;
   procedure  M (PI : access Prot2_Int; N : in Integer)  is null;
   procedure  N (PI : access constant Prot2_Int; N : in Integer)  is null;

   type Prot3_Int is protected interface;
   procedure  T (PI : in     Prot3_Int; N : in Integer)  is abstract;

   -------------
   --  Types  --
   -------------

   protected type Protected_1 is new Prot1_Int with
      function A return Integer;                      -- OK. {7;1}
      function B return Integer;                      -- ERROR: {7;1}
      function C return Integer;                      -- ERROR: {7;1}
      function D return Integer;                      -- ERROR: {7;1}
      function E return Integer;                      -- OK. {7;1}
   end Protected_1;

   protected type Protected_2 is new Prot2_Int with
      procedure J (N : in Integer);                   -- ERROR: {7;1}
      procedure K (N : in Integer);                   -- OK. {7;1}
      procedure L (N : in Integer);                   -- OK. {7;1}
      procedure M (N : in Integer);                   -- OK. {7;1}
      procedure N (N : in Integer);                   -- ERROR: {7;1}
   end Protected_2;

   protected type Protected_3 is new Prot2_Int with
      entry J (N : in Integer);                       -- ERROR: {7;1}
      entry K (N : in Integer);                       -- OK. {7;1}
      entry L (N : in Integer);                       -- OK. {7;1}
      entry M (N : in Integer);                       -- OK. {7;1}
      entry N (N : in Integer);                       -- ERROR: {7;1}
   end Protected_3;

   protected type Protected_4 is new Prot3_Int with
      procedure T (N : in Integer);                   -- ERROR: {7;1}
   end Protected_4;
   procedure  T (PI : in Protected_4; N : in Integer);-- OPTIONAL ERROR: {4;1}
      -- In this case, protected procedure T still "implements" the inherited
      -- routine even though it is (later) illegal. The primitive routine
      -- doesn't change that, and it can be considered illegal itself for
      -- conflicting with the protected proecedure T.

   protected type Protected_5 is new Prot3_Int with
      procedure S (N : in Integer);                   -- OK. {7;1}
   end Protected_5;
   procedure  T (PI : in Protected_5; N : in Integer);-- OK. {4;1}

   ----------------------
   --  Single Objects  --
   ----------------------

   protected Protected_11 is new Prot1_Int with
      function A return Integer;                      -- OK. {7;1}
      function B return Integer;                      -- ERROR: {7;1}
      function C return Integer;                      -- ERROR: {7;1}
      function D return Integer;                      -- ERROR: {7;1}
      function E return Integer;                      -- OK. {7;1}
   end Protected_11;

   protected Protected_12 is new Prot2_Int with
      procedure J (N : in Integer);                   -- ERROR: {7;1}
      procedure K (N : in Integer);                   -- OK. {7;1}
      procedure L (N : in Integer);                   -- OK. {7;1}
      procedure M (N : in Integer);                   -- OK. {7;1}
      procedure N (N : in Integer);                   -- ERROR: {7;1}
   end Protected_12;

   protected Protected_13 is new Prot2_Int with
      entry J (N : in Integer);                       -- ERROR: {7;1}
      entry K (N : in Integer);                       -- OK. {7;1}
      entry L (N : in Integer);                       -- OK. {7;1}
      entry M (N : in Integer);                       -- OK. {7;1}
      entry N (N : in Integer);                       -- ERROR: {7;1}
   end Protected_13;


end B940009;
