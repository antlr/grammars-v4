-- B680001
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
--
-- OBJECTIVE:
--     Check legality of an expression function:
--       (A) An expression function cannot complete a procedure declaration,
--           package declaration, or any kind of body;
--       (B) Check that an expression function that completes a declaration
--           must fully conform to that declaration;
--       (C) Check that a completion is not allowed for an expression function;
--       (D) Check that the type of the expression of an expression function
--           must be the same as the result type of the function.
--
-- TEST DESCRIPTION:
--
--     We test a few examples of each kind of error.
--
--     For (A), overloading makes it impossible to test completing a procedure
--     body -- the expression function with have the wrong profile and thus
--     will not appear to be a completion. We still can test completing a
--     procedure declaration that has no other completion - that will cause
--     some sort of error. We can also test completing a non-overloadable body
--     like a package body.
--
--     For (C), we'll just check completing with a function body, since that
--     is the most likely error.
--
--     Since (B) and (D) are specific examples of more general kinds of
--     checking, exhaustive tests are not necessary here -- the purpose of
--     this test is to ensure that the checks are made at all.
--
-- PASS/FAIL CRITERIA:
--     The test contains several lines marked POSSIBLE ERROR: [Setnn].
--     For each value of n, the implementation must detect one or more of
--     these possible errors. For instance, an error must be detected on
--     at least one of the lines labeled POSSIBLE ERROR: [Set01] for an
--     implementation to pass.
--
-- CHANGE HISTORY:
--      20 Mar 2014   RLB   Created test.
--      13 Dec 2017   RLB   Corrected use of nonexistent parameter; added
--                          location indicators.
--
--!
procedure B680001 is

   procedure F1 (A : Integer);                 -- POSSIBLE ERROR: [Set01] A {4}

   function F1 (A : Integer) return Boolean is -- POSSIBLE ERROR: [Set01] A {4}
       (A > 10);

   package P2 is

      function F2 (A : Integer) return Boolean is
          (A > 30);

      package F4 is
         function FX (A : Integer) return Boolean is
             (A < 0);
      end F4;

      package F5 is
         function FY (A : Integer) return Boolean is
             (A < 0);
      end F5;

   end P2;

   package body P2 is

      function F2 (A : Integer) return Boolean is  -- ERROR: C {7}
      begin
         return (A > 30);
      end F2;

      function F3 (A : Integer)
         return Boolean is                   -- POSSIBLE ERROR: [Set03] A {1:7}
      begin
         return (A >= 12);
      end F3;

      function F3 (A : Integer)
         return Boolean is                   -- POSSIBLE ERROR: [Set03] A {1:7}
          (A >= 12);

      function F4 (A : Integer) return Boolean is  -- ERROR: A {7}
          (A < 0);

      package body F5 is
         function Fugh (A : Integer) return Boolean is
             (A in 1 .. 99);
      end F5;

      function F5 (A : Integer) return Boolean is  -- ERROR: A {7}
          (A in 1 .. 99);

   end P2;

   package P3 is

      function F06 (A : Integer) return Boolean;

      function F07 (A : Integer) return Boolean;

      function F08 (A : Integer) return Boolean;

      function F09 (A : Integer) return Integer;

      function F10 (A : Integer) return Integer;

   private

      function F06 (B : Integer) return Boolean is -- ERROR: B {7}
         (B > 75);

      function F07 (A : Natural) return Boolean is -- ERROR: B {7}
         (A > 35);

      function F08 (A : aliased Integer) return Boolean is -- ERROR: B {7}
         (A > 60);

      function F09 (A : Integer) return Integer is
         (F08 (A));                                -- ERROR: D {10;1}

      type Short is range 0 .. 99;
      Var : Short;

      function F10 (A : Integer) return Integer is
         (if A > 0 then Short else 0);             -- ERROR: D {10;1}

   end P3;

begin
   null;
end B680001;

