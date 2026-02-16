-- BDE0011.A
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
--    The ACAA has created and maintains the Ada Conformity Assessment Test
--    Suite for the purpose of conformity assessments conducted in accordance
--    with the International Standard ISO/IEC 18009 - Ada: Conformity
--    assessment of a language processor. This test suite should not be used
--    to make claims of conformance unless used in accordance with
--    ISO/IEC 18009 and any applicable ACAA procedures.
--
--*
--
-- OBJECTIVE:
--    Check that entities in an aspect specification cause freezing (but
--    at the freezing point of the entity associated with the aspect
--    specification).
--
--    Check that appearance of an entity in the expression of an
--    expression function does not cause freezing, but a call of the function,
--    use of Access on the function, or use of the function as an actual
--    parameter to a instantiation causes freezing of the entity.
--
-- TEST DESCRIPTION:
--    For aspect specifications, we're testing 13.14(7.2/3).
--
--    For the expression of an expression function, we're testing
--    the last sentence of 13.14(10.1/3) [calls], 13.14(10.3/3) [Access],
--    and the last sentence of 13.14(10.2/3) [instantiations].
--
-- PASS/FAIL CRITERIA:
--    The test contains several lines marked POSSIBLE ERROR: [Setnn].
--    For each value of n, the implementation must detect one or more of
--    these possible errors. For instance, an error must be detected on
--    at least one of the lines labeled POSSIBLE ERROR: [Set01] for an
--    implementation to pass.
--
--    Note: The test passes if one or more of the aspect specifications
--    is not supported, as an error indication will be at the location of
--    one of the possible errors. Thus this test is applicable to
--    all implementations.
--
-- CHANGE HISTORY:
--    14 Nov 2013   GRB   Initial version.
--    27 Nov 2013   RLB   Added headers.
--    18 Apr 2014   RLB   Added test cases, improved objectives.
--    21 Apr 2014   RLB   Added an additional two test cases by suggestion.
--    23 Apr 2014   RLB   Change new case as legality is ambiguous.
--
--!
package BDE0011 is

   -- Aspect_specifications:

   X : constant Integer;                      -- POSSIBLE ERROR: [Set01]

   type A is new Natural with Size => X;      -- POSSIBLE ERROR: [Set01]

   type B is new Integer with Size => A'Size; -- POSSIBLE ERROR: [Set01]

   type C is new Integer;                     -- POSSIBLE ERROR: [Set02]

   type D is new Integer with Size => C'Size; -- POSSIBLE ERROR: [Set02]

   P : D;                    -- Freezes C, D  -- POSSIBLE ERROR: [Set02]

   for C'Size use Integer'Size;               -- POSSIBLE ERROR: [Set02]


   -- Expressions of an expression function - A call freezes:

   Y : constant Integer;                      -- POSSIBLE ERROR: [Set03]

   function E return Integer is (Y);          -- POSSIBLE ERROR: [Set03]

   -- The following call of E freezes Y:
   Q : constant Integer := E;                 -- POSSIBLE ERROR: [Set03]


   -- Expressions of an expression function - 'Access freezes:

   type Flub is range 0 .. 100;               -- POSSIBLE ERROR: [Set04]

   function Foo (A : in Natural) return Natural is
       (A + Flub'Size);                       -- POSSIBLE ERROR: [Set04]

   type Bar is access function (A : in Natural) return Natural;

   -- The following freezes Flub:
   R : Bar := Foo'Access;                     -- POSSIBLE ERROR: [Set04]

   for Flub'Size use Natural'Size;            -- POSSIBLE ERROR: [Set04]

   Val : Natural := R.all(5); -- If Foo'Access didn't freeze Flub, this
                              -- call would use an aspect of an unfrozen type.


   -- Expressions of an expression function - use in an instantiation freezes:

   generic
      with function GG return Integer;
   procedure Gen;

   Z : constant Integer;                      -- POSSIBLE ERROR: [Set05]

   function G return Integer is (Z);          -- POSSIBLE ERROR: [Set05]

   -- The following freezes Z:
   procedure Inst is new Gen (G);             -- POSSIBLE ERROR: [Set05]

   -- A typical use of an expression function as a completion still needs
   -- to be checked; first we declare the private type and it's primitive
   -- operations:

   type Priv is private;

   Null_Priv : constant Priv;

   function Length (A : Priv) return Natural;

private
   S : A;                 -- Freezes X, A, B  -- POSSIBLE ERROR: [Set01]
   -- These full definitions are too late, the object has already
   -- been frozen:
   X : constant Integer := Integer'Size;      -- POSSIBLE ERROR: [Set01]
   Y : constant Integer := 10;                -- POSSIBLE ERROR: [Set03]
   Z : constant Integer := 20;                -- POSSIBLE ERROR: [Set05]


   type Priv is record
      Len : Natural;
      Data: Natural;
   end record;

   -- Completions still need checking (call case):
   function Length (A : Priv) return Natural is
      (if A = Null_Priv then 0 else A.Len);   -- POSSIBLE ERROR: [Set06]
      -- This not frozen by the exact wording of 13.14(3/3), but it
      -- might have been intended to be frozen. Question now sent to ARG.
      -- Resolution of the question will not require a test change,
      -- through, as we allow an error above anyway.

   Empty_Priv : constant Priv := (0, 0); -- OK, freezes Priv.

   -- The following call of Length freezes Null_Priv -- before it is completed:
   Null_Priv : constant Priv :=
      (Len => Length(Empty_Priv), Data => 0); -- POSSIBLE ERROR: [Set06]

end BDE0011;

