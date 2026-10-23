-- B3A1005.A
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
--
--     Check that if a use of an incomplete type T is part of the declaration
--     of a primitive subprogram of T, and T is given in the private part of
--     package P, T must be completed in the same private part.
--
-- TEST DESCRIPTION:
--
--     Test both normal and tagged incomplete types. (We do not need to
--     test incomplete views, since those cannot be declared in the same
--     private part as primitive operations.) We try incomplete types
--     that are completed in the private part as well as those that are
--     deferred in order to check that the rule 3.10.1(9.3/2) is only
--     applied to incomplete types that are not completed. We also check
--     that uses of T'Class are allowed for tagged incomplete types
--     in operations that would have been primitive and thus illegal if
--     they had been for T.
--
-- CHANGE HISTORY:
--     01 Oct 2008  RLB  Created test based on part of test B3A1A01.
--     03 Mar 2014  RLB  Allowed cases newly allowed by Ada 2012,
--                       adjusted comments.


package B3A1005 is

private

   -- The follows are all allowed by 3.10.1(5-9/2);
   -- 3.10.1(9.3/2) does not apply:

   type Normal_Inc;
   type Normal_Tagged_Inc is tagged;

   procedure Proc11 (A : Normal_Inc);                   -- OK.

   procedure Proc12 (A : Normal_Tagged_Inc);            -- OK.

   procedure Proc13 (Grab : access Normal_Inc);         -- OK.

   procedure Proc14 (Ptr  : access Normal_Tagged_Inc);  -- OK.

   procedure Proc15 (Cnt : in Natural;
                     Obj : out Normal_Inc);             -- OK.

   procedure Proc16 (Why : in Boolean;
                     Obj : in out Normal_Tagged_Inc);   -- OK.

   procedure Proc17 (
      Obj : in out Normal_Tagged_Inc'Class);            -- OK.

   procedure Proc18 (
      Oper : access Normal_Tagged_Inc'Class);           -- OK.

   function Func11 return Normal_Inc;                   -- OK.

   function Func12 return Normal_Tagged_Inc;            -- OK.

   function Func13 (Shape : Natural)
                   return access Normal_Inc;            -- OK.

   function Func14 (Undo : Boolean)
                   return access Normal_Tagged_Inc;     -- OK.

   function Func15 (Count : Natural)
               return access Normal_Tagged_Inc'Class;   -- OK.


   -- The errors in the following are violations of 3.10.1(9.3/2) (our
   -- objective):

   type Deferred_Inc;
   type Deferred_Tagged_Inc is tagged;

   procedure Proc21 (A : Deferred_Inc);                 -- ERROR:

   procedure Proc22 (A : Deferred_Tagged_Inc);          -- ERROR:

   procedure Proc23 (Grab : access Deferred_Inc);       -- ERROR:

   procedure Proc24 (Ptr  : access Deferred_Tagged_Inc);-- ERROR:

   procedure Proc25 (Cnt : in Natural;
                     Obj : out Deferred_Inc);           -- ERROR:

   procedure Proc26 (Why : in Boolean;
                     Obj : in out Deferred_Tagged_Inc); -- ERROR:

   procedure Proc27 (
      Obj : in out Deferred_Tagged_Inc'Class);          -- OK.

   procedure Proc28 (
      Oper : access Deferred_Tagged_Inc'Class);         -- OK.

   function Func21 return Deferred_Inc;                 -- ERROR:

   function Func22 return Deferred_Tagged_Inc;          -- ERROR:

   function Func23 (Shape : Natural)
                   return access Deferred_Inc;          -- ERROR:

   function Func24 (Undo : Boolean)
                   return access Deferred_Tagged_Inc;   -- ERROR:

   function Func25 (Count : Natural)
               return access Deferred_Tagged_Inc'Class; -- OK.



   type Normal_Inc is (A, B, C);

   type Normal_Tagged_Inc is tagged record
       O : Normal_Inc;
   end record;

end B3A1005;
