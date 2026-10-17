-- B611005.A
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
--
--*
--
-- OBJECTIVE:
--
--     Check that Pre or Post cannot be specified on an abstract subprogram.
--
--     Check that Pre or Post cannot be specified on a null procedure.
--
-- CHANGE HISTORY:
--     04 Feb 2016   RLB   Created test.
--     28 Mar 2016   RLB   Added error location codes.
--
--!
package B611005 is

   -- Untagged cases:
   type Blah is new Integer;

   function "+" (Left, Right : Blah) return Blah is (Left)
       with Pre => Right = 0;                                  -- OK. {13;1}

   function "-" (Left, Right : Blah) return Blah is (Left)
       with Post => Right = 0;                                 -- OK. {13;1}

   function "/" (Left, Right : Blah) return Blah is abstract
       with Pre => Right /= 0;                                 -- ERROR: {13;1}

   function "*" (Left, Right : Blah) return Blah is abstract
       with Post => B611005."*"'Result >= Left + Right;        -- ERROR: {13;1}

   procedure Update (Obj : in out Blah) is null
       with Pre => Obj >= 0;                                   -- ERROR: {13;1}

   procedure Bump (Obj : in out Blah) is null
       with Post => Obj >= 0;                                  -- ERROR: {13;1}

   -- Tagged cases:
   type Root is abstract tagged null record;

   function Is_OK (Obj : in Root) return Boolean;

   procedure OK (Obj : in out Root)
       with Pre => Is_OK (Obj);                                -- OK. {13;1}

   procedure Ugh (Obj : in out Blah) is abstract
       with Pre => Is_OK (Obj);                                -- ERROR: {13;1}

   procedure Ugly (Obj : in out Blah) is abstract
       with Post => Is_OK (Obj);                               -- ERROR: {13;1}

   procedure Flub (Obj : in out Blah) is null
       with Pre => Is_OK (Obj);                                -- ERROR: {13;1}

   procedure Flab (Obj : in out Blah) is null
       with Post => Is_OK (Obj);                               -- ERROR: {13;1}

end B611005;
