-- B3930112.A
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
--    ISO/IEC 18017 and any applicable ACAA procedures.
--
--*
--
-- OBJECTIVE
--     See B3930110.A.
--
-- TEST DESCRIPTION
--     See B3930110.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         B3930110.A
--         B3930111.A
--      -> B3930112.A
--         B3930113.A
--
-- PASS/FAIL CRITERIA:
--     See B3930110.A.
--
-- CHANGE HISTORY:
--      05 Jan 2015   RLB   Created test, patterned after examples in
--                          AI05-0068-1.
--
--!

with B393011_P;
package B393011_S is
   type TP is abstract new B393011_P.I with private;
   -- Op[1] inherited here as abstract.
   package Inner1 is
      procedure Inner_Op1 (X : TP) renames Op;           -- OK.
         -- Inner_Op1 is abstract, which is OK as it is not primitive for T.
   end Inner1;
   --procedure Org_Op1 (X : TP) renames Op;
      -- OK, but would make the full type TP illegal, as it is an abstract
      -- primitive routine of a non-abstract type.
private
   type TP is new B393011_P.I with null record;
   -- Op[1] inherited here as "requires overriding".
   package Inner2 is
      procedure Inner_Op2 (X : TP) renames Op;           -- ERROR:
         -- Violates 8.5.4(5.1/2), cannot rename a subprogram that requires
         -- overriding.
   end Inner2;
   procedure Org_Op2 (X : TP) renames Op;                -- ERROR:
      -- Violates 8.5.4(5.1/2), cannot rename a subprogram that requires
      -- overriding.
   procedure Op (X : TP) is null;       -- Op[3]
   procedure Org_Op3 (X : TP) renames Op;                -- OK.
      -- Renames Op[3].
   package Inner3 is
      procedure Inner_Op3 (X : TP) renames Op;           -- OK.
   end Inner3;
end B393011_S;
