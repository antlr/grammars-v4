-- BA120102.A
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
--     See BA120100.A.
--
-- TEST DESCRIPTION
--     See BA120100.A.
--
-- TEST FILES:
--     This test consists of the following files:
--         BA120100.A
--         BA120101.A
--         BA120102.A
--         BA120103.A
--         BA120104.A
--         BA120105.A
--
-- PASS/FAIL CRITERIA:
--     See BA120100.A.
--
-- CHANGE HISTORY:
--    29 Nov 2004   PHL   Initial version.
--    26 Mar 2007   RLB   Created ACATS version from submitted test.
--    06 Feb 2018   RLB   Revised to use Possible Error to better reflect
--                        intent of test. Added error location indicators.
--
--!

limited with BA12010.Pak3;   -- POSSIBLE ERROR: [Set2] {1;1}
with BA12010.Renpak21;
with BA12010.Renpak31;
use type BA12010.Renpak31.T; -- POSSIBLE ERROR: [Set2] {1;1} A use clause that
                             --    names an entity declared in a package
                             --    mentioned by a previous limited with.
use BA12010.Renpak21;
limited with BA12010.Pak2;   -- ERROR: {1;1} Limited with clause in same
                             --        context clause as a use clause naming an
                             --        entity in the package Pak2 (a rename of
                             --        the child package Pak21)
package BA12010.Pak6 is
end BA12010.Pak6;
