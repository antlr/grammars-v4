-- B611007.A
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
--     Check that Pre'Class and Post'Class cannot be specified for an entry of
--     an untagged task or protected type.
--
--     Check that Pre'Class and Post'Class cannot be specified for a protected
--     subprogram of an untagged protected type.
--
-- CHANGE HISTORY:
--     24 Feb 2016   RLB   Created test.
--     28 Mar 2016   RLB   Added error location codes.
--     12 Dec 2017   RLB   Corrected test to avoid violating rule added by
--                         Binding Interpretation AI12-0166-1.
--
--!
package B611007 is

   task type Tsk1 (D : Natural) is

      entry E1 (N : Natural) with Pre => N > D;                 -- OK. {35}

      entry E2 (N : Natural) with Pre'Class => N > D;           -- ERROR: {35}

      entry E3 (N : in out Natural) with Post => N > D;         -- OK. {42}

      entry E4 (N : in out Natural) with Post'Class => N > D;   -- ERROR: {42}

   private

      entry E5 (N : Natural) with Pre => N > D;                 -- OK. {35}

      entry E6 (N : Natural) with Pre'Class => N > D;           -- ERROR: {35}

      entry E7 (N : in out Natural) with Post => N > D;         -- OK. {42}

      entry E8 (N : in out Natural) with Post'Class => N > D;   -- ERROR: {42}

   end Tsk1;

   protected type Prot1 (D : Natural) is

      function Is_Open return Boolean;

      function Mode return Natural with Pre => D > 1;           -- OK. {41}

      function Foob (N : Natural) return Natural
         with Pre'Class => N > D;                               -- ERROR: {15}

      function La_Mode return Natural
         with Post => La_Mode'Result > D;                       -- OK. {15}

      function Pie return Natural
         with Post'Class => Pie'Result > D;                     -- ERROR: {15}

      procedure P1 (N : in out Natural)
         with Pre => N > D;                                     -- OK. {15}

      procedure P2 (N : in out Natural)
         with Pre'Class => N > D;                               -- ERROR: {15}

      procedure P3 (N : in out Natural)
         with Post => N > D;                                    -- OK. {15}

      procedure P4 (N : in out Natural)
         with Post'Class => N > D;                              -- ERROR: {15}

      entry E1 (N : in out Natural) with Pre => N > D;          -- OK. {42}

      entry E2 (N : in out Natural) with Pre'Class => N > D;    -- ERROR: {42}

      entry E3 (N : in out Natural) with Post => N > D;         -- OK. {42}

      entry E4 (N : in out Natural) with Post'Class => N > D;   -- ERROR: {42}

   private

      function F1 return Natural with Pre => D >= 12;           -- OK. {39}

      function F2 (N : Natural) return Natural
         with Pre'Class => N > D;                               -- ERROR: {15}

      function F3 return Natural
         with Post => F3'Result > D;                            -- OK. {15}

      function F4 return Natural
         with Post'Class => F4'Result > D;                      -- ERROR: {15}

      procedure P5 (N : in out Natural)
         with Pre => N > D;                                     -- OK. {15}

      procedure P6 (N : in out Natural)
         with Pre'Class => N > D;                               -- ERROR: {15}

      procedure P7 (N : in out Natural)
         with Post => N > D;                                    -- OK. {15}

      procedure P8 (N : in out Natural)
         with Post'Class => N > D;                              -- ERROR: {15}

      entry E5 (N : in out Natural) with Pre => N > D;          -- OK. {42}

      entry E6 (N : in out Natural) with Pre'Class => N > D;    -- ERROR: {42}

      entry E7 (N : in out Natural) with Post => N > D;         -- OK. {42}

      entry E8 (N : in out Natural) with Post'Class => N > D;   -- ERROR: {42}

      M : Natural := 0;
   end Prot1;

end B611007;


