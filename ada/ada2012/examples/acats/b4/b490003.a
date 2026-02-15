-- B490003.A
--
--                                     Grant of Unlimited Rights
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
--                                                DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                                 Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
-- OBJECTIVE:
--      Check that statically unevaluated static expressions do not
--      make a static expression illegal.
--
-- TEST DESCRIPTION:
--
--     This test checks the proper implementation of the notion of statically
--     unevaluated, as defined in (4.9(32.1/3-32.6/3)).
--
--     We check that each of the following conditions for an expression being
--     statically unevaluated (and therefore not evaluated within a static
--     expression) are properly implemented:
--      (A) The right operand of a static short-circuit control form whose
--          value is determined by its left operand (4.9(32.2/3));
--      (B) A dependent_expression of an if_expression whose associated
--          condition is static and equals False (4.9(32.3/3));
--      (C) A condition or dependent_expression of an if_expression where
--          the condition corresponding to at least one preceding
--          dependent_expression of the if_expression is static and
--          equals True (4.9(32.4/3));
--      (D) A dependent_expression of a case_expression whose
--          selecting_expression is static and whose value is not covered
--          by the corresponding discrete_choice_list (4.9(32.5/3));
--      (E) A choice_expression (or a simple_expression of a range that occurs
--          as a membership_choice of a membership_choice_list) of a static
--          membership test that is preceded in the enclosing
--          membership_choice_list by another item whose individual membership
--          test (see 4.5.2) statically yields True (4.9(32.6/3)).
--      (F) Choices in a case expression are never statically unevaluated,
--          since they are needed for verifying that the case choices are
--          complete, which is a normal condition for legality of the case.
--          No specific rule in 4.9 covers this, but it is worth noting as
--          otherwise one might not realize this special case.
--
-- CHANGE HISTORY:
--        03 Mar 14   RBKD    Created test (for AdaCore).
--        02 Apr 14   RLB     Renamed submitted test for ACATS 4.0.
--                            Replaced error cascade of subtype S3 with
--                            an qualifed expression, added named number to
--                            make a few test cases more realistic.
--        19 Nov 19   RLB     Moved error tags, added error location
--                            indicators.
--!
package B490003 is
   Debug_Cnt : constant := 0;

   C1 : constant :=
          Boolean'Pos (True or else 1=1/0);                -- OK. (A)  {1:4}
   C2 : constant :=
          (if Debug_Cnt = 0 then 1 else 1 / Debug_Cnt);    -- OK. (B)  {1:4}
   C3 : constant :=
          (case True is
           when 0=1 => Positive'(Debug_Cnt), when 2=2 => 1); -- OK. (D) {2:4}
   C4 : constant :=
          Boolean'Pos (if True then True
                       else 1 in 0 .. 1/Debug_Cnt);        -- OK. (B)  {2:4}
   C5 : constant :=
          (case Integer'(4) is
             when Integer => 1,
             when others  => 1/Debug_Cnt);                 -- OK. (D)  {3:4}
   C6 : constant :=
          (case Debug_Cnt /= 0 is
             when Debug_Cnt=1 => 1/Debug_Cnt,
             when 2=2 => 1);                               -- ERROR: (D) {3:4}
   C7 : constant :=
          Boolean'Pos (1 in 1 | 2/0);                      -- OK. (E)  {1:4}
   C8 : constant :=
          (if True then 1
           elsif 1/0=0 then 2 else 3);                     -- OK. (C)  {2:4}
   C9 : constant :=
          Boolean'Pos
            (1.0 in 1.0 | 1.0 / 0.0);                      -- OK. (E)  {2:4}
   C10 : constant :=
           Boolean'Pos
             (2.0 in 1.0 | 1.0 / 0.0);                     -- ERROR: (E) {2:4}

   subtype S1 is String (1 .. 2);
   subtype S2 is String (1 .. 3);

   Scon : constant String := "ABC";                        -- OK. {4}

   C11 : constant :=
           Boolean'Pos
             (Scon in S1 | S2 | S2'("12"));                -- OK. (E)  {2:4}
   C12 : constant :=
           Boolean'Pos
             (Scon in S2'("12") | S2 | S1);                -- ERROR: (E) {2:4}
   C13 : constant :=
           Boolean'Pos
             (Scon in "ABC" | S2'("12"));                  -- OK. (E)  {2:4}
   C14 : constant :=
           Boolean'Pos
             (Scon in "AB1" | "AB2" | S2'("12"));          -- ERROR: (E) {2:4}
   C15 : constant :=
           (if Debug_Cnt = 0 then 1
            else Boolean'Pos (Positive'(Debug_Cnt) = 0));  -- OK. (B)  {2:4}
   C16 : constant :=
           (if Debug_Cnt /= 0 then 1
            else Boolean'Pos (Positive'(Debug_Cnt) = 0));  -- ERROR: (B) {2:4}
   C17 : constant :=
           (if True then 1
            else (case 1 is
                    when 1/Debug_Cnt => 2,
                    when others => 3));                    -- ERROR: (F) {4:4}

end B490003;
