-- B952005.A
--
--                             Grant of Unlimited Rights
--
--     AdaCore holds unlimited rights in the software and documentation
--     contained herein. Unlimited rights are the same as those granted
--     by the U.S. Government for older parts of the Ada Conformity
--     Assessment Test Suite, and are defined in DFAR 252.227-7013(a)(19).
--     By making this public release, AdaCore intends to confer upon all
--     recipients unlimited rights equal to those held by the Ada Conformity
--     Assessment Authority. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever,
--     and to have or permit others to do so.
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
--    Check that an entry with an overriding indicator of
--    "overriding" implements an inherited subprogram.
--
--    Check that an entry with an overriding indicator of
--    "not overriding" does not implement an inherited subprogram.
--
-- TEST DESCRIPTION:
--    We create a variety of interfaces along with a variety of task and
--    protected types and single objects to check that the rules are
--    enforced correctly. The errors found are (A) no implemented subprogram
--    for "overrding", and [B] subprogram implemented for "not overriding".
--
--    Note that the similar rules for protected subprograms are found in
--    subclause 9.4, and the similar test for those cases is found there.
--
-- CHANGE HISTORY:
--    02 June 2005 - H K - Initial Version.
--    27 June 2019 - RLB - Split into two parts, added test cases, updated
--                         for inclusion in the ACATS.
--!

package B952005 is

   --------------------
   --  Parent Types  --
   --------------------

   type Task_Interface is task interface;
   procedure A (TI : in out Task_Interface; X : out Integer)  is abstract;
   procedure C (TI : in out Task_Interface; X : out Integer)  is abstract;
   procedure E (TI : in out Task_Interface; X : out Integer)  is abstract;
   procedure F (TI : in out Task_Interface; X : out Integer)  is abstract;
   procedure G (TI : access Task_Interface; X : out Integer)  is abstract;
   procedure H (TI : access Task_Interface; X : out Integer)  is abstract;

   type Protected_Iface is protected interface;
   procedure J (PI : in out Protected_Iface; X : out Integer) is abstract;
   procedure L (PI : in out Protected_Iface; X : out Integer) is abstract;
   procedure N (PI : in out Protected_Iface; X : out Integer) is abstract;
   procedure P (PI : in out Protected_Iface; X : out Integer) is abstract;
   procedure Q (PI : access Protected_Iface; X : out Integer) is abstract;
   procedure R (PI : access Protected_Iface; X : out Integer) is abstract;

   -------------
   --  Types  --
   -------------

   task type Task_1 is new Task_Interface with
                     entry     A (X : out Integer);  -- OK. {22;1}
          overriding entry     B (X : out Integer);  -- ERROR: [A] {11;1}
      not overriding entry     C (X : out Integer);  -- ERROR: [B] {7;1}
      not overriding entry     D (X : out Integer);  -- OK. {7;1}
          overriding entry     E (X : in Character); -- ERROR: [A] {11;1}
                     entry     F (X : out Integer);  -- OK. {22;1}
          overriding entry     G (X : out Integer);  -- OK. {11;1}
      not overriding entry     H (X : out Integer);  -- ERROR: [B] {7;1}
   end Task_1;

   protected type Protected_1 is new Protected_Iface with
                     entry     J (X : out Integer);  -- OK. {22;1}
          overriding entry     K (X : out Integer);  -- ERROR: [A] {11;1}
      not overriding entry     L (X : out Integer);  -- ERROR: [B] {7;1}
      not overriding entry     M (X : out Integer);  -- OK. {7;1}
          overriding entry     N (X : in Character); -- ERROR: [A] {11;1}
                     entry     P (X : out Integer);  -- OK. {22;1}
          overriding entry     Q (X : out Integer);  -- OK. {11;1}
      not overriding entry     R (X : out Integer);  -- ERROR: [B] {7;1}
   end Protected_1;

   task type Task_2 is
                     entry     A (X : out Integer);  -- OK. {22;1}
          overriding entry     B (X : out Integer);  -- ERROR: [A] {11;1}
      not overriding entry     C (X : out Integer);  -- OK. {7;1}
   end Task_2;

   protected type Protected_2 is
                     entry     J (X : out Integer);  -- OK. {22;1}
          overriding entry     K (X : out Integer);  -- ERROR: [A] {11;1}
      not overriding entry     L (X : out Integer);  -- OK. {7;1}
   end Protected_2;

   ----------------------
   --  Single Objects  --
   ----------------------

   task Task_3 is new Task_Interface with
                     entry     A (X : out Integer);  -- OK. {22;1}
          overriding entry     B (X : out Integer);  -- ERROR: [A] {11;1}
      not overriding entry     C (X : out Integer);  -- ERROR: [B] {7;1}
      not overriding entry     D (X : out Integer);  -- OK. {7;1}
          overriding entry     E (X : in Character); -- ERROR: [A] {11;1}
                     entry     F (X : out Integer);  -- OK. {22;1}
          overriding entry     G (X : out Integer);  -- OK. {11;1}
      not overriding entry     H (X : out Integer);  -- ERROR: [B] {7;1}
   end Task_3;

   protected Protected_3 is new Protected_Iface with
                     entry     J (X : out Integer);  -- OK. {22;1}
          overriding entry     K (X : out Integer);  -- ERROR: [A] {11;1}
      not overriding entry     L (X : out Integer);  -- ERROR: [B] {7;1}
      not overriding entry     M (X : out Integer);  -- OK. {7;1}
          overriding entry     N (X : in Character); -- ERROR: [A] {11;1}
                     entry     P (X : out Integer);  -- OK. {22;1}
          overriding entry     Q (X : out Integer);  -- OK. {11;1}
      not overriding entry     R (X : out Integer);  -- ERROR: [B] {7;1}
   end Protected_3;

   task Task_4 is
                     entry     A (X : out Integer);  -- OK. {22;1}
          overriding entry     B (X : out Integer);  -- ERROR: [A] {11;1}
      not overriding entry     C (X : out Integer);  -- OK. {7;1}
   end Task_4;

   protected Protected_4 is
                     entry     J (X : out Integer);  -- OK. {22;1}
          overriding entry     K (X : out Integer);  -- ERROR: [A] {11;1}
      not overriding entry     L (X : out Integer);  -- OK. {7;1}
   end Protected_4;

end B952005;
