-- B940008.A
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
--    Check that a protected subprogram with an overriding indicator of
--    "overriding" implements an inherited subprogram.
--
--    Check that a protected subprogram with an overriding indicator of
--    "not overriding" does not implement an inherited subprogram.
--
-- TEST DESCRIPTION:
--    We create a variety of interfaces along with a variety of protected
--    types and single objects to check that the rules are enforced
--    correctly. The errors found are [A] no implemented subprogram for
--    "overrding", and [B] subprogram implemented for "not overriding".
--
--    Note that the similar rules for protected entries are found in
--    subclause 9.5.2, and the similar test for those cases is found there.
--
-- CHANGE HISTORY:
--    02 June 2005 - H K - Initial Version.
--    27 June 2019 - RLB - Split into two parts, added test cases, updated
--                         for inclusion in the ACATS.
--!

package B940008 is

   --------------------
   --  Parent Types  --
   --------------------

   type Protected_Iface is protected interface;
   function  G (PI : in     Protected_Iface) return Integer   is abstract;
   function  I (PI : in     Protected_Iface) return Integer   is abstract;
   procedure J (PI : in out Protected_Iface; X : out Integer) is abstract;
   procedure L (PI : in out Protected_Iface; X : out Integer) is abstract;
   function  M (PI : access constant Protected_Iface) return Integer
                                                              is abstract;
   procedure P (PI : access Protected_Iface; X : out Integer) is abstract;
   procedure R (PI : access Protected_Iface; X : out Integer) is abstract;
   procedure S (PI : access Protected_Iface; X : out Integer) is abstract;

   -------------
   --  Types  --
   -------------

   protected type Protected_1 is new Protected_Iface with
                     function  G return Integer;     -- OK, {22;1}
          overriding function  H return Integer;     -- ERROR: [A] {11;1}
      not overriding function  I return Integer;     -- ERROR: [B] {7;1}
                     procedure J (X : out Integer);  -- OK. {22;1}
          overriding procedure K (X : out Integer);  -- ERROR: [A] {11;1}
      not overriding procedure L (X : out Integer);  -- ERROR: [B] {7;1}
          overriding function  M return Integer;     -- OK. {11;1}
      not overriding function  N return Integer;     -- OK. {7;1}
          overriding procedure P (X : out Integer);  -- OK. {11;1}
      not overriding procedure Q (X : out Integer);  -- OK. {7;1}
      not overriding procedure R (X : out Integer);  -- ERROR: [B] {7;1}
          overriding procedure S (X : in String);    -- ERROR: [A] {11;1}
   end Protected_1;

   protected type Protected_2 is
                     function  G return Integer;     -- OK, {22;1}
          overriding function  H return Integer;     -- ERROR: [A] {11;1}
      not overriding function  I return Integer;     -- OK. {7;1}
                     procedure J (X : out Integer);  -- OK. {22;1}
          overriding procedure K (X : out Integer);  -- ERROR: [A] {11;1}
      not overriding procedure L (X : out Integer);  -- OK. {7;1}
   end Protected_2;

   ----------------------
   --  Single Objects  --
   ----------------------

   protected Protected_3 is new Protected_Iface with
                     function  G return Integer;     -- OK, {22;1}
          overriding function  H return Integer;     -- ERROR: [A] {11;1}
      not overriding function  I return Integer;     -- ERROR: [B] {7;1}
                     procedure J (X : out Integer);  -- OK. {22;1}
          overriding procedure K (X : out Integer);  -- ERROR: [A] {11;1}
      not overriding procedure L (X : out Integer);  -- ERROR: [B] {7;1}
          overriding function  M return Integer;     -- OK. {11;1}
      not overriding function  N return Integer;     -- OK. {7;1}
          overriding procedure P (X : out Integer);  -- OK. {11;1}
      not overriding procedure Q (X : out Integer);  -- OK. {7;1}
      not overriding procedure R (X : out Integer);  -- ERROR: [B] {7;1}
          overriding procedure S (X : in String);    -- ERROR: [A] {11;1}
   end Protected_3;

   protected Protected_4 is
                     function  G return Integer;     -- OK, {22;1}
          overriding function  H return Integer;     -- ERROR: [A] {11;1}
      not overriding function  I return Integer;     -- OK. {7;1}
                     procedure J (X : out Integer);  -- OK. {22;1}
          overriding procedure K (X : out Integer);  -- ERROR: [A] {11;1}
      not overriding procedure L (X : out Integer);  -- OK. {7;1}
   end Protected_4;

end B940008;
