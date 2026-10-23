-- BC55001.A
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
--      Check that for a formal interface type, then the following entities
--      that are of the formal type are illegal because the type is abstract:
--      an object created by an object declaration or an allocator,
--      a component, an aggregate, a generic formal object of mode in, the
--      result type of a non-abstract function, the designated type of the
--      access result type of a non-abstract function, the result type of a
--      generic function, and the designated type of the access result type
--      of a generic function.
--
-- TEST DESCRIPTION:
--      The test is based on test BC51013, adding test cases as needed.
--      The test checks 3.9.3(8/3) as modified by AI05-0073-1 and AI12-0203-1.
--      Note that we check rules that have to be tested in a body in a separate
--      test (assignment and extended return subtypes).
--
--
-- CHANGE HISTORY:
--      22 Nov 19  RLB  Created test from outline of BC51013.
--
--!

generic
   type Formal_Type is interface;                              -- OK.    {4;1}
   type Limited_Formal_Type is limited interface;              -- OK.    {4;1}
   type Protected_Formal_Type is protected interface;          -- OK.    {4;1}
   type Synchronized_Formal_Type is synchronized interface;    -- OK.    {4;1}
   type Task_Formal_Type is task interface;                    -- OK.    {4;1}
package BC55001 is

   type Abstract_Access is access Limited_Formal_Type;

   function Sink (Obj : Formal_Type'Class) return Natural;     -- OK.    {4;1}

   Object1 : Synchronized_Formal_Type;                         -- ERROR: {4;1}
                                                 -- Type of object is abstract.
   Object2 : aliased Protected_Formal_Type;                    -- ERROR: {4;1}
                                                 -- Type of object is abstract.

   Abs_Ptr : Abstract_Access := new Limited_Formal_Type;       -- ERROR: {4;1}
                                       -- Type of allocated object is abstract.

   type Rec is record
      Component : Task_Formal_Type;                            -- ERROR: {7;1}
   end record;                                -- Type of component is abstract.

   type Arr is array (1 .. 5) of Protected_Formal_Type;        -- ERROR: {4;1}
                                              -- Type of component is abstract.

   Tsk_Arr : array (1 .. 5) of Task_Formal_Type;               -- ERROR: {4;1}
                                              -- Type of component is abstract.

   Val1 : Natural := Sink (Formal_Type'(null record));         -- ERROR: {4;1}
                                              -- Type of aggregate is abstract.

   Val2 : Natural := Sink (Formal_Type'(others => <>));        -- ERROR: {4;1}
                                              -- Type of aggregate is abstract.

   generic
      Gen_Obj1 : in out Formal_Type;                           -- OK.    {7;1}
   procedure Generic_Proc_w_In_Out (Obj : in Formal_Type'Class);

   generic
      Gen_Obj2 : in Limited_Formal_Type;                       -- ERROR: {7;1}
                       -- Type of generic formal object of mode in is abstract.
   procedure Generic_Proc_w_In (Obj : in Formal_Type'Class);


   function Non_Abstract_Function return Protected_Formal_Type;-- ERROR: {4;1}
                       -- Function is not abstract but result type is abstract.

   function Abstract_Function return Formal_Type is abstract;  -- OK.    {4;1}

   function Another_Non_Abstract_Function
      return access Synchronized_Formal_Type;                 -- ERROR: {1:4;1}
            -- Function is not abstract but designated result type is abstract.

   function Another_Abstract_Function
      return access Protected_Formal_Type is abstract;        -- OK. {1:4;1}

   function YA_Non_Abstract_Function return Abstract_Access;  -- OK.    {4;1}
            -- Not an access result type, so rule doesn't apply.

   generic
   function A_Generic_Function return Task_Formal_Type;       -- ERROR: {1:4;1}

   generic
   function A_Generic_Function_2 (Obj : in Limited_Formal_Type'Class)
      return Limited_Formal_Type'Class;                       -- OK.    {2:4;1}

   generic
   function A_Generic_Function_3
      return access Protected_Formal_Type;                    -- ERROR: {2:4;1}

   generic
   function A_Generic_Function_4 return Abstract_Access;      -- OK.    {1:4;1}

end BC55001;
