-- B611006.A
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
--     Check that, for a primitive operation of a type T, that a class-wide
--     precondition or postcondition expression cannot make calls to
--     nonprimitive operations of type T or functions of T'Class.
--
--     Check that, for a primitive operation of a type T, that a class-wide
--     precondition or postcondition expression cannot use a global object
--     of type T or T'Class as a parameter to a primitive operation of type T.
--
-- TEST DESCRIPTION:
--     These objectives descend from changes made by AI12-0113-1, part
--     of Technical Corrigendum 1 (ISO/IEC 8652:2012/Corr1:2016).
--     The purpose of these rules is to ensure that the interpretation of
--     class-wide precondition and postcondition expressions can be
--     meaningfully inherited. We don't want to allow calls to routines that
--     don't have equivalents for a descendant type, nor parameters that
--     wouldn't have a matching type for a descendant type.
--
-- CHANGE HISTORY:
--     04 Feb 2016   RLB   Created test.
--     22 Feb 2016   RLB   Corrected operations of Intf to be abstract.
--     28 Mar 2016   RLB   Generalized error messages, added location codes.
--     20 Jan 2017   RLB   Clarified error in Pack3.
--      2 Feb 2017   RLB   Corrected Pack5, which is not illegal.
--     28 Jan 2021   RLB   Removed the third objective, as it is incompatible
--                         with AI12-0412-1, a new Binding Interpretation.
--
--!
package B611006 is

   -- Correct root types:

   type Root is tagged null record;

   function Is_OK (Obj : Root) return Boolean;

   procedure Proc1 (Obj : in out Root)
                                with Pre'Class => Is_OK (Obj);   -- OK. {38}

   procedure Proc2 (Obj : in out Root)
                                with Post'Class => Is_OK (Obj);  -- OK. {38}

   type Intf is limited interface;

   function Is_Old (Obj : Intf) return Boolean is abstract;

   procedure Proc3 (Obj : in out Intf) is abstract
                                with Pre'Class => Is_Old (Obj);  -- OK. {38}

   procedure Proc4 (Obj : in out Intf) is abstract
                                with Post'Class => Is_Old (Obj); -- OK. {38}

   type NRT is new Root with null record;

   package Pack1 is
      function Fooey return NRT'Class;

      function Blooey return NRT;

      function Is_Bad (Obj : in NRT) return Boolean;

      function Is_Cool (Obj : in NRT'Class) return Boolean;

   end Pack1;

   -- First objective:

   procedure Proc1 (Obj : in out NRT)
             with Pre'Class => Is_OK (Obj);                      -- OK. {19}

   procedure Proc2 (Obj : in out NRT)
             with Post'Class => Is_OK (Obj);                     -- OK. {19}

   procedure Proc3 (Obj : in out NRT)
             with Pre'Class => Pack1.Is_Bad (Obj);               -- ERROR: {19}

   procedure Proc4 (Obj : in out NRT)
             with Post'Class => Pack1.Is_Bad (Obj);              -- ERROR: {19}

   -- As always, the situation with class-wide types is asymetrical:
   -- an object of a descendant of type NRT can be passed to a
   -- parameter of NRT'Class, but an object of type NRT'Class cannot be
   -- passed to a parameter of a descendant of type NRT. Thus Fooey
   -- below is illegal, but Is_Cool is legal.

   procedure Proc5 (Obj : in out NRT)
             with Pre'Class => Is_OK (Pack1.Fooey);              -- ERROR: {19}

   procedure Proc6 (Obj : in out NRT)
             with Post'Class => Is_OK (Pack1.Fooey);             -- ERROR: {19}

   procedure Proc7 (Obj : in out NRT)
             with Pre'Class => Is_OK (Pack1.Blooey);             -- ERROR: {19}

   procedure Proc8 (Obj : in out NRT)
             with Post'Class => Is_OK (Pack1.Blooey);            -- ERROR: {19}

   procedure Proc9 (Obj : in out NRT)
             with Pre'Class => Pack1.Is_Cool (Obj);              -- OK.    {19}

   procedure ProcA (Obj : in out NRT)
             with Post'Class => Pack1.Is_Cool (Obj);             -- OK.    {19}

   -- Second objective:
   -- Note: Because an object of type T freezes type T, and we cannot
   -- freeze T, the global objects have to be access objects of some sort.
   -- We assume that these objects are initialized in the body (which we
   -- don't show in this test).

   type Acc_NRT is access all NRT;
   Specific_Glob : Acc_NRT;

   type Acc_NRT_Class is access all NRT'Class;
   Classwide_Glob : Acc_NRT_Class;

   Unrelated_Glob : Boolean := True;

   procedure ProcB (Obj : in out NRT)
             with Pre'Class => Obj /= Specific_Glob.all;         -- ERROR: {19}

   procedure ProcC (Obj : in out NRT)
             with Post'Class => Obj /= Specific_Glob.all;        -- ERROR: {19}

   procedure ProcD (Obj : in out NRT)
             with Pre'Class => Obj /= Classwide_Glob.all;        -- ERROR: {19}

   procedure ProcE (Obj : in out NRT)
             with Post'Class => Obj /= Classwide_Glob.all;       -- ERROR: {19}

   procedure ProcF (Obj : in out NRT)
             with Pre'Class => Is_OK (Obj) and Unrelated_Glob;   -- OK. {19}

   procedure ProcG (Obj : in out NRT)
             with Post'Class => Is_OK (Obj) and Unrelated_Glob;  -- OK. {19}

end B611006;


