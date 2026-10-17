-- B611011.A
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
--     Check that the Old attribute cannot be used inside a
--     precondition expression.
--
--     Check that the Old attribute cannot be used inside of the specification
--     of a generic unit, other than in postconditions.
--
-- CHANGE HISTORY:
--     26 Feb 2016   RLB   Created test.
--     28 Mar 2016   RLB   Added error location codes.
--
--!
package B611011 is

    function F01 (I : in out Natural) return Natural
       with Pre => I'Old = I;                                -- ERROR:

    function F02 (I : in out Natural) return Natural
       with Post => I'Old = I;                               -- OK.

    procedure P03 (S : in String; I : in out Natural)
       with Pre => S(I)'Old = 'J';                           -- ERROR:

    procedure P04 (S : in String; I : in out Natural)
       with Post => S(I)'Old = 'J';                          -- OK.

    procedure P05 (S : in String; I : in out Natural)
       with Pre => S(I'Old) = 'J';                           -- ERROR:

    procedure P06 (S : in String; I : in out Natural)
       with Post => S(I'Old) = 'J';                          -- OK.

    type Root is tagged record
       I : Natural;
    end record;

    function F40 (Obj : in Root; N : in out Natural) return Natural
       with Post'Class => N'Old = N;                         -- OK.

    function F41 (Obj : in Root; N : in out Natural) return Natural
       with Pre'Class => N'Old = N;                          -- ERROR:

    procedure P42 (Obj : access Root; N : in out Natural)
       with Post'Class=> Obj'Old.I = N;                      -- OK. {26;7}

    procedure P43 (Obj : access Root; N : in out Natural)
       with Pre'Class=> Obj'Old.I = N;                       -- ERROR: {25;7}

    procedure P44 (Obj : access Root; N : in out Natural)
       with Post'Class=> Obj.I'Old = N;                      -- OK. {26;5}

    procedure P45 (Obj : access Root; N : in out Natural)
       with Pre'Class=> Obj.I'Old = N;                       -- ERROR: {25;5}

    generic
       Foo : in out Natural;
    package Bar is

       procedure Blah
          with Post => Foo'Old = Foo;                        -- OK. {24;7}

       pragma Assert (Foo'Old in 1 .. 10);                   -- ERROR: {23;13}

       function Flub (N : in Natural := Foo'Old) return Float;-- ERROR: {41;15}

       function Flab (S : in String) return Float
          with Pre => S(Foo'Old) = 'R';                      -- ERROR: {25;8}

    end Bar;

end B611011;


