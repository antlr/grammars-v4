-- BD11002.A
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
--
--    Check that the usage names in an aspect_declaration given in the visible
--    part of a package are resolved at the end of the the visible part; in
--    particular, names declared in the private part cannot be used.
--
-- TEST DESCRIPTION:
--
--    This test checks the global visibility rules for aspect specifications.
--    We only try a few representative aspects; the test does not check
--    requirements for specific aspects.
--
-- CHANGE HISTORY:
--    09 Dec 15   RLB    Created test.

package BD11002 is

    type Priv (D : Natural) is tagged private;

    procedure Do_It1 (P : in out Priv)
       with Pre => Is_Valid(P);                                  -- OK.

    procedure Do_It2 (P : in out Priv)
       with Pre => Is_Great(P);                                  -- ERROR:

    procedure Do_It3 (P : in out Priv)
       with Pre => P.D >= 1;                                     -- OK.

    procedure Do_It4 (P : in out Priv)
       with Pre => P.Comp > 1;                                   -- ERROR:

    procedure Do_It5 (P : in out Priv)
       with Pre => Value(P) = Hide;                              -- ERROR:

    procedure Do_It6 (P : in out Priv)
       with Pre => Value(P) = Seek;                              -- OK.

    procedure Do_It7 (P : in out Priv)
       with Pre => Value(P) in Tiny;                             -- ERROR:

    procedure Do_It8 (P : in out Priv)
       with Pre => P /= Empty;                                   -- OK.

    function Is_Valid (P : in Priv) return Boolean;

    function Value (P : in Priv) return Natural
       with Pre => Is_Valid(P);                                  -- OK.

    Empty : constant Priv;

    subtype Sub1 is Natural
       with Dynamic_Predicate => Sub1 in 1 .. Seek;              -- OK.

    subtype Sub2 is Natural
       with Dynamic_Predicate => Sub2 in 1 .. Hide;              -- ERROR:

    subtype Sub3 is Natural
       with Dynamic_Predicate => Sub3 in Tiny | 27;              -- ERROR:

    type My_Int is range -100 .. 100
       with Size => Seek;                                        -- OK.

    type My_Flt is digits Float'Digits
       with Size => Hide;                                        -- ERROR:

    Seek : constant Natural := Integer'Size;

private

    Hide : constant Natural := Float'Size;

    subtype Tiny is Natural range 1 .. 10;

    function Is_Great (P : in Priv) return Boolean;

    type Priv (D : Natural) is tagged record
       Comp : Natural;
    end record;

    Empty : constant Priv := (D => 1, Comp => 0);

end BD11002;
