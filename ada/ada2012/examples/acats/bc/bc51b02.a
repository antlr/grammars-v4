-- BC51B02.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--      Check that the ancestor of a formal derived type may not be class-
--      wide. Check that a formal derived type may not have a known
--      discriminant part. Check that if a generic formal private or
--      derived subtype is indefinite, it must not appear in a context which
--      requires a definite subtype.
--
-- TEST DESCRIPTION:
--      A definite subtype is any subtype which is not indefinite. An
--      indefinite subtype is either:
--         a) An unconstrained array subtype.
--         b) A subtype with unknown discriminants (this includes class-wide
--            types).
--         c) A subtype with unconstrained discriminants without defaults.
--
--      The possible forms of indefinite formal subtype are as follows:
--
--         Formal derived types:
--            - Ancestor is an unconstrained non-formal array type
--            - Ancestor is an unconstrained formal array type
--            - Ancestor is a discriminated record type without defaults
--            - Ancestor is a discriminated tagged type
--            - Ancestor type has unknown discriminants
--            - Formal type has an unknown discriminant part
--
--         Formal private types:
--            - Formal type has an unknown discriminant part
--            - Formal type has a known discriminant part
--
-- TEST FILES:
--      The following files comprise this test:
--
--         FC51B00.A
--      -> BC51B02.A
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      21 Nov 19   RLB     Added error location indicators.
--!

with FC51B00;  -- Indefinite subtype definitions.
package BC51B02 is

   --
   -- Illegal forms of formal derived type:
   --

   subtype Tiny is Natural range 1 .. 2;
   generic

      type ClassWideAncestor is new FC51B00.Vector'Class
        with private;                                         -- ERROR: {1:7;1}
                                                     -- Ancestor is class-wide.

      type KnownDiscriminantPart (S : Tiny) is
        new FC51B00.Square;                                   -- ERROR: {1:7;1}
                            -- Formal derived type has known discriminant part.

   package Illegal is end;

   --
   -- Formal derived type cases:
   --

   generic
      type AncestorIsUnconstrainedNonFormalArray is new FC51B00.Matrix;
   package One is
      Object : AncestorIsUnconstrainedNonFormalArray;         -- ERROR: {7;1}
                                              -- Subtype is unconstrained.
   end One;


   generic
      type Item is private;
      type Index is (<>);
      type UnconstrainedArray is array (Index range <>) of Item;
      type AncestorIsUnconstrainedFormalArray is new UnconstrainedArray;
   package Two is
      Object : AncestorIsUnconstrainedFormalArray;            -- ERROR: {7;1}
                                              -- Subtype is unconstrained.
   end Two;


   generic
      type AncestorIsDiscrimRecordNoDefaults is new FC51B00.Square;
   package Three is
      type Rec is record
         Field : AncestorIsDiscrimRecordNoDefaults;           -- ERROR: {7;1}
                                    -- Component subtype is unconstrained.
      end record;
   end Three;


   generic
      type AncestorIsDiscrimTagged is new FC51B00.Square_Pair with private;
   package Four is
      Object : AncestorIsDiscrimTagged;                       -- ERROR: {7;1}
                                              -- Subtype is unconstrained.
   end Four;


   generic
      with package Formal_Package is new FC51B00.Signature (<>);
      type AncestorHasUnknownDiscrim is new Formal_Package.Vectors
        with private;
   package Five is
      Object : AncestorHasUnknownDiscrim;                     -- ERROR: {7;1}
                                              -- Subtype is unconstrained.
   end Five;


   generic
      type FormalHasUnknownDiscrim (<>) is new FC51B00.Vector with private;
   package Six is
      type Table is array (1 .. 3) of FormalHasUnknownDiscrim; -- ERROR: {7;1}
                                    -- Component subtype is unconstrained.
   end Six;


   --
   -- Formal private type cases:
   --

   generic
      type FormalHasUnknownDiscrim (<>) is tagged private;
   package Seven is
      Object : FormalHasUnknownDiscrim;                       -- ERROR: {7;1}
                                              -- Subtype is unconstrained.
   end Seven;


   generic
      type FormalHasKnownDiscrim (S : Natural) is private;
   package Eight is
      Object : FormalHasKnownDiscrim;                         -- ERROR: {7;1}
                                              -- Subtype is unconstrained.
   end Eight;

end BC51B02;
