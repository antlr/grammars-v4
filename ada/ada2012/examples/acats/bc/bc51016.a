-- BC51016.A
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
--      Check that, if the reserved word "abstract" appears in the declaration
--      of a formal private type, the reserved word "tagged" must also appear.
--      Check that, if the reserved word "abstract" appears in the declaration
--      of a formal derived type, the reserved words "with private" must also
--      appear. Check that a tagged type derived from a non-tagged generic
--      formal private or derived type is illegal.
--
-- TEST DESCRIPTION:
--      Verify for formal private and formal derived types, as appropriate.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      17 Feb 95   SAIC    Updated prologue wording.
--      06 Feb 18   RLB     Corrected format of error tags, added error
--                          location indicators.
--
--!

package BC51016 is

   type Abstract_Type is abstract tagged record     -- Abstract type.
      Field : String (1 .. 5);
   end record;

   type Non_Tagged_Type is record
      Field : String (1 .. 5);
   end record;

   generic
      type Formal_Private_Type is abstract private;             -- ERROR: {7;1}
                                -- Formal type missing reserved word "tagged."
   package P1 is end;

   generic
      type Formal_Derived_Type is abstract new Abstract_Type;   -- ERROR: {7;1}
                         -- Formal type missing reserved words "with private."
   package P2 is end;

   generic
      type Formal_Private is private;                           -- OK. {7;1}
   package P3 is
      type Derived_Type_1 is new Formal_Private with private;   -- ERROR: {7;1}
                                                 -- Parent type is not tagged.

      type Derived_Type_2 is new Formal_Private with null record;-- ERROR:{7;1}
                                                 -- Parent type is not tagged.
   private
      type Derived_Type_1 is new Formal_Private
                                         with record -- OPTIONAL ERROR: {1:7;0}
         Another_Field : Integer := 345;         -- Parent type is not tagged.
      end record;

   end P3;


   generic
      type Formal_Derived is new Non_Tagged_Type;                -- OK. {7;1}
   package P4 is
      type Derived_Type_1 is new Formal_Derived with null record;-- ERROR:{7;1}
                                                 -- Parent type is not tagged.
   end P4;


end BC51016;
