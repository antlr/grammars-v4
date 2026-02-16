-- BA21A03.A
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
--      Check that each of the following constructs is illegal within a
--      library package declaration to which a pragma Preelaborate applies:
--
--         (a) A call to a nonstatic function.
--         (b) A primary that is a name of an object, including within the
--             default expression for a default-initialized component, if the
--             name is not a static expression and does not statically denote
--             a discriminant of an enclosing type.
--         (c) A declaration of an object that is initialized by default of a
--             type that does not have preelaborable initialization
--             (abbreviated P_I in the comments below):
--             (1) a task type;
--             (2) a controlled type with a user-defined Initialize;
--             (3) a type with a component of a private type that does
--                 not have pragma Preelaborable_Initialization.
--         (d) An extension aggregate with an ancestor subtype mark denoting
--             a subtype of a controlled type that does not have preelaborable
--             initialization.
--
--      Check that each of the following constructs is legal within a library
--      package declaration to which a pragma Preelaborate applies:
--
--         (e) A call to a static function.
--         (f) A reference to an attribute of an object, in particular 'Access
--             and 'Address.
--         (g) A primary that is a name of an object, if the name statically
--             denotes a discriminant of an enclosing type.
--         (h) A declaration of a named access type.
--
-- TEST DESCRIPTION:
--      Declare various supporting types, objects, and subprograms in a
--      preelaborated package declaration (foundation code).
--      Verify the illegality of each of the constructs (a)-(f) above in
--      the visible or private part of a library package declaration which
--      contains a pragma Preelaborate; also verify the legality of
--      (e)-(h) above.
--
--
-- CHANGE HISTORY:
--      05 Apr 95   SAIC    Initial prerelease version.
--      22 Mar 07   RLB     Updated objective and comments; added
--                          Initialize to ensure non-P_I. Updated to use
--                          nearly identical existing foundation code rather
--                          than duplicating that and its commentary here.
--      20 Aug 07   RLB     Corrected various errors in test.
--
--!

with FA21A00;
with System;
package BA21A03 is

   pragma Preelaborate (BA21A03);


   type New_Controlled is new FA21A00.My_Controlled with record
      S : String (1 .. 7);
   end record;

   type Use_of_Disc (Disc: FA21A00.My_Int) is record
      Next: FA21A00.My_Int := FA21A00.My_Int'Succ(Disc);            -- OK.
                                   -- Primary which statically denotes a
                                   -- discriminant of an enclosing type (g).
   end record;



   Call  : FA21A00.My_Int := FA21A00.Func;                        -- ERROR:
                                             -- Call to nonstatic function (a).

   Prim  : FA21A00.RecPrimDefault;                                -- ERROR:
                                          -- Non-static primary (in component's
                                          -- default expression) (b).

   Task1 : FA21A00.Tsk(1);                                        -- ERROR:
                                               -- Object of a task type (c)(1).

   Cont  : FA21A00.My_Controlled;                                 -- ERROR:
               -- Default-initialized object of non P_I controlled type (c)(2).

   Ptr   : FA21A00.AccTag := FA21A00.Tag1'Access;                 -- OK.
                             -- Name of an object, but not as a primary (f).

   type Access_to_Tag is access constant FA21A00.Tag;             -- OK.
                                                   -- Named access type (h).
private

   Tag2  : FA21A00.Tag := FA21A00.Tag1;                           -- ERROR:
                                       -- Primary that is a name of an object
                                       -- which is not a static expression (b).

   Arr   : FA21A00.PrivComp;                                      -- ERROR:
         -- Default-initialized component of a private type without P_I (c)(3).

   Aggr  : New_Controlled :=
     (FA21A00.My_Controlled with "mistake");                      -- ERROR:
          -- Extension aggregate with ancestor denoting a type without P_I (d).

   Stat  : FA21A00.My_Int :=
     FA21A00.My_Int'Max (FA21A00.Three, FA21A00.Zero);            -- OK.
                                             -- Call to static function (e).

   Addr  : System.Address := FA21A00.Tag1'Address;                -- OK.
                             -- Name of an object, but not as a primary (f).

end BA21A03;
