-- B660003.A
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
--     Check that the modes of the parameters of an operator cannot have a
--     mode other than IN.
--
-- CHANGE HISTORY:
--       2 Feb 81   DAS     Created legacy test B65001A.
--      10 Mar 14   RLB     Created test from legacy test B65001A, now
--                          obsolete because functions (but not operators) can
--                          have parameters of other modes. Added enumeration
--                          and instance subtests.
--      19 Nov 19   RLB     Fixed overlong lines; added error location
--                          indicators.

package B660003 is

     type T is new Integer range 0 .. 100;

     type Color is (Yellow, Red, Green, Blue);

     function "+" (I : in out T; J : in T) return T;   -- ERROR: In Out.  {6}

     function "-" (I : out T) return T;                -- ERROR: Out.     {6}

     function "*" (I : T; J : out T) return T;         -- ERROR: Out.     {6}

     function "mod" (I : T; J : in out Color) return T;-- ERROR: In Out.  {6}

     function "/" (I : access T; J : T) return T;      -- OK. Access is   {6}
                                                       --     not a mode!

     function Flab (I : in out T; J : out T) return T; -- OK. Not an      {6}
                                                       --     operator.

     function ">=" (I : out Color; J : Color) return Boolean; -- ERROR: Out.{6}

     generic
         type D is (<>);
     function Gen1 (A : D; B : out D) return Boolean;

     generic
         type D is (<>);
     function Gen2 (A : in out D; B : in D) return Boolean;

     generic
         type D is (<>);
     function Gen3 (A : D; B : access D) return Boolean;

     function "and" is new Gen1 (T);                   -- ERROR: Out.     {6}

     function "xor" is new Gen2 (T);                   -- ERROR: In Out.  {6}

     function "or" is new Gen3 (T);                    -- OK. Access is   {6}
                                                       --     not a mode!

     function Foo is new Gen1 (T);                     -- OK. Not an      {6}
                                                       --     operator.

     function Bar is new Gen2 (T);                     -- OK. Not an      {6}
                                                       --     operator.

     function "<=" is new Gen1 (Color);                -- ERROR: Out.     {6}

     function "<" is new Gen2 (Color);                 -- ERROR: In Out.  {6}

     function ">" is new Gen3 (Color);                 -- OK. Access is   {6}
                                                       --     not a mode!

     function Glow is new Gen1 (Color);                -- OK. Not an
                                                       --     operator.   {6}

end B660003;
