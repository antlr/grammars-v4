-- B392003.A
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
--      Check that:
--      A dispatching operation which overrides an inherited subprogram is
--      required to be subtype conformant with the inherited subprogram.
--      The declaration of dispatching operations does not allow the use of
--      subtypes which do not statically match the first subtype of the tagged
--      type (in a package).
--
-- TEST DESCRIPTION:
--      This test declares tagged types with discriminant, type extensions,
--      subtypes, and subprograms. Verify that compiler generates errors
--      for all cases as described in the objective.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      21 Dec 94   SAIC    Correct freezing violations.  Added "New_"
--                          to various subtypes for consistency.
--      07 Feb 18   RLB     Added error location indicators to reflect common
--                          error reporting strategies.
--
--!

package B392003 is

   type Short is range 1..10;

   type Uncons_Tagged (D : Short) is tagged null record;

   subtype Other_Subtype is integer range 1..10;

   function Init_2 return integer;

   subtype Init_Subtype is integer range 1..Init_2;

   subtype Compatible_Subtype is integer range 1..Other_Subtype'last;

   procedure Proc (P1: Uncons_Tagged; P2 : Other_Subtype);

   type New_Uncons_Tagged_1 is new Uncons_Tagged with null record;
   -- Inherited procedure Proc.

   procedure Proc (P1: New_Uncons_Tagged_1; P2 : integer);    -- ERROR: {4;1}
                                              -- P2 is not subtype conformant.

   type New_Uncons_Tagged_2 is new Uncons_Tagged with null record;
   -- Inherited procedure Proc.

   procedure Proc (P1: New_Uncons_Tagged_2;
                   P2 : Init_Subtype);                        -- ERROR: {1:4;1}
                                              -- P2 is not subtype conformant.
   type New_Uncons_Tagged_3 is new Uncons_Tagged with null record;
   -- Inherited procedure Proc.

   procedure Proc (P1: New_Uncons_Tagged_3;
                   P2 : Compatible_Subtype);                  -- OK. {1:4;1}

   --------------------------------------------------------------------------
   function Init_1 return Short;

   type Other_Uncons_Tagged (D : Short) is tagged null record;

   subtype Other_Uncons_Sub is Other_Uncons_Tagged;

   subtype Other_Cons_Sub_1 is Other_Uncons_Tagged (10);

   subtype Other_Cons_Sub_2 is Other_Uncons_Tagged (Short'last);

   subtype Other_Cons_Sub_3 is Other_Uncons_Tagged (Init_1);

   procedure Static_Match_Proc (P1 : Other_Uncons_Tagged;
                                P2 : Other_Uncons_Sub);       -- OK.{1:4;1}

   procedure Non_Static_Match_Proc1 (P1 : Other_Uncons_Tagged;
                                     P2 : Other_Cons_Sub_1);  -- ERROR: {1:4;1}
                                       -- Parameter does not statically match.

   procedure Non_Static_Match_Proc2 (P2 : Other_Cons_Sub_2);  -- ERROR: {4;1}
                                       -- Parameter does not statically match.

   procedure Non_Static_Match_Proc3 (P2 : Other_Cons_Sub_3);  -- ERROR: {4;1}
                                       -- Parameter does not statically match.

   function Static_Match_Func1 (P1 : Other_Uncons_Tagged)
     return Other_Uncons_Sub;                                 -- OK. {1:4;1}

   function Static_Match_Func2 (P1 : Other_Uncons_Sub)
     return Other_Uncons_Tagged;                              -- OK. {1:4;1}

   function Non_Static_Match_Func1 return Other_Cons_Sub_1;   -- ERROR: {4;1}
                                          -- Result does not statically match.

   function Non_Static_Match_Func2 (P1 : Other_Cons_Sub_2)
     return Other_Uncons_Tagged;                              -- ERROR: {1:4;1}
                                       -- Parameter does not statically match.

   function Non_Static_Match_Func3 (P1 : Other_Uncons_Tagged;
                                    P2 : Other_Cons_Sub_3)
     return Other_Uncons_Tagged;                              -- ERROR: {2:4;1}
                                       -- Parameter does not statically match.

   --------------------------------------------------------------------------
   type More_Uncons_Tagged (D : Short) is tagged null record;

   -- Primitive of More_Uncons_Tagged.
   procedure Other_Proc (P1: More_Uncons_Tagged);

   subtype New_More_Cons_Tagged_Sub_1 is More_Uncons_Tagged (15);

   procedure Other_Proc1 (P1: New_More_Cons_Tagged_Sub_1);    -- ERROR: {4;1}
                                       -- Parameter does not statically match.

   type Other_Short is range 10..20;

   -- A derived type with a new discriminant.
   type New_More_Uncons_Tagged (D : Other_Short)
     is new More_Uncons_Tagged (7) with null record;

   subtype New_More_Uncons_Tagged_Sub_1 is New_More_Uncons_Tagged;

   subtype New_More_Cons_Tagged_Sub is New_More_Uncons_Tagged (12);

   procedure Other_Proc2 (P1: New_More_Uncons_Tagged);        -- OK. {4;1}
   procedure Other_Proc3 (P1: New_More_Uncons_Tagged_Sub_1);  -- OK. {4;1}

   procedure Other_Proc4 (P1: New_More_Cons_Tagged_Sub);      -- ERROR: {4;1}
                                       -- Parameter does not statically match.

private
   subtype Private_New_More_Uncons_Subtype is New_More_Uncons_Tagged (4);
   procedure Other_Proc5 (P1: Private_New_More_Uncons_Subtype);-- ERROR: {4;1}
                                       -- Parameter does not statically match.

   procedure Other_Proc6 (P1: New_More_Cons_Tagged_Sub);      -- ERROR: {4;1}
                                       -- Parameter does not statically match.
end B392003;
