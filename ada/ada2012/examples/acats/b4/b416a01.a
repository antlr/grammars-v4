-- B416A01.A
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
--     When both Constant_Indexing and Variable_Indexing are both specified,
--     check that a generalized indexing is illegal if it is called in
--     variable contexts when the prefix is a constant and Constant_Indexing
--     specifies a function returning an ordinary object.
--
-- HISTORY:
--      14 May 2015   RLB   Created test.

with F416A00; use F416A00;
procedure B416A01 is

   Maze : Window;

   procedure Limit (X : in out Natural) is
   begin
      if X > 20 then
         X := 20;
      end if;
   end Limit;

   procedure Sink (X : in Natural) is null;

   procedure Test (AMaze : in Window) is
       -- Try a variety of variable and constant contexts,
       -- with constant and variable prefixes:
   begin
      if Maze("Blinky").C /= Red then                          -- OK.
         null;
      end if;

      Maze("User  ").K := Ghost;                               -- OK.

      Limit (Maze("Clyde ").X);                                -- OK.

      Sink (Maze("Clyde ").Y);                                 -- OK.

      declare
         S1 : Sprite renames Maze("Clyde ");                   -- OK.
      begin
         Maze("Blinky") := S1;                                 -- OK.
      end;

      case Maze("Pinky ").K is                                 -- OK.
         when Ghost => null;
         when others => raise Program_Error;
      end case;

      if AMaze("Blinky").C /= Red then                         -- OK.
         null;
      end if;

      AMaze("User  ").K := Ghost;                              -- ERROR:

      Limit (AMaze("Clyde ").X);                               -- ERROR:

      Sink (AMaze("Clyde ").Y);                                -- OK.

      declare
         S1 : Sprite renames AMaze("Clyde ");                  -- OK.
            -- This is a variable context, but it is OK to rename
            -- a constant. S1 therefore is a constant.
      begin
         AMaze("Blinky") := S1;                                -- ERROR:

         S1.C := White;                                        -- ERROR:
      end;

      case AMaze("Pinky ").K is                                -- OK.
         when Ghost => null;
         when others => raise Program_Error;
      end case;

   end Test;

begin

   -- Create sprites:
   Create_Sprite (Maze, "User  ", X=> 10, Y => 10, K => Player, C => Yellow);

   Create_Sprite (Maze, "Blinky", X=> 1, Y => 1, K => Ghost, C => Red);

   Create_Sprite (Maze, "Pinky ", X=> 1, Y => 20, K => Ghost, C => Pink);

   Create_Sprite (Maze, "Inky  ", X=> 20, Y => 20, K => Ghost, C => Cyan);

   Create_Sprite (Maze, "Clyde ", X=> 20, Y => 1, K => Ghost, C => Orange);

   Test (Maze);

end B416A01;
