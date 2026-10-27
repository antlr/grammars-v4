-- B3A10071.A
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
--    The ACAA has created and maintains the Ada Conformity Assessment Test
--    Suite for the purpose of conformity assessments conducted in accordance
--    with the International Standard ISO/IEC 18009 - Ada: Conformity
--    assessment of a language processor. This test suite should not be used
--    to make claims of conformance unless used in accordance with
--    ISO/IEC 18009 and any applicable ACAA procedures.
--*
--
-- OBJECTIVE:
--     See B3A10070.A.
--
-- TEST DESCRIPTION
--     See B3A10070.A.
--
-- TEST FILES:
--     This test consists of the following files:
--         B3A10070.A
--      -> B3A10071.A
--
-- PASS/FAIL CRITERIA:
--     See B3A10070.A.
--
-- CHANGE HISTORY:
--     13 Mar 2014  RLB  Created test for the remaining objectives of B38105A.
--!

limited with B3A1007_A;
package B3A1007 is

   type Inc_Int;
   type Inc_Float;
   type Inc_Fix;
   type Inc_Array;
   type Inc_Rec;
   type Inc_Rec2;
   type Inc_Rec_w_Disc (D : Boolean);
   type Inc_Tagged is tagged;
   type Inc_Tagged2 is tagged;
   type Inc_Tagged_w_Disc (D : Boolean) is tagged;
   type Inc_Access_Rec;

   -- Access types:
   type Acc01 is access Inc_Int range 4 .. 6;                  -- ERROR:

   type Acc02 is access Inc_Float range 0.0 .. 0.5;            -- ERROR:

   type Acc03 is access Inc_Fix range -1.0 .. 1.0;             -- ERROR:

   type Acc04 is access Inc_Array(1..3);                       -- ERROR:

   type Acc05 is access Inc_Rec;                               -- OK.

   type Acc06 is access Inc_Rec2(True);                        -- ERROR:

   type Acc07 is access Inc_Rec_w_Disc(False);                 -- OK.

   type Acc08 is access Inc_Tagged;                            -- OK.

   type Acc09 is access Inc_Tagged2(True);                     -- ERROR:

   type Acc10 is access Inc_Tagged_w_Disc(False);              -- OK.

   type Acc11 is access not null Inc_Access_Rec;               -- ERROR:

   type Acc12 is access B3A1007_A.An_Int range 4 .. 6;         -- ERROR:

   type Acc13 is access B3A1007_A.A_Float range 0.0 .. 0.5;    -- ERROR:

   type Acc14 is access B3A1007_A.A_Fix range -1.0 .. 1.0;     -- ERROR:

   type Acc15 is access B3A1007_A.An_Array(1..3);              -- ERROR:

   type Acc16 is access B3A1007_A.A_Rec;                       -- OK.

   type Acc17 is access B3A1007_A.A_Rec_w_Disc(True);          -- ERROR:
      -- Note: Items imported from a limited view never have discriminants,
      -- see 10.1.1(12.3/3).

   type Acc18 is access B3A1007_A.A_Tagged;                    -- OK.

   type Acc19 is access B3A1007_A.A_Tagged_w_Disc(False);      -- ERROR:

   type Acc20 is access not null B3A1007_A.An_Access_Rec;      -- ERROR:

   -- Legal access types:
   type Acc_Inc_Array is access Inc_Array;
   type Acc_An_Array is access B3A1007_A.An_Array;

   -- Test constraints on access-to-incomplete:
   subtype Sub_Acc_Inc_Array_1 is Acc_Inc_Array(1..5);         -- ERROR:
   subtype Sub_Acc_Inc_Array_2 is Acc_Inc_Array;               -- OK.
   subtype Sub_Acc_An_Array_1 is Acc_An_Array(6..9);           -- ERROR:
   subtype Sub_Acc_An_Array_2 is Acc_An_Array;                 -- OK.
   -- We don't test range constraints here as they wouldn't be legal on
   -- an access type anyway.

   -- Subtype tests:
   subtype Sub01 is Inc_Int range 4 .. 6;                      -- ERROR:

   subtype Sub02 is Inc_Float range 0.0 .. 0.5;                -- ERROR:

   subtype Sub03 is Inc_Fix range -1.0 .. 1.0;                 -- ERROR:

   subtype Sub04 is Inc_Array(1..3);                           -- ERROR:

   subtype Sub05 is Inc_Rec;                                   -- OK.

   subtype Sub06 is Inc_Rec2(True);                            -- ERROR:

   subtype Sub07 is Inc_Rec_w_Disc(False);                     -- ERROR:

   subtype Sub08 is Inc_Tagged;                                -- OK.

   subtype Sub09 is Inc_Tagged2(True);                         -- ERROR:

   subtype Sub10 is Inc_Tagged_w_Disc(False);                  -- ERROR:

   subtype Sub11 is not null Inc_Access_Rec;                   -- ERROR:

   subtype Sub12 is B3A1007_A.An_Int range 4 .. 6;             -- ERROR:

   subtype Sub13 is B3A1007_A.A_Float range 0.0 .. 0.5;        -- ERROR:

   subtype Sub14 is B3A1007_A.A_Fix range -1.0 .. 1.0;         -- ERROR:

   subtype Sub15 is B3A1007_A.An_Array(1..3);                  -- ERROR:

   subtype Sub16 is B3A1007_A.A_Rec;                           -- OK.

   subtype Sub17 is B3A1007_A.A_Rec_w_Disc(True);              -- ERROR:

   subtype Sub18 is B3A1007_A.A_Tagged;                        -- OK.

   subtype Sub19 is B3A1007_A.A_Tagged_w_Disc(False);          -- ERROR:

   subtype Sub20 is not null B3A1007_A.An_Access_Rec;          -- ERROR:


   -- Completions:
   type Inc_Int is range -1000 .. 1000;

   type Inc_Float is new Float;

   type Inc_Fix is delta 1.0 range -2.0 .. 2.0;

   type Inc_Array is array (Positive range <>) of Integer;

   type Inc_Rec is record
      C : Character;
   end record;

   type Inc_Rec2 (D : Boolean) is record
      I : Inc_Int;
   end record;

   type Inc_Rec_w_Disc (D : Boolean) is record
      N : Natural;
   end record;

   type Inc_Tagged is tagged record
      W : Wide_Character;
   end record;

   type Inc_Tagged2 (D : Boolean) is tagged record
      A : Inc_Array(1..10);
   end record;

   type Inc_Tagged_w_Disc (D : Boolean) is tagged record
      F : Float;
   end record;

   type Inc_Access_Rec is access Inc_Rec;

   -- Legal cases after the completion:
   type Acc24 is access Inc_Int range 4..6;
   type Acc25 is access Inc_Float range 0.0..0.25;
   type Acc26 is access Inc_Fix range -1.0..1.0;
   type Acc27 is access Inc_Array(1..2);
   type Acc28 is access Inc_Rec2(False);

end B3A1007;
