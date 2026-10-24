-- B455002.A

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687 and
--     F08630-91-C-0015, the U.S. Government obtained unlimited rights in the
--     software and documentation contained herein.  Unlimited rights are
--     defined in DFAR 252.227-7013(a)(19).  By making this public release,
--     the Government intends to confer upon all recipients unlimited rights
--     equal to those held by the Government.  These rights include rights to
--     use, duplicate, release or disclose the released technical data and
--     computer software in whole or in part, in any manner and for any purpose
--     whatsoever, and to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--     Check that a universal fixed multiplying operator cannot be used in
--     a context where the result type is univeral fixed.
--
-- CHANGE HISTORY:
--      18 MAR 99   RLB    Initial version
--      28 JUN 16   RLB    Corrected error tags and overlong lines.
--!

procedure B455002 is

     type Fixed is delta 0.5 range -10.0 .. 10.0;
     F : Fixed := 0.0;
     F2 : Fixed := 2.0;

     I : Integer := 1;

begin

     F := F * 0.5;               -- OK.
     F := F * I;                 -- OK.
     F := F / 2.0;               -- OK.
     F := F / I;                 -- OK.

     F := 1.0 * 0.5;             -- OK.
     F := F * F2;                -- OK.
     F := F * F2 * 0.5;          -- ERROR: F2 * 0.5 result is universal fixed.
     F := F * F2 * 2;            -- OK.
     F := F * Fixed(F2 * 0.5);   -- OK.
     F := (F * F2) * 2.0;        -- ERROR: F * F2 result is universal fixed.
     F := (F * F2) * 2;          -- OK.
     F := Fixed(F * F2) * 2.0;   -- OK.

     F := 1.0 / 2.0;             -- OK.
     F := F / F2;                -- OK.
     F := F * F2 / 2.0;          -- ERROR: F2 / 2.0 result is universal fixed.
     F := F * F2 / 2;            -- OK.
     F := F * Fixed(F2 / 2);     -- OK.
     F := (F / F2) * 2;          -- OK.
     F := (F / F2) * 2.0;        -- ERROR: F * F2 result is universal fixed.
     F := Fixed(F / F2) * 2.0;   -- OK.

end B455002;
