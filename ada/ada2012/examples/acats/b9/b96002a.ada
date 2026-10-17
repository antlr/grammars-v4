-- B96002A.ADA

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
-- CHECK THAT AN ARGUMENT TO THE DELAY STATEMENT MUST HAVE TYPE
-- DURATION. ALSO, CHECK THE TYPE OF ARGUMENTS PASSED TO "+" AND "-"
-- FUNCTIONS IN THE PACKAGE CALENDAR. TESTS ARE:
--   (A) TYPE OF ARGUMENT TO A DELAY STATEMENT.
--   (B) TYPE OF ARGUMENTS TO FUNCTION "+".
--   (C) TYPE OF ARGUMENTS TO FUNCTION "-".

-- CPP 8/14/84

WITH SYSTEM;
WITH CALENDAR;  USE CALENDAR;
PROCEDURE B96002A IS
BEGIN

     -----------------------------------------------

DECLARE   -- (A)

     TYPE REAL IS DIGITS 3;
     X : INTEGER := 5;
     Y : TIME := CLOCK;
     Z : CONSTANT REAL := 3.0;

BEGIN     -- (A)

     DELAY X;                      -- ERROR: INTEGER.
     NULL;

     DELAY Y;                      -- ERROR: TIME.
     NULL;

     DELAY Z;                      -- ERROR: REAL.
     NULL;

END; -- (A)

     -----------------------------------------------

DECLARE   -- (B)

     TYPE REAL IS DIGITS 3;
     BEFORE : TIME := CLOCK;
     AFTER : TIME;
     INC1 : CONSTANT REAL := 2.0;

BEGIN     -- (B)

     AFTER := CLOCK + SYSTEM.TICK;
     AFTER := BEFORE + AFTER;      -- ERROR: ARGS OF TYPE TIME.
     NULL;

     AFTER := BEFORE + INC1;       -- ERROR: TIME + REAL.
     NULL;

END; -- (B)

     -----------------------------------------------

DECLARE   -- (C)

     TYPE REAL IS DIGITS 3;
     BEFORE : TIME := CLOCK;
     AFTER : TIME;
     INC1 : CONSTANT REAL := 3.0;

BEGIN     -- (C)

     BEFORE := INC1 - BEFORE;      -- ERROR: DURATION - TIME.
     NULL;

     AFTER := BEFORE - INC1;            -- ERROR: TIME - REAL.
     NULL;

END; -- (C)

     -----------------------------------------------

END B96002A;
