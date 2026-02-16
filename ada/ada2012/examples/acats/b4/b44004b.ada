-- B44004B.ADA

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
-- CHECK THAT "OBJECT IN TYPE_MARK DIGITS N" IS ILLEGAL.

-- PWB  03/04/86
-- KAS  11/24/95 DELETE DIGITS CONSTRAINT FROM SUBTYPE

PROCEDURE B44004B IS

     TYPE FLOATING IS DIGITS 5;
     SUBTYPE COARSE IS FLOATING;
     SUBTYPE SHORT IS FLOATING RANGE 0.0 .. 1.0;
     NUM  : FLOATING := 1.0;
     BOOL : BOOLEAN := (NUM IN FLOATING DIGITS 4);    -- ERROR: DIGITS.


BEGIN

     IF NUM NOT IN FLOATING DIGITS 3 THEN             -- ERROR: DIGITS.
          NULL;
     ELSIF NUM IN COARSE DIGITS 3 THEN                -- ERROR: DIGITS.
          NULL;
     ELSIF NUM NOT IN COARSE DIGITS 3 THEN            -- ERROR: DIGITS.
          NULL;
     ELSE
          BOOL := (NUM IN SHORT DIGITS 4);            -- ERROR: DIGITS.
          BOOL := (NUM NOT IN SHORT DIGITS 4);        -- ERROR: DIGITS.
     END IF;

END B44004B;
