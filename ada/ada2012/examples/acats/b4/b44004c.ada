-- B44004C.ADA

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
-- CHECK THAT "OBJECT IN TYPE_MARK DELTA D" IS ILLEGAL.

-- PWB  03/04/86

PROCEDURE B44004C IS

     TYPE FINE IS DELTA 0.01 RANGE -10.0 .. 10.0;
     SUBTYPE COARSE IS FINE DELTA 0.1;
     NUM  : FINE := 0.01;
     BOOL : BOOLEAN := (NUM IN COARSE DELTA 1.0);       -- ERROR: DELTA.

BEGIN

     IF NUM IN FINE DELTA 0.1 THEN                      -- ERROR: DELTA.
          NULL;
     ELSIF NUM NOT IN COARSE DELTA 0.2 THEN             -- ERROR: DELTA.
          NULL;
     ELSIF NUM NOT IN FINE DELTA 0.05 THEN              -- ERROR: DELTA.
          BOOL := NUM NOT IN FINE DELTA 0.05;           -- ERROR: DELTA.
     END IF;

END B44004C;
