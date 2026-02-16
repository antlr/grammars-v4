-- B44004D.ADA

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
-- CHECK THAT "OBJECT IN TYPE_MARK (LO .. HI)" IS ILLEGAL.

-- PWB  03/04/86

PROCEDURE B44004D IS

     TYPE ARRAY_OVER_INT IS ARRAY (INTEGER RANGE <>) OF BOOLEAN;
     TYPE ARRAY_OVER_CHAR IS ARRAY (CHARACTER RANGE <>) OF INTEGER;
     CON_ARRAY_INT : ARRAY_OVER_INT(1..2) := (OTHERS => FALSE);
     CON_ARRAY_CH : ARRAY_OVER_CHAR('A'..'Z') := (OTHERS => 0);

     BOOL : BOOLEAN := (CON_ARRAY_INT IN 
                        ARRAY_OVER_INT(1..2));   -- ERROR: INDEX CONSTR.

BEGIN

     IF CON_ARRAY_INT NOT IN 
        ARRAY_OVER_INT(1..2) THEN                -- ERROR: INDEX CONSTR.
          BOOL := (CON_ARRAY_CH IN 
                   ARRAY_OVER_CHAR('A'..'Z'));   -- ERROR: INDEX CONSTR.
     ELSIF CON_ARRAY_CH NOT IN 
           ARRAY_OVER_CHAR('A'..'Z') THEN        -- ERROR: INDEX CONSTR.
          NULL;
     END IF;

END B44004D;
