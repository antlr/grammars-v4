-- B37302A.ADA

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
-- CHECK THAT THE TYPE OF THE DISCRIMINANT AND EACH CHOICE MUST BE THE 
-- SAME, AND THAT EVERY PAIR OF CHOICES MUST COVER A DISJOINT SET OF
-- VALUES.
 
-- ASL 7/9/81
-- SPS 12/7/82 

PROCEDURE B37302A IS
 
     SUBTYPE INT IS INTEGER RANGE 1..10;
     TYPE NEW_INT IS NEW INTEGER;
 
     TYPE REC1 ( DISC :  INT ) IS
         RECORD
               COMP : INTEGER;
               CASE DISC IS
                    WHEN NEW_INT'(5) => NULL; -- ERROR: WRONG TYPE.
                    WHEN 5 => NULL;           -- OK.
                    WHEN TRUE => NULL;        -- ERROR: WRONG TYPE.
                    WHEN OTHERS => NULL;
               END CASE;
          END RECORD;
 
     TYPE REC2(DISC : INTEGER) IS
          RECORD
               COMP : INTEGER;
               CASE DISC IS
                    WHEN 1 => NULL;           -- OK.
                    WHEN 1..10 => NULL;       -- ERROR: OVERLAP.
                    WHEN 11 => NULL;          -- OK.
                    WHEN 12 | 12 => NULL;     -- ERROR: OVERLAP.
                    WHEN 13..1000 => NULL;    -- OK.
                    WHEN 999..1000 => NULL;   -- ERROR: OVERLAP.
                    WHEN OTHERS => NULL;      
               END CASE;
          END RECORD;
 
     TYPE REC3(DISC : INTEGER) IS
          RECORD
               COMP : INTEGER;
               CASE DISC IS
                    WHEN 1..5 | 5..10 =>      -- ERROR: OVERLAP.
                        NULL;
                    WHEN 12 => NULL;          -- OK.
                    WHEN 101..105 | 101..110 => -- ERROR: OVERLAP.
                         NULL;
                    WHEN 201..210 => NULL;    -- OK.
                    WHEN 205..210 => NULL;    -- ERROR: OVERLAP.
                    WHEN 301..310 => NULL;    -- OK.
                    WHEN 304..306 => NULL;    -- ERROR: OVERLAP.
                    WHEN 501..505 => NULL;    -- OK.
                    WHEN 502..506 => NULL;    -- ERROR: OVERLAP.
                    WHEN 401..410 => NULL;    -- OK.
                    WHEN 401..410 => NULL;    -- ERROR: OVERLAP.
                    WHEN OTHERS => NULL;
               END CASE;
          END RECORD;
 
BEGIN
     NULL;
END B37302A;
