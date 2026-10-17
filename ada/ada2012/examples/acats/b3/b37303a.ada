-- B37303A.ADA

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
-- CHECK THAT NON-STATIC CHOICE VALUES ARE FORBIDDEN IN VARIANT
-- RECORDS.
 
-- ASL 7/9/81
-- JWC 5/29/85
 
PROCEDURE B37303A IS
 
     N : INTEGER RANGE 1..1 := 1;
     I1 : INTEGER := 3;
     I2 : INTEGER := 9;
     SUBTYPE ST1 IS INTEGER RANGE I1..6;
     SUBTYPE ST2 IS INTEGER RANGE 7..I2;
     HIGH : INTEGER RANGE 20..20 := 20;
     LOW : INTEGER RANGE 10..10 := 10;
     X : INTEGER := 30;
     Y : INTEGER := 35;
 
     TYPE REC(DISC : INTEGER) IS
          RECORD
               COMP : INTEGER;
               CASE DISC IS
                    WHEN N => NULL;           -- ERROR: DYNAMIC CHOICE.
                    WHEN 15 => NULL;          -- OK.
                    WHEN ST1 => NULL;         -- ERROR: DYNAMIC CHOICE.
                    WHEN 23..22 => NULL;      -- OK.
                    WHEN ST2 RANGE 7..9 =>    -- ERROR: DYNAMIC CHOICE.
                         NULL;
                    WHEN 5000 => NULL;        -- OK.
                    WHEN HIGH..LOW => NULL;   -- ERROR: DYNAMIC CHOICE.
                    WHEN INTEGER RANGE 50..55 => -- OK.
                         NULL;
                    WHEN INTEGER RANGE X..Y => -- ERROR: DYNAMIC CHOICE.
                         NULL;
                    WHEN 100 => NULL;         -- OK.
                    WHEN OTHERS => NULL;
               END CASE;
          END RECORD;
BEGIN
     NULL;
END B37303A;
