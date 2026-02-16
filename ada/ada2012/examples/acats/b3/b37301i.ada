-- B37301I.ADA

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
-- CHECK THAT FOR A VARIANT PART OF A RECORD
-- THE OTHERS CHOICE MUST BE THE ONLY CHOICE GIVEN IN THE LAST
-- ALTERNATIVE.
 
-- ASL 7/1/81
 
PROCEDURE B37301I IS
 
     TYPE DAY IS (SUN,MON,TUE,WED,THU,FRI,SAT);
 
     TYPE VREC1(DISC : DAY) IS
          RECORD
               COMP : INTEGER;
               CASE DISC IS
                    WHEN OTHERS =>            -- ERROR: OTHERS NOT LAST.
                         WRONG : INTEGER;
                    WHEN MON =>
                         VAR1 : INTEGER;
               END CASE;
          END RECORD;
 
     TYPE VREC2(DISC : DAY) IS
          RECORD
               COMP : INTEGER;
               CASE DISC IS 
                    WHEN MON =>
                         VAR1 : INTEGER;
                    WHEN OTHERS =>            -- ERROR: OTHERS NOT LAST.
                         WRONG : INTEGER;
                    WHEN TUE =>
                         VAR2 : INTEGER;
               END CASE;  
          END RECORD;
 
     TYPE VREC3(DISC : DAY) IS
          RECORD
               COMP : INTEGER;
               CASE DISC IS
                    WHEN MON =>
                         VAR1 : INTEGER;
                    WHEN OTHERS | TUE =>      -- ERROR: OTHERS NOT 
                                              --   ALONE.
                         WRONG : INTEGER;
               END CASE;
          END RECORD;
 
     TYPE VREC4(DISC : DAY) IS
          RECORD
               COMP : INTEGER;
               CASE DISC IS
                    WHEN MON => 
                         VAR1 : INTEGER;
                    WHEN TUE | OTHERS | WED => -- ERROR: OTHERS NOT 
                                               --   ALONE.
                         WRONG : INTEGER;
               END CASE;
          END RECORD;
 
     TYPE VREC5(DISC : DAY) IS
          RECORD
               COMP : INTEGER;
               CASE DISC IS
                    WHEN MON =>
                         VAR1 : INTEGER;
                    WHEN TUE | OTHERS =>      -- ERROR: OTHERS NOT 
                                              --   ALONE.
                         WRONG : INTEGER;
               END CASE;
          END RECORD;
 
BEGIN
     NULL;
END B37301I;
