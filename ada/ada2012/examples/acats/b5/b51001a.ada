-- B51001A.ADA

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
-- CHECK THAT DECLARATIONS CANNOT BE INTERLEAVED WITH STATEMENTS
--    IN A SEQUENCE OF STATEMENTS.

-- DCB 2/1/80
-- SPS 3/15/83

PROCEDURE B51001A IS

     I1, I2, I3, I4 : INTEGER;

     PACKAGE P IS
          C1, C2 : CHARACTER;
     END P;

     PACKAGE BODY P IS
     BEGIN
          C1 := 'A';
          C9 : INTEGER; -- ERROR: DECLARATION INTERLEAVED WITH
                        --        STATEMENT IN PACKAGE BODY.
          C2 := 'B';
     END P;

BEGIN

     I1 := 3;
     I9 : INTEGER;      -- ERROR: DECLARATION INTERLEAVED WITH
                        --        STATEMENT IN PROCEDURE BODY.
     I2 := 4;

     IF I2 = I3 THEN
          I4 := I2;
          I8 : INTEGER; -- ERROR: DECLARATION INTERLEAVED WITH
                        --        STATEMENT IN IF BODY.
          I2 := 3;
     END IF;

     CASE I1 IS
          WHEN 1..3 =>
               I2 := 7;
               I9 : INTEGER; -- ERROR: DECL INTERLEAVED WITH STMT
                             --        IN CASE BODY.
               I1 := 5;
          WHEN OTHERS => NULL;
     END CASE;

     WHILE I1 < 10 LOOP
          I1  := I1 + 1;
          I20 :  INTEGER; -- ERROR: DECL INTERLEAVED WITH STMT
                          --        IN LOOP BODY.
          I2  := I1;
     END LOOP;

     DECLARE
          I5 : INTEGER;

     BEGIN
          I5 := I1 + I2;
          I99 : INTEGER;     -- ERROR: DECL INTERLEAVED WITH STMT
                             --        IN INNER BLOCK.
          I3 := 3;
     END;

END B51001A;
