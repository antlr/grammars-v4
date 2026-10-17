-- B55A01A.ADA

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
-- CHECK BASIC LOOP SYNTAX.

-- CHECK THAT A LOOP CAN HAVE A LOOP IDENTIFIER, AND IF PERSENT, THE
-- SAME IDENTIFIER MUST BE PRESENT AT THE END OF THE LOOP.  IF NOT
-- PRESENT, NO IDENTIFIER IS PERMITTED AT THE END OF THE LOOP.

-- CHECK THAT THE IDENTIFIER MUST BE A SIMPLE NAME.

-- CHECK THAT THE IDENTIFIER CANNOT BE THE NAME OF THE LOOP PARAMETER.

-- CHECK THAT THE LOOP PARAMETER CAN ONLY BE A SIMPLE NAME.

-- DAT 1/21/81
-- SPS 3/1/83
-- JRK 3/26/84

PROCEDURE B55A01A IS

BEGIN

L1:  LOOP                -- OK.
          EXIT;
     END LOOP L1;        -- OK.

L2:  LOOP
          EXIT;
     END LOOP L1;        -- ERROR: WRONG LOOP ID.

L3:
L4:  LOOP                -- ERROR: ONLY ONE LOOP ID ALLOWED.
          EXIT;
     END LOOP L3;

L5:  LOOP
          EXIT;
     END LOOP LL5;       -- ERROR: WRONG LOOP ID.

L6:  LOOP
          EXIT;
     END LOOP;           -- ERROR: MUST REPEAT LOOP ID.

     LOOP
          EXIT;
     END LOOP L7;        -- ERROR: NO LOOP ID ON BEGINNING.

B1:  BEGIN
     L8:  LOOP
               EXIT;
          END LOOP B1.L8;     -- ERROR: QUALIFIED LOOP ID ILLEGAL.
     END B1;

<<L10>>
     LOOP
          EXIT;
     END LOOP L10;       -- ERROR: CANNOT USE LABEL AS LOOP ID.

L11: LOOP
     L11A:
          LOOP
               EXIT;
          END LOOP L11;  -- ERROR: INCORRECT LOOP ID.
     END LOOP L11;       -- OK.

     FOR L12 IN 1 .. 10 LOOP
          EXIT;
     END LOOP L12;       -- ERROR: LOOP PARAMETER USED AS LOOP ID.

B2:  DECLARE

          A: ARRAY (1..2) OF INTEGER;

          TYPE RT IS
               RECORD
                    E: INTEGER;
               END RECORD;

          TYPE AI IS ACCESS INTEGER;

          R : RT;
          IP : AI := NEW INTEGER;
          I : INTEGER;

     BEGIN

          FOR A(1) IN 1 .. 10 LOOP      -- ERROR: SUBSCRIPTED LOOP
                                        -- PARAMETER.
               NULL;
          END LOOP;

          FOR R.E IN 1 .. 10 LOOP       -- ERROR: SELECTED LOOP
                                        -- PARAMETER.
               NULL;
          END LOOP;

          FOR B2.I IN 1 .. 10 LOOP      -- ERROR: SELECTED LOOP
                                        -- PARAMETER.
               NULL;
          END LOOP;

          FOR IP.ALL IN 1 .. 10 LOOP    -- ERROR: SELECTED LOOP
                                        -- PARAMETER.
               NULL;
          END LOOP;

          FOR IP IN 1 .. 10 LOOP        -- OK.
               NULL;
          END LOOP;

     L9:  FOR L9.I IN 1 .. 10 LOOP      -- ERROR: SELECTED LOOP
                                        -- PARAMETER.
               NULL;
          END LOOP L9;

     END B2;

END B55A01A;
