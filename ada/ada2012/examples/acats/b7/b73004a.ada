-- B73004A.ADA

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
-- OBJECTIVE:
--     CHECK THAT ENTITIES DECLARED IN A PACKAGE BODY AREN'T ACCESSIBLE
--     FROM OUTSIDE THE PACKAGE BODY.

-- HISTORY:
--     DHH 03/11/88 CREATED ORIGINAL TEST.

PROCEDURE B73004A IS

     G : INTEGER;
     B : BOOLEAN;

     PACKAGE PACK IS

          SUBTYPE I IS INTEGER RANGE 1 .. 10;
          TYPE F IS DIGITS 5;
          SUBTYPE C IS CHARACTER RANGE 'A' .. 'C';
          TYPE REC IS
               RECORD
                    INTR : I;
               END RECORD;
     END PACK;

     PACKAGE BODY PACK IS
           INT : I;
           FLT : F;
           CHR : C;
           RCD : REC;
           CON : CONSTANT I := 5;

           PACKAGE PACK1 IS
                K : F;
           END PACK1;

           FUNCTION FAIL_IT(X : INTEGER) RETURN BOOLEAN IS
           BEGIN
                IF X = X THEN
                     RETURN FALSE;
                ELSE
                     RETURN TRUE;
                END IF;
           END FAIL_IT;

          PROCEDURE PROC IS
               B : BOOLEAN;
          BEGIN
               B := FALSE;
          END PROC;
     BEGIN
          INT := RCD.INTR;
     END PACK;

     PROCEDURE REFERENCE IS
          B : BOOLEAN;
          G : INTEGER;
     BEGIN
          PACK.INT := 1;                         -- ERROR:
          PACK.FLT := 1.0;                       -- ERROR:
          PACK.CHR := 'C';                       -- ERROR:
          PACK.RCD.INTR := 1;                    -- ERROR:
          B := PACK.FAIL_IT;                     -- ERROR:
          PACK.PROC;                             -- ERROR:
          G := PACK.CON;                         -- ERROR:
     END REFERENCE;

USE PACK;

USE PACK.PACK1;                                  -- ERROR:

BEGIN
     PACK.INT := 1;                              -- ERROR:
     PACK.FLT := 1.0;                            -- ERROR:
     PACK.CHR := 'C';                            -- ERROR:
     PACK.RCD.INTR := 1;                         -- ERROR:
     B := PACK.FAIL_IT;                          -- ERROR:
     PACK.PROC;                                  -- ERROR:
     G := PACK.CON;                              -- ERROR:

     INT := 1;                                   -- ERROR:
     FLT := 1.0;                                 -- ERROR:
     CHR := 'C';                                 -- ERROR:
     RCD.INTR := 1;                              -- ERROR:
     B := FAIL_IT;                               -- ERROR:
     PROC;                                       -- ERROR:
     G := CON;                                   -- ERROR:
END B73004A;
