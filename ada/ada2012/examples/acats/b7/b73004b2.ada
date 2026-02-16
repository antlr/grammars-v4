-- B73004B2.ADA

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
--     CHECK THAT ENTITIES DECLARED IN A LIBRARY PACKAGE BODY AREN'T
--     ACCESSIBLE FROM OUTSIDE THE PACKAGE BODY.

-- SEPARATE FILES ARE:
--     B73004B0M.ADA - PACKAGE B73004B_PACK WHICH SHOULD COMPILE.
--     B73004B1.ADA -- PACKAGE B73004B_PACK2 WHICH CONTAINS AN ERROR.
--     B73004B2.ADA -- PROCEDURE B73004B WHICH ATTEMPTS TO REFERENCE ENTITIES
--                     DECLARED IN THE PACKAGE BODIES.

-- HISTORY:
--     LDC   06/13/88 CREATED ORIGINAL TEST.
--     MCH   04/13/90 SPLIT COMPILATIONS INTO SEPARATE FILES TO AVOID
--                    CONFLICTS WITH AI-00255.

WITH B73004B_PACK;

PROCEDURE B73004B IS

     A : INTEGER;
     B : BOOLEAN;


     PROCEDURE REFERENCE IS
          C : BOOLEAN;
          D : B73004B_PACK.INT_TYPE;                 -- OK;
          E : B73004B_PACK.INT_BODY_TYPE;            -- ERROR:
     BEGIN
          B73004B_PACK.INT := 1;                     -- ERROR:
          B73004B_PACK.FLT := 1.0;                   -- ERROR:
          B73004B_PACK.CHR := 'C';                   -- ERROR:
          B73004B_PACK.RCD.ELEMT := 1;               -- ERROR:
          B73004B_PACK.BOL := TRUE;                  -- ERROR:
          C := B73004B_PACK.FAIL_IT;                 -- ERROR:
          B73004B_PACK.PROC;                         -- ERROR:
          D := B73004B_PACK.CON;                     -- ERROR:
     END REFERENCE;

USE B73004B_PACK;

BEGIN
     INT := 1;                                   -- ERROR:
     FLT := 1.0;                                 -- ERROR:
     CHR := 'C';                                 -- ERROR:
     REC.ELEMT := 1;                             -- ERROR:
     BOL := TRUE;                                -- ERROR:
     A := FAIL_IT;                               -- ERROR:
     PROC;                                       -- ERROR:
     B := CON;                                   -- ERROR:

END B73004B;
