-- B95031A.ADA

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
-- CHECK THAT THE TASK DESIGNATED BY AN OBJECT OF A TASK TYPE
-- EXHIBITS EXACTLY THE ENTRIES DECLARED IN THE SPECIFICATION
-- OF THE TASK TYPE.

-- WEI  3/ 4/82
-- RJK  2/ 1/84     ADDED TO ACVC
-- JWC 6/28/85   RENAMED FROM B950ADA-B.ADA

PROCEDURE B95031A IS

     TYPE I2 IS RANGE 1..2;
     TYPE NI3 IS NEW INTEGER RANGE 0..2;

     TASK TYPE TT1 IS
          ENTRY E1 (P1 : NI3);
          ENTRY E2 (I2) (P2 : NI3; P3 : I2);
     END TT1;

     TASK TYPE TT2 IS
          ENTRY E1 (NI3);
          ENTRY E2 (P1 : I2; P2 : NI3; P3 : I2);
     END TT2;

     TASK TYPE TT3;

     OBJ_TT1_1, OBJ_TT1_2 : TT1;
     OBJ_TT2_1, OBJ_TT2_2 : TT2;
     OBJ_TT3 : TT3;

     TASK BODY TT1 IS
     BEGIN
          ACCEPT E1 (P1 : NI3);
          ACCEPT E2 (1) (P2 : NI3; P3 : I2);
     END TT1;

     TASK BODY TT2 IS
     BEGIN
          ACCEPT E1 (0);
          ACCEPT E2 (P1 : I2; P2 : NI3; P3 : I2);
     END TT2;

     TASK BODY TT3 IS
     BEGIN
          NULL;
     END TT3;

BEGIN

     OBJ_TT3.E1;                   -- ERROR: NO ENTRY IN TASK TT3.
     OBJ_TT1_1.E3;                 -- ERROR: ENTRY E3 NON EXISTENT.
     OBJ_TT1_1.E1 (1) (0);         -- ERROR: ENTRY E1 IS NO FAMILY.
     OBJ_TT1_1.E2 (0, 1);          -- ERROR: ENTRY E2 IS A FAMILY.
     OBJ_TT1_2.E1 (0, 1);          -- ERROR: WRONG PARAMETER LIST.
     OBJ_TT1_2.E2 (1) (0);         -- ERROR: WRONG PARAMETER LIST.

     OBJ_TT2_1.E1 (0) (1, 0, 1);   -- ERROR: ENTRY E1 HAS NO PARAMETER
     OBJ_TT2_1.E2 (0);             -- ERROR: WRONG PARAMETER LIST.
     OBJ_TT2_2.E1 (3);             -- OK.
     OBJ_TT2_2.E2 (P1 => 0, P2 => 0, P2 => 1); -- ERROR:
                                   -- DOUBLE PARAMETER IN PAR.LIST.

END B95031A;
