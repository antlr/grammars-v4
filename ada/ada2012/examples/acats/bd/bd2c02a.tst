-- BD2C02A.TST

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
--     CHECK THAT TWO TASK STORAGE SIZE SPECIFICATIONS, EVEN IF THE
--     SAME, CANNOT BE GIVEN FOR THE SAME TYPE.

-- MACRO SUBSTITUTION:
--     $TASK_STORAGE_SIZE IS THE NUMBER OF STORAGE_UNITS REQUIRED FOR
--     THE ACTIVATION OF A TASK.

-- HISTORY:
--     JKC 04/06/88  CREATED ORIGINAL TEST.
--     BCB 04/14/89  CHANGED EXTENSION TO '.TST'.  ADDED A MACRO TO
--                   TASK STORAGE_SIZE CLAUSES.

PROCEDURE BD2C02A IS

     TASK TYPE T;
     FOR T'STORAGE_SIZE USE $TASK_STORAGE_SIZE;
     FOR T'STORAGE_SIZE USE $TASK_STORAGE_SIZE;    -- ERROR: DUPLICATE.

     TASK TYPE T2;
     FOR T2'STORAGE_SIZE USE $TASK_STORAGE_SIZE;
     FOR T2'STORAGE_SIZE
          USE $TASK_STORAGE_SIZE * 2;   -- ERROR: DUPLICATE.

     TASK BODY T IS
     BEGIN
          NULL;
     END T;

     TASK BODY T2 IS
     BEGIN
          NULL;
     END T2;

BEGIN
     NULL;
END BD2C02A;
