-- B92001A.ADA

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
-- CHECK THAT NEITHER ASSIGNMENT NOR THE PREDEFINED COMPARISONS
-- = AND /= ARE AVAILABLE FOR TASK OBJECTS.

-- WEI 3/ 4/82
-- RJK 2/ 1/84     ADDED TO ACVC.
-- TBN 7/14/86     RENAMED FROM B920BDA-B.ADA.

PROCEDURE B92001A IS

     TASK T1;
     TASK T2;
     TASK TYPE TT1;
     TASK TYPE TT2;


     TYPE ATT1 IS ACCESS TT1;
     TYPE ATT2 IS ACCESS TT2;
     TYPE AI IS ACCESS INTEGER;

     POINTER_AI : AI := NEW INTEGER;

     OBJ_TT1_1, OBJ_TT1_2 : TT1;
     OBJ_TT2_1 : TT2;
     POINTER_TT1_1 : ATT1 := NEW TT1;
     POINTER_TT1_2 : ATT1 := NEW TT1;
     POINTER_TT2_1 : ATT2 := NEW TT2;

     TASK BODY T1 IS
     BEGIN
          NULL;
     END T1;

     TASK BODY T2 IS
     BEGIN
          NULL;
     END T2;

     TASK BODY TT1 IS
     BEGIN
          NULL;
     END TT1;

     TASK BODY TT2 IS
     BEGIN
          NULL;
     END TT2;

BEGIN

     POINTER_AI.ALL := 13; -- OK. CHECK IF 'ALL' WORKS.

     T1 := T2;                                         -- ERROR: :=.
     OBJ_TT1_1 := OBJ_TT1_2;                           -- ERROR: :=.
     OBJ_TT2_1 := OBJ_TT1_1;                           -- ERROR: :=.

     IF OBJ_TT1_1 = OBJ_TT1_1 THEN                     -- ERROR: =.
          NULL; 
     END IF;

     IF OBJ_TT1_1 /= OBJ_TT1_1 THEN                    -- ERROR: /=.
          NULL; 
     END IF;

     IF OBJ_TT2_1 = OBJ_TT1_1 THEN                     -- ERROR: =.
          NULL; 
     END IF;

     POINTER_TT1_1.ALL := POINTER_TT1_2.ALL;           -- ERROR: :=.
     POINTER_TT2_1.ALL := POINTER_TT1_1.ALL;           -- ERROR: :=.
     IF POINTER_TT1_1.ALL = POINTER_TT1_1.ALL THEN     -- ERROR: =.
          NULL;
     END IF;

     IF POINTER_TT1_1.ALL/= POINTER_TT1_1.ALL THEN     -- ERROR: /=.
          NULL;
     END IF;

     IF POINTER_TT2_1.ALL = POINTER_TT1_1.ALL THEN     -- ERROR: =.
          NULL;
     END IF;

END B92001A;
