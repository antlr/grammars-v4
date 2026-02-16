-- B52002C.ADA

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
-- CHECK THAT ENTRIES, TASKS, AND AGGREGATES WITH TASK COMPONENTS CANNOT
--   BE ASSIGNED TO.

-- JRK 9/17/81
-- SPS 3/21/83

PROCEDURE B52002C IS

     TASK TYPE TT IS
          ENTRY E;
     END TT;

     TYPE A IS ARRAY (1..3) OF TT;

     TYPE R IS
          RECORD
               I : INTEGER := 0;
               T : TT;
          END RECORD;

     TYPE AR IS ARRAY (1..3) OF R;

     TYPE DT IS NEW TT;

     TYPE DAR IS NEW AR;

     T1, T2 : TT;
     A1, A2 : A;
     R1, R2 : R;
     AR1, AR2 : AR;
     DT1, DT2 : DT;
     DAR1, DAR2 : DAR;

     TASK BODY TT IS
     BEGIN
          NULL;
     END TT;

BEGIN

     T1.E := T2.E;       -- ERROR: NO := FOR ENTRIES.

     T1 := T2;           -- ERROR: NO := FOR TASKS.

     A1 := A2;           -- ERROR: NO := FOR TASK AGGREGATES.

     R1 := R2;           -- ERROR: NO := FOR TASK AGGREGATES.

     AR1 := AR2;         -- ERROR: NO := FOR TASK AGGREGATES.

     DT1.E := DT2.E;     -- ERROR: NO := FOR DERIVED ENTRIES.

     DT1 := DT2;         -- ERROR: NO := FOR DERIVED TASKS.

     DAR1 := DAR2;       -- ERROR: NO := FOR DERIVED TASK AGGREGATES.

END B52002C;
