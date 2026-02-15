-- B55B09C.DEP

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
--     CHECK THAT UNIVERSAL_INTEGER LOOPS RESULT IN A LOOP INDEX
--     OF TYPE INTEGER.

--     PART 2: CONTEXT SUGGESTS  "LONG_INTEGER" .

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     THE TYPE "LONG_INTEGER".
--
--     IF "LONG_INTEGER" IS NOT SUPPORTED, THEN THE SUBTYPE
--     DECLARATION OF T MUST BE REJECTED.

-- HISTORY:
--     RM  07/09/82  CREATED ORIGINAL TEST.
--     JBG 02/13/84
--     BCB 12/28/87  MODIFIED HEADER.

PROCEDURE B55B09C IS

     SUBTYPE  T  IS  LONG_INTEGER;                 -- N/A => ERROR.

     A  :  ARRAY ( INTEGER RANGE 0 .. 2 ) OF  T;
     B  :  ARRAY ( T       RANGE 0 .. 2 ) OF  INTEGER;

     J  :  T  := 0;

     C1 :  CONSTANT := 1 + 1 - 1;
     C2 :  CONSTANT := C1 + 1;

     PROCEDURE  P ( I : T ) IS
     BEGIN
          NULL;
     END  P;

BEGIN

     FOR  K  IN 1 .. 2  LOOP

          J        :=  K;           -- ERROR: WRONG RHS TYPE.
          A ( K )  :=  K;           -- ERROR: WRONG RHS TYPE.
          B ( K )  :=  K;           -- ERROR: WRONG INDEX TYPE.
          P ( K )       ;           -- ERROR: WRONG ARGUMENT TYPE.
          A ( 0 MOD 3 )  :=  K;     -- ERROR: WRONG RHS TYPE.
          B ( K REM 3 )  :=  7;     -- ERROR: WRONG "REM"-ARGUMENT TYPE.

     END LOOP;


     FOR  K  IN C1 .. C2  LOOP

          DECLARE

               PROCEDURE  Q ( L : T ) IS
               BEGIN

                    J        :=  K;       -- ERROR: WRONG RHS TYPE.
                    A ( K )  :=  K;       -- ERROR: WRONG RHS TYPE.
                    B ( K )  :=  K;       -- ERROR: WRONG INDEX TYPE.
                    P ( K )       ;       -- ERROR: WRONG ARGUMENT TYPE.
                    A ( K MOD 3 )  :=  K; -- ERROR: WRONG RHS TYPE.
                    B ( K REM 3 )  :=  K; -- ERROR: WRONG "REM"-ARGUMENT
                                          --     TYPE.

               END  Q;

          BEGIN
               NULL;
          END;

     END LOOP;


END  B55B09C;
