-- B74203B.ADA

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
--     CHECK THAT NO BASIC OPERATIONS THAT DEPEND ON THE FULL
--     DECLARATION OF THE TYPE ARE AVAILABLE FOR LIMITED AND NON-LIMITED
--     PRIVATE TYPES.  INCLUDE TYPES WITH DISCRIMINANTS AND PRIVATE
--     TYPES WITH LIMITED COMPONENTS.  THIS TEST CHECKS A DERIVED
--     INTEGER TYPE.

-- HISTORY:
--     BCB 07/15/88  CREATED ORIGINAL TEST.
--     BCB 04/10/90  SPLIT ORIGINAL TEST INTO B74203B.ADA, B74203C.ADA,
--                   B74203D.ADA, AND B74203E.ADA.

PROCEDURE B74203B IS

     PACKAGE P IS
          TYPE PR IS PRIVATE;

          CONS : CONSTANT PR;
     PRIVATE
          TYPE PR IS NEW INTEGER;

          CONS : CONSTANT PR := 10;
     END P;

     USE P;

     X1, X2, X3 : PR := CONS;
     BOOL : BOOLEAN := FALSE;
     VAL : INTEGER := 0;
     ST : STRING(1..10) := "1234567890";

BEGIN

     X1 := 10;         -- ERROR: LITERALS NOT DEFINED.

     X3 := X1 + X2;    -- ERROR: ADDITION OPERATOR NOT DEFINED.

     X3 := X1 - X2;    -- ERROR: SUBTRACTION OPERATOR NOT DEFINED.

     X3 := X1 * X2;    -- ERROR: MULTIPLICATION OPERATOR NOT DEFINED.

     X3 := X1 / X2;    -- ERROR: DIVISION OPERATOR NOT DEFINED.

     X3 := X1 ** 2;    -- ERROR: EXPONENTIATION OPERATOR NOT DEFINED.

     BOOL := X1 < X2;  -- ERROR: LESS THAN OPERATOR NOT DEFINED.

     BOOL := X1 > X2;  -- ERROR: GREATER THAN OPERATOR NOT DEFINED.

     BOOL := X1 <= X2; -- ERROR: LESS THAN OR EQUAL TO OPERATOR
                       --        NOT DEFINED.

     BOOL := X1 >= X2; -- ERROR: GREATER THAN OR EQUAL TO OPERATOR
                       --        NOT DEFINED.

     X3 := X1 MOD X2;  -- ERROR: MOD OPERATOR NOT DEFINED.

     X3 := X1 REM X2;  -- ERROR: REM OPERATOR NOT DEFINED.

     X3 := ABS(X1);    -- ERROR: ABS OPERATOR NOT DEFINED.

     X3 := P.PR'BASE'FIRST; -- ERROR: 'BASE ATTRIBUTE NOT DEFINED.

     X3 := P.PR'FIRST; -- ERROR: 'FIRST ATTRIBUTE NOT DEFINED.

     X3 := P.PR'LAST;  -- ERROR: 'LAST ATTRIBUTE NOT DEFINED.

     VAL := P.PR'WIDTH; -- ERROR: 'WIDTH ATTRIBUTE NOT DEFINED.

     VAL := P.PR'POS(X3); -- ERROR: 'POS ATTRIBUTE NOT DEFINED.

     X3 := P.PR'VAL(VAL); -- ERROR: 'VAL ATTRIBUTE NOT DEFINED.

     X3 := P.PR'SUCC(X2); -- ERROR: 'SUCC ATTRIBUTE NOT DEFINED.

     X3 := P.PR'PRED(X2); -- ERROR: 'PRED ATTRIBUTE NOT DEFINED.

     ST := P.PR'IMAGE(X3); -- ERROR: 'IMAGE ATTRIBUTE NOT DEFINED.

     X3 := P.PR'VALUE(ST); -- ERROR: 'VALUE ATTRIBUTE NOT DEFINED.

END B74203B;
