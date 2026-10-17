-- B38009D.ADA
 
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
--     CHECK THAT AN INDEX OR DISCRIMINANT CONSTRAINT CANNOT BE IMPOSED
--     ON A GENERIC FORMAL ACCESS TYPE WHOSE DESIGNATED TYPE IS AN
--     ACCESS TYPE.

-- TYPES OF ERROR MESSAGES:
--     A) CONSTRAINT IMPOSED IN PARAMETER DECLARATION.
--     B) CONSTRAINT IMPOSED IN RETURN TYPE IN FUNCTION DECLARATION.

-- HISTORY:
--     THS  09/21/90  CREATED TEST FROM SPLIT OF B38009B.ADA.

PROCEDURE B38009D IS

     TYPE ARR IS ARRAY(INTEGER RANGE <>) OF INTEGER;
 
     TYPE REC(DISC : INTEGER) IS
          RECORD
               NULL;
          END RECORD;
 
     TYPE ACC1 IS ACCESS ARR;
     TYPE ACC2 IS ACCESS REC;

GENERIC
     TYPE A1 IS ACCESS ACC1;
     TYPE A2 IS ACCESS ACC2;
PROCEDURE P;
PROCEDURE P IS

     PROCEDURE PROC1 (PARM1 : IN OUT A1(1..5);     -- ERROR: A.
                      PARM2 : IN OUT A2(5)) IS     -- ERROR: A.
     BEGIN
          NULL;
     END PROC1;

     FUNCTION FUNC1 RETURN A1(1..5) IS             -- ERROR: B.
     BEGIN
          RETURN NULL;
     END FUNC1;

     FUNCTION FUNC2 RETURN A2(5) IS                -- ERROR: B.
     BEGIN
          RETURN NULL;
     END;

BEGIN
     NULL;
END P;

BEGIN
     NULL;
END B38009D;
