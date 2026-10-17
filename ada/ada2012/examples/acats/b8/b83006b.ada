-- B83006B.ADA

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
--     CHECK THAT THE DISCRIMINANTS IN A GENERIC FORMAL TYPE DECLARATION
--     CANNOT HAVE THE SAME IDENTIFIER.

-- HISTORY:
--     TBN 07/28/88  CREATED ORIGINAL TEST.

PROCEDURE B83006B IS

     TYPE ENUM IS (ONE, TWO, THREE);

     TYPE SHORT IS NEW INTEGER RANGE 1..10;

     GENERIC
          TYPE REC1 (X : INTEGER) IS PRIVATE;
          TYPE REC2 (A : INTEGER; A : INTEGER) IS PRIVATE;     -- ERROR:
          TYPE REC3 (B : INTEGER; B : BOOLEAN) IS PRIVATE;     -- ERROR:
          TYPE REC4 (C : INTEGER; C : CHARACTER) IS PRIVATE;   -- ERROR:
          TYPE REC5 (D : INTEGER; D : ENUM) IS PRIVATE;        -- ERROR:
          TYPE REC7 (F : INTEGER; F : SHORT) IS PRIVATE;       -- ERROR:
     PACKAGE PACK1 IS
          A : REC1(3);
     END PACK1;

     GENERIC
          TYPE REC1 (X : CHARACTER) IS PRIVATE;
          TYPE REC2 (A : CHARACTER; A : CHARACTER) IS PRIVATE; -- ERROR:
          TYPE REC3 (B : BOOLEAN; B : BOOLEAN) IS PRIVATE;     -- ERROR:
          TYPE REC4 (C : ENUM; A : INTEGER; C : CHARACTER) IS  -- ERROR:
                PRIVATE;
          TYPE REC5 (D : ENUM; D : ENUM) IS PRIVATE;           -- ERROR:
          TYPE REC7 (F : SHORT; F : SHORT) IS PRIVATE;         -- ERROR:
     PROCEDURE PROC1;

     GENERIC
          TYPE REC1 (X : BOOLEAN) IS LIMITED PRIVATE;
          TYPE REC2 (A, B : INTEGER; A : CHARACTER) IS         -- ERROR:
               LIMITED PRIVATE;
          TYPE REC3 (B : BOOLEAN; A : INTEGER; B : ENUM) IS    -- ERROR:
               LIMITED PRIVATE;
          TYPE REC5 (D : ENUM; C : INTEGER; D : CHARACTER) IS  -- ERROR:
               PRIVATE;
          TYPE REC6 (E : BOOLEAN; D : INTEGER; E : SHORT) IS   -- ERROR:
               PRIVATE;
          TYPE REC7 (F : SHORT; E : CHARACTER; F : SHORT) IS   -- ERROR:
               LIMITED PRIVATE;
     FUNCTION FUNC1 RETURN INTEGER;


     PROCEDURE PROC1 IS
     BEGIN
          NULL;
     END PROC1;

     FUNCTION FUNC1 RETURN INTEGER IS
     BEGIN
          RETURN 1;
     END FUNC1;

BEGIN
     NULL;
END B83006B;
