-- B38009A.ADA
 
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
--     ON AN ACCESS TYPE WHOSE DESIGNATED TYPE IS AN ACCESS TYPE.

-- TYPES OF ERROR MESSAGES:
--     A) CONSTRAINT IMPOSED IN OBJECT DECLARATION.
--     B) CONSTRAINT IMPOSED IN ARRAY COMPONENT DECLARATION.
--     C) CONSTRAINT IMPOSED IN RECORD COMPONENT DECLARATION.
--     D) CONSTRAINT IMPOSED IN ACCESS TYPE DECLARATION.
--     E) CONSTRAINT IMPOSED IN DERIVED TYPE DEFINITION.

-- HISTORY:
--     AH   08/25/86 
--     THS  09/21/90  MODIFIED HEADER FORMAT AND SPLIT TEST TO
--                    B38009C.ADA.

PROCEDURE B38009A IS

     TYPE ARR IS ARRAY(INTEGER RANGE <>) OF INTEGER;
 
     TYPE REC(DISC : INTEGER) IS
          RECORD
               NULL;
          END RECORD;
 
     TYPE ACC1 IS ACCESS ARR;
     TYPE ACC2 IS ACCESS REC;

     SUBTYPE ST1 IS ACC1(1..5);
     SUBTYPE ST2 IS ACC2(5);

     TYPE A1 IS ACCESS ACC1;
     TYPE A2 IS ACCESS ACC2;

     OBJ1 : ACC1(1..5);                            -- OK.
     OBJ2 : ACC2(5);                               -- OK.

     OBJ3 : ST1;                                   -- OK.
     OBJ4 : ST2;                                   -- OK.

     OBJ5 : A1(1..5);                              -- ERROR: A.
     OBJ6 : A2(5);                                 -- ERROR: A.

     TYPE ARR2 IS ARRAY(1..10) OF A1(1..5);        -- ERROR: B.
     TYPE ARR3 IS ARRAY(1..10) OF A2(5);           -- ERROR: B.
  
     TYPE REC2 IS
          RECORD
               COMP1 : A1(1..5);                   -- ERROR: C.
               COMP2 : A2(5);                      -- ERROR: C.
          END RECORD;
 
     TYPE AT1 IS ACCESS A1(1..5);                  -- ERROR: D.
     TYPE AT2 IS ACCESS A2(5);                     -- ERROR: D.

     TYPE DER1 IS NEW A1(1..5);                    -- ERROR: E.
     TYPE DER2 IS NEW A2(5);                       -- ERROR: E.
 
BEGIN
     NULL;
END B38009A;
