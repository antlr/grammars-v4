-- BD2A03B.ADA

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
--     CHECK THAT A SIZE SPECIFICATION CANNOT BE GIVEN IN A PACKAGE
--     SPECIFICATION FOR ENUMERATION, TASK, RECORD AND FLOATING POINT
--     TYPES, DECLARED IN AN ENCLOSING PACKAGE SPECIFICATION OR
--     DECLARATIVE PART.

-- HISTORY:
--     DHH 08/22/88 CREATED ORIGINAL TEST.

PROCEDURE BD2A03B IS

     PACKAGE P IS

          TYPE ENUM IS (RED, YELLOW, BLUE);

          TASK TYPE TASK_TYPE IS
               ENTRY E;
          END TASK_TYPE;

          TYPE RECORD_TYPE IS
               RECORD
                    UNIT1 : INTEGER := 1;
                    UNIT2 : INTEGER := 2;
                    UNIT3 : INTEGER := 3;
               END RECORD;

          TYPE FLOAT_TYPE IS DIGITS 5;

          PACKAGE INNER_P IS

               FOR ENUM'SIZE USE INTEGER'SIZE;                -- ERROR:
               FOR TASK_TYPE'SIZE USE 8 * INTEGER'SIZE;       -- ERROR:
               FOR RECORD_TYPE'SIZE  USE 6 * INTEGER'SIZE;    -- ERROR:
               FOR FLOAT_TYPE'SIZE USE FLOAT'SIZE;            -- ERROR:

          END INNER_P;

     END P;

     PACKAGE BODY P IS

          TASK BODY TASK_TYPE IS
          BEGIN
               ACCEPT E;
          END TASK_TYPE;

     END P;

BEGIN
     DECLARE  -- BLOCK STATEMENT.

          TYPE ENUM IS (RED, YELLOW, BLUE);

          TASK TYPE TASK_TYPE IS
               ENTRY E;
          END TASK_TYPE;

          TYPE RECORD_TYPE IS
               RECORD
                    UNIT1 : INTEGER := 1;
                    UNIT2 : INTEGER := 2;
                    UNIT3 : INTEGER := 3;
               END RECORD;

          TYPE FLOAT_TYPE IS DIGITS 5;

          PACKAGE INNER_P IS

               FOR ENUM'SIZE USE INTEGER'SIZE;                -- ERROR:
               FOR TASK_TYPE'SIZE USE 8 * INTEGER'SIZE;       -- ERROR:
               FOR RECORD_TYPE'SIZE  USE 6 * INTEGER'SIZE;    -- ERROR:
               FOR FLOAT_TYPE'SIZE USE FLOAT'SIZE;            -- ERROR:
          END INNER_P;

          TASK BODY TASK_TYPE IS
          BEGIN
               ACCEPT E;
          END TASK_TYPE;

     BEGIN
          NULL;
     END;       -- BLOCK STATEMENT.
END BD2A03B;
