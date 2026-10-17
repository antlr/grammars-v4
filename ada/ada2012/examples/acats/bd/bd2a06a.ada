-- BD2A06A.ADA

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
--     CHECK THAT THE EXPRESSION IN A SIZE SPECIFICATION FOR AN ARRAY,
--     RECORD, TASK, ENUMERATION, FLOATING-POINT, AND FIXED-POINT TYPES
--     MUST BE STATIC, INTEGER EXPRESSIONS.

-- HISTORY:
--     JKC 03/16/88  CREATED ORIGINAL TEST.

PROCEDURE BD2A06A IS

     TYPE BASIC_ENUM IS (A, B, C);
     FOR BASIC_ENUM'SIZE USE 4.0;                            -- ERROR:

     I : INTEGER RANGE 1..5 := 5;
     TYPE FIXED_TYPE IS DELTA 0.25 RANGE 1.0..4.0;
     FOR FIXED_TYPE'SIZE USE I;                              -- ERROR:

     F: CONSTANT := 1.0;
     TYPE FLOAT_TYPE IS DIGITS 2;
     FOR FLOAT_TYPE'SIZE USE F * INTEGER'SIZE;               -- ERROR:

     TYPE ARRAY_TYPE IS ARRAY (1..4) OF INTEGER;
     FOR ARRAY_TYPE'SIZE USE 4 * F;                          -- ERROR:

     TYPE INT IS NEW INTEGER;
     Z: INT := 96;
     TYPE RECORD_TYPE IS
          RECORD
               UNIT1: INTEGER := 1;
               UNIT2: INTEGER := 2;
               UNIT3: INTEGER := 3;
          END RECORD;
     FOR RECORD_TYPE'SIZE USE Z;                             -- ERROR:

     FUNCTION F_INT (INT: INTEGER) RETURN INTEGER IS
          F_OUT: INTEGER;
     BEGIN
          F_OUT := INT * 1;
          RETURN F_OUT;
     END F_INT;

BEGIN
     DECLARE
          TASK TYPE T IS
               ENTRY E;
          END T;
          FOR T'SIZE USE F_INT(4);                           -- ERROR:

          TASK BODY T IS
          BEGIN
               ACCEPT E;
          END T;

     BEGIN
          NULL;
     END;

END BD2A06A;
