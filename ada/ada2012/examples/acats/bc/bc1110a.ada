-- BC1110A.ADA

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
-- CHECK THAT A DEFAULT EXPRESSION FOR A GENERIC FORMAL OBJECT
-- (OF MODE IN) MUST HAVE THE SAME BASE TYPE AS THE FORMAL OBJECT.

-- PWB  2/3/86

PROCEDURE BC1110A IS

     TYPE FLOAT_5 IS DIGITS 5;
     TYPE FIXED_01 IS DELTA 0.01 RANGE 1.0 .. 9.99;
     TYPE ENUM_COLOR IS ( RED, BLUE, YELLOW );
     TYPE ARRAY_OF_INT IS ARRAY ( POSITIVE RANGE <> ) OF INTEGER;
     TYPE REC_OF_BOOL IS
          RECORD
               WHETHER : BOOLEAN;
          END RECORD;
     TYPE EARLY_LETTER IS NEW CHARACTER RANGE 'A' .. 'M';

     INT_DEF   : INTEGER  := 5;
     FLOAT_DEF : FLOAT_5  := 1.0;
     FIXED_DEF : FIXED_01 := 1.0;
     ENUM_DEF  : ENUM_COLOR := RED;
     ARRAY_DEF : ARRAY_OF_INT(1..2) := (2,4);
     CHAR_DEF  : CHARACTER := 'X';

     GENERIC
          TYPE INT_TYPE IS RANGE <>;
          TYPE FLOAT_TYPE IS DIGITS <>;
          TYPE FIXED_TYPE IS DELTA <>;
          INT_FORMAL : IN INT_TYPE := INT_DEF;     -- ERROR: WRONG TYPE.
          FLOAT_FORMAL : FLOAT_TYPE := FLOAT_DEF;  -- ERROR: WRONG TYPE.
          FIXED_FORMAL : FIXED_TYPE := FIXED_DEF;  -- ERROR: WRONG TYPE.
     PACKAGE GEN_PACK IS
     END GEN_PACK;

     GENERIC
          TYPE ENUM_TYPE IS ( <> );
          ENUM_FORMAL : ENUM_TYPE := ENUM_DEF;     -- ERROR: WRONG TYPE.
          STRING_FORMAL : IN STRING := (2, 3, 4);  -- ERROR: WRONG TYPE.
          RECORD_FORMAL : REC_OF_BOOL := FALSE;    -- ERROR: WRONG TYPE.
     PROCEDURE GEN_PROC ( X : IN OUT INTEGER );

     GENERIC
          TYPE ARRAY_TYPE IS
               ARRAY ( POSITIVE RANGE <> )
               OF INTEGER;
          ARRAY_FORMAL : ARRAY_TYPE := ARRAY_DEF;  -- ERROR: WRONG TYPE.
          CHAR_FORMAL : IN EARLY_LETTER
                           := CHAR_DEF;            -- ERROR: WRONG TYPE.
          BOOL_FORMAL : BOOLEAN := 0;              -- ERROR: WRONG TYPE.
          ARRAY_FORMAL_2 : ARRAY_OF_INT
                         := (1 => 'A', 2 => 'B');  -- ERROR: WRONG TYPE.
     FUNCTION GEN_FUNC ( X : INTEGER ) RETURN INTEGER;

     PROCEDURE GEN_PROC ( X : IN OUT INTEGER ) IS
     BEGIN
          NULL;
     END GEN_PROC;

     FUNCTION GEN_FUNC ( X : INTEGER ) RETURN INTEGER IS
     BEGIN
          IF X = INTEGER'LAST THEN
               RETURN INTEGER'FIRST;
          ELSE
               RETURN INTEGER'LAST;
          END IF;
     END GEN_FUNC;

BEGIN     -- BC1110A
     NULL;
END BC1110A;
