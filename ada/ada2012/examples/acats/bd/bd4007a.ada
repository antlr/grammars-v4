-- BD4007A.ADA

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
--     CHECK THAT TWO COMPONENT CLAUSES, EVEN IF IDENTICAL, CANNOT BE
--     GIVEN FOR THE SAME COMPONENT.  ALSO, CHECK THE CASE IN WHICH
--     THE COMPONENT IS NONEXISTENT.

-- HISTORY:
--     BCB 04/11/88  CREATED ORIGINAL TEST.
--     DHH 03/28/89  ADDED CHECK FOR COMPONENT WHICH DOES NOT EXIST IN
--                   ANY VARIANT
--     THS 03/30/90  MOVED "-- ERROR:" TO THE RIGHT OF THE CONSTRUCT
--                   IN ERROR.

PROCEDURE BD4007A IS

     TYPE REC1 IS RECORD
          INT1 : INTEGER;
     END RECORD;

     TYPE REC2 IS RECORD
          INT2 : INTEGER;
     END RECORD;

     TYPE REC3 (D : POSITIVE) IS RECORD
          CASE D IS
               WHEN 1 .. 10 => CHAR3 : CHARACTER;
               WHEN OTHERS => BOOL3 : BOOLEAN;
          END CASE;
     END RECORD;

     TYPE REC4 IS NEW REC3(4);
     TYPE REC5 IS NEW REC3(5);

     FOR REC1 USE RECORD
          INT1 AT 0 RANGE 0 .. INTEGER'SIZE-1;
          INT1 AT 0 RANGE 0 .. INTEGER'SIZE-1;  -- ERROR: SECOND
                                                -- COMPONENT CLAUSE.
     END RECORD;

     FOR REC2 USE RECORD
          INT2 AT 0 RANGE 0 .. INTEGER'SIZE-1;
          INT2 AT 8 RANGE INTEGER'SIZE .. 2*INTEGER'SIZE-1; -- ERROR:
                                          -- SECOND COMPONENT CLAUSE.
     END RECORD;

     FOR REC4 USE RECORD
          CHAR3 AT 0 RANGE 0 .. CHARACTER'SIZE-1;
          BOOL3 AT 0 RANGE 0 .. BOOLEAN'SIZE-1;
          BOOL3 AT 0                                -- ERROR:
              RANGE CHARACTER'SIZE ..               -- SECOND COMPONENT
                    CHARACTER'SIZE+BOOLEAN'SIZE-1;  -- CLAUSE FOR
                                                    -- NONEXISTENT
     END RECORD;                                    -- COMPONENT.

     FOR REC5 USE RECORD
          CHAR3 AT 0 RANGE 0 .. CHARACTER'SIZE-1;
          BOOL4 AT 0                                -- ERROR:
              RANGE CHARACTER'SIZE ..               -- COMPONENT CLAUSE
                    CHARACTER'SIZE+BOOLEAN'SIZE-1;  -- FOR NONEXISTENT
     END RECORD;                                    -- COMPONENT.

BEGIN
     NULL;
END BD4007A;
