-- BD4006A.TST

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
--     CHECK THAT THE EXPRESSIONS IN ALIGNMENT AND COMPONENT CLAUSES
--     MUST BE STATIC, INTEGER EXPRESSIONS.

-- HISTORY:
--     BCB 04/11/88  CREATED ORIGINAL TEST.
--     THS 04/16/90  CHANGED MOD TO A MACRO VALUE, CHANGED EXTENSION
--                   FROM '.ADA' TO '.TST', AND MOVED "-- ERROR:" TO
--                   THE RIGHT OF THE CONSTRUCT IN ERROR.

-- MACRO SUBSTITUTION:
--     $ALIGNMENT IS THE VALUE USED TO ALIGN A RECORD ON A BOUNDARY
--     DEFINED BY THE IMPLEMENTATION.

PROCEDURE BD4006A IS

     TYPE REC1 IS RECORD
          NULL;
     END RECORD;

     TYPE REC2 IS RECORD
          INT2 : INTEGER;
     END RECORD;

     TYPE REC3 IS RECORD
          INT3 : INTEGER;
     END RECORD;

     TYPE NEWREC IS RECORD
          NULL;
     END RECORD;

     TYPE NEWREC2 IS RECORD
          INT2 : INTEGER;
     END RECORD;

     TYPE NEWREC3 IS RECORD
          INT3 : INTEGER;
     END RECORD;

     NONSTATIC1 : INTEGER := 0;
     NONSTATIC2 : INTEGER := $ALIGNMENT;

     REAL : CONSTANT := $ALIGNMENT * 1.0;

     FOR REC1 USE RECORD AT MOD NONSTATIC2;       -- ERROR: NON-STATIC
     END RECORD;                                  -- ALIGNMENT VALUE.

     FOR  REC2 USE RECORD
          INT2 AT NONSTATIC1 RANGE 0 .. INTEGER'SIZE-1; -- ERROR:
                                                        -- NON-STATIC
                                                        -- COMPONENT
                                                        -- VALUE.
     END RECORD;

     FOR REC3 USE RECORD
          INT3 AT 1 RANGE NONSTATIC1 .. INTEGER'SIZE-1; -- ERROR:
                                                        -- NON-STATIC
                                                        -- RANGE VALUE.
     END RECORD;

     FOR NEWREC USE RECORD AT MOD REAL;          -- ERROR: NON-INTEGER
                                                 -- ALIGNMENT VALUE.
     END RECORD;

     FOR NEWREC2 USE RECORD
          INT2 AT 0.0 RANGE 0 .. INTEGER'SIZE-1; -- ERROR: NON-INTEGER
                                                 -- COMPONENT VALUE.
     END RECORD;

     FOR NEWREC3 USE RECORD
          INT3 AT 1 RANGE 0.0 .. INTEGER'SIZE-1; -- ERROR: NON-INTEGER
                                                 -- RANGE VALUE.
     END RECORD;

BEGIN
     NULL;
END BD4006A;
