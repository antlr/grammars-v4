-- BD4011A.ADA

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
--     CHECK THAT A 'RANGE ATTRIBUTE CANNOT BE USED IN A COMPONENT
--     CLAUSE.

-- HISTORY:
--     BCB 04/11/88  CREATED ORIGINAL TEST.

PROCEDURE BD4011A IS

     INTSIZE : CONSTANT := INTEGER'SIZE-1;

     TYPE I IS ARRAY(0 .. INTSIZE) OF INTEGER;

     SUBTYPE T IS INTEGER RANGE 0 .. INTSIZE;

     TYPE REC IS RECORD
          INT : INTEGER;
     END RECORD;

     TYPE REC2 IS RECORD
          INT2 : INTEGER;
     END RECORD;

     FOR REC USE RECORD
          INT AT 0 RANGE I'RANGE;          -- ERROR: 'RANGE ATTRIBUTE.
     END RECORD;

     FOR REC2 USE RECORD
          INT2 AT 0 RANGE T'RANGE;         -- ERROR: 'RANGE ATTRIBUTE.
     END RECORD;

BEGIN
     NULL;
END BD4011A;
