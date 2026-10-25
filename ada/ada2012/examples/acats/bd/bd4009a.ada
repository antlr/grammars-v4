-- BD4009A.ADA

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
--     CHECK THAT A RECORD REPRESENTATION CLAUSE CANNOT SPECIFY
--     OVERLAPPING STORAGE FOR TWO DIFFERENT COMPONENTS IF THEY DO NOT
--     BELONG TO DIFFERENT VARIANTS.

-- HISTORY:
--     BCB 04/11/88  CREATED ORIGINAL TEST.
--     THS 03/30/90  MOVED "-- ERROR:" TO THE RIGHT OF THE CONSTRUCT
--                   IN ERROR.  REMOVED 'OK' CASE.

PROCEDURE BD4009A IS

     TYPE INT IS RANGE 0 .. 1;

     TYPE REC(DISC : INT) IS RECORD
          CASE DISC IS
               WHEN 0 =>
                    I1 : INTEGER;
                    C1 : CHARACTER;
               WHEN 1 =>
                    NULL;
          END CASE;
     END RECORD;

     FOR REC USE RECORD
          I1 AT 0 RANGE 0 .. INTEGER'SIZE-1;
          C1 AT 0 RANGE 0 .. CHARACTER'SIZE-1;  -- ERROR:
                                                -- OVERLAPPING STORAGE.
     END RECORD;

BEGIN
     NULL;
END BD4009A;
