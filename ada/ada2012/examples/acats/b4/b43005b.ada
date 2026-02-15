-- B43005B.ADA

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
--     CHECK THAT THE NUMBER OF COMPONENT ASSOCIATIONS IS NOT USED IN
--     RESOLVING THE TYPE OF AN AGGREGATE.

-- HISTORY:
--     BCB 07/14/88  CREATED ORIGINAL TEST.

PROCEDURE B43005B IS

     TYPE REC1 IS RECORD
          COMP1 : INTEGER;
          COMP2 : INTEGER;
          COMP3 : INTEGER;
     END RECORD;

     TYPE REC2 IS RECORD
          COMP1 : INTEGER;
          COMP2 : INTEGER;
          COMP3 : INTEGER;
          COMP4 : INTEGER;
          COMP5 : INTEGER;
     END RECORD;

     PROCEDURE P (X : REC1) IS
     BEGIN
          NULL;
     END P;

     PROCEDURE P (X : REC2) IS
     BEGIN
          NULL;
     END P;

BEGIN

     P (REC1'(1,2,3));                                 -- OK.
     P (REC2'(1,2,3,4,5));                             -- OK.

     P ((1,2,3));                                      -- ERROR:
     P ((1,2,3,4,5));                                  -- ERROR:

END B43005B;
