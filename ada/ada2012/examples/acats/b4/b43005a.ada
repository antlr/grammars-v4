-- B43005A.ADA

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
--     CHECK THAT THE NUMBER OF ARRAY DIMENSIONS IS NOT USED IN
--     RESOLVING THE TYPE OF AN AGGREGATE.

-- HISTORY:
--     BCB 01/22/88  CREATED ORIGINAL TEST.
--     BCB 03/13/90  CHANGED "-- ERROR." TO "-- ERROR:".

PROCEDURE B43005A IS

     TYPE ARR1 IS ARRAY (1..2) OF INTEGER;
     TYPE ARR2 IS ARRAY (1..2, 1..2) OF INTEGER;

     PROCEDURE P (X : ARR1) IS
     BEGIN
          NULL;
     END P;

     PROCEDURE P (X : ARR2) IS
     BEGIN
          NULL;
     END P;

BEGIN

     P (ARR1'(3,4));                                   -- OK.
     P (ARR2'((3,4), (5,6)));                          -- OK.

     P ((3,4));                                        -- ERROR:
     P (((3,4), (5,6)));                               -- ERROR:

END B43005A;
