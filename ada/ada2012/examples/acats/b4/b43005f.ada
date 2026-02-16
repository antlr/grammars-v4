-- B43005F.ADA

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
--     CHECK THAT THE USE OF MIXED POSITIONAL AND NAMED COMPONENT
--     ASSOCIATIONS IS NOT USED IN RESOLVING THE TYPE OF AN AGGREGATE.

-- HISTORY:
--     BCB 07/14/88

PROCEDURE B43005F IS

     TYPE ARR IS ARRAY(1..4) OF INTEGER;

     TYPE REC (D : INTEGER := 0) IS RECORD
          COMP1 : INTEGER;
          COMP2 : INTEGER;
          COMP3 : BOOLEAN;
     END RECORD;

     PROCEDURE P (X : ARR) IS
     BEGIN
          NULL;
     END P;

     PROCEDURE P (X : REC) IS
     BEGIN
          NULL;
     END P;

BEGIN

     P ((0,1,COMP2 => 2, COMP3 => TRUE));                   -- ERROR:

END B43005F;
