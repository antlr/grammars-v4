-- B43221B.ADA

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
--     CHECK THAT THE LENGTH OF AN ARRAY AGGREGATE IS NOT USED IN
--     OVERLOAD RESOLUTION.

-- HISTORY:
--     DHH 06/17/88 CREATED ORIGINAL TEST.

PROCEDURE B43221B IS

     TYPE BOOLEN IS ARRAY(1 .. 2) OF BOOLEAN;

     TYPE BOOLAN IS ARRAY(1 .. 4) OF BOOLEAN;

     PROCEDURE PROC_VR(X : BOOLEN) IS
     BEGIN
          NULL;
     END PROC_VR;

     PROCEDURE PROC_VR(X : BOOLAN) IS
     BEGIN
          NULL;
     END PROC_VR;

BEGIN
     PROC_VR((TRUE, FALSE));                       -- ERROR: AMBIGUOUS.

     PROC_VR((TRUE, TRUE, TRUE, FALSE));           -- ERROR: AMBIGUOUS.

     PROC_VR((1 => TRUE, 2 => FALSE));             -- ERROR: AMBIGUOUS.

     PROC_VR((1 .. 4 => TRUE));                    -- ERROR: AMBIGUOUS.

END B43221B;
