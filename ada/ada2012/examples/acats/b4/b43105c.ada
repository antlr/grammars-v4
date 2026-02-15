-- B43105C.ADA

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
--     CHECK THAT IN A RECORD AGGREGATE, THE RESOLUTION OF E IN
--     (X | Y => E) DOES NOT USE THE FACT THAT X AND Y MUST HAVE
--     THE SAME TYPE.

-- HISTORY:
--     DHH 06/16/88 CREATED ORIGINAL TEST.

PROCEDURE B43105C IS

     TYPE COLOR IS (RED, YELLOW, BLUE);

     TYPE LIGHT IS (RED, GREEN, YELLOW);

     TYPE REC1(X : COLOR) IS
          RECORD
               Y : LIGHT;
          END RECORD;

     TYPE REC2 IS
          RECORD
               X : COLOR;
               Y : COLOR;
          END RECORD;

     PROCEDURE PROC_VR(X : REC1) IS
     BEGIN
          NULL;
     END PROC_VR;

     PROCEDURE PROC_VR(X : REC2) IS
     BEGIN
          NULL;
     END PROC_VR;

BEGIN

     PROC_VR((X | Y => RED));                      -- ERROR: AMBIGUOUS.

END B43105C;
