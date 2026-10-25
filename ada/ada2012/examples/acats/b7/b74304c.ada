-- B74304C.ADA

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
-- CHECK THAT A GENERIC IN PARAMETER CANNOT BE A YET-DEFERRED
-- CONSTANT EVEN WITHIN A GENERIC UNIT WHICH IS NEVER INSTANTIATED.

-- DIFFERENT THAN B74304B-B, PACKAGE PKG IS A GENERIC.

-- DAT 9/18/81
-- SPS 4/28/82
-- SPS 10/19/83
-- PWN 11/09/95  REMOVED CHECKS WHERE DEFERRED CONSTANT RULES RELAXED.
-- PWN 03/28/96  Restored checks in Ada 95 legal format.

PROCEDURE B74304C IS

     GENERIC
          TYPE T IS PRIVATE;
          INP : IN T;
     PACKAGE P1 IS END P1;

     GENERIC
     PACKAGE PKG IS
          TYPE PRIV IS PRIVATE;
          DC : CONSTANT PRIV;

          PACKAGE I1 IS NEW P1 (PRIV, DC);   -- ERROR: PRIV AND DC
                                             --     DEFERRED.

     PRIVATE
          TYPE PRIV IS (X);

          PACKAGE I2 IS NEW P1 (PRIV, DC);   -- ERROR: DC DEFERRED.

          GENERIC
               INP : PRIV := DC;             -- OK.
          PACKAGE GP2 IS END GP2;

          GENERIC
               INP : PRIV := X;
          PACKAGE GP3 IS END GP3;

          PACKAGE I3 IS NEW GP3 (DC);        -- ERROR: DC DEFERRED.
          PACKAGE I4 IS NEW GP3 (X);         -- OK.
          PACKAGE I5 IS NEW GP3;             -- OK.
          PACKAGE I6 IS NEW P1 (PRIV, X);    -- OK.

          DC : CONSTANT PRIV := X;

          PACKAGE I7 IS NEW P1 (PRIV, DC);   -- OK.
          PACKAGE I8 IS NEW GP3 (DC);        -- OK.
     END;

BEGIN
     NULL;
END B74304C;
