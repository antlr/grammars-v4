-- B95061F.ADA

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
-- CHECK THAT THE TYPE OF A FORMAL PARAMETER MUST BE DESIGNATED BY A
-- TYPE MARK; IT CANNOT HAVE A DISCRIMINANT CONSTRAINT.
 
-- JWC 6/18/85
 
PROCEDURE B95061F IS
 
     TYPE REC ( DISC : INTEGER ) IS
          RECORD NULL; END RECORD;
 
     TASK T IS
 
          ENTRY E1 ( X : REC (DISC => 1) );     -- ERROR: DISCRIMINANT
                                                -- CONSTRAINT.
     END T;
 
     TASK BODY T IS
     BEGIN
          NULL;
     END T;
 
BEGIN
     NULL;
END B95061F;
