-- B44004E.ADA

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
-- CHECK THAT "OBJECT IN REC_TYPE(DISCRIMINANT_CONSTRAINT)" IS ILLEGAL.

-- PWB  03/04/86

PROCEDURE B44004E IS

     TYPE REC (DISC : INTEGER) IS RECORD
          NULL;
     END RECORD;
     TYPE REC_PTR IS ACCESS REC;

     RVAR : REC(4);
     RP   : REC_PTR;

BEGIN

     IF RVAR IN REC(4) THEN              -- ERROR: DISCRIMINANT CONSTR.
          RP := NEW REC(1);
          IF RP.ALL NOT IN REC(1) THEN   -- ERROR: DISCRIMINANT CONSTR.
               NULL;
          END IF;
     ELSIF RVAR NOT IN REC(1) THEN       -- ERROR: DISCRIMINANT CONSTR.
          RP := NULL;
          IF RP.ALL IN REC(0) THEN       -- ERROR: DISCRIMINANT CONSTR.
               NULL;
          END IF;
     END IF;

END B44004E;
