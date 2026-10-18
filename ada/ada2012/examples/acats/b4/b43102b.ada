-- B43102B.ADA

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
--     CHECK THAT IN A RECORD AGGREGATE, THE STATICNESS OF THE
--     EXPRESSION GIVING THE VALUE OF A DISCRIMINANT WHICH GOVERNS
--     A VARIANT PART IS NOT USED TO RESOLVE THE TYPE OF THE
--     AGGREGATE.

-- HISTORY:
--     DHH 06/16/88 CREATED ORIGINAL TEST.

PROCEDURE B43102B IS

     SUBTYPE SMALL IS INTEGER RANGE 1 .. 3;

     TYPE VAR_REC(DIS : SMALL) IS
          RECORD
               CHAR : CHARACTER;
               CASE DIS IS
                    WHEN 1 =>
                         BOOL : BOOLEAN;
                    WHEN 2 =>
                         T : CHARACTER;
                    WHEN 3 =>
                         I : INTEGER;
               END CASE;
          END RECORD;

     TYPE REC_BOOL IS
          RECORD
               DIS : SMALL;
               CHAR : CHARACTER;
               BOOL : BOOLEAN;
          END RECORD;

     TYPE REC_CHAR IS
          RECORD
               DIS : SMALL;
               CHAR : CHARACTER;
               T : CHARACTER;
          END RECORD;

     TYPE REC_INT IS
          RECORD
               DIS : SMALL;
               CHAR : CHARACTER;
               I : INTEGER;
          END RECORD;

     X : SMALL := 2;

     FUNCTION IDENT(X : INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN X;
     END IDENT;

     PROCEDURE PROC_VR(X : VAR_REC) IS
     BEGIN
          NULL;
     END PROC_VR;

     PROCEDURE PROC_VR(X : REC_BOOL) IS
     BEGIN
          NULL;
     END PROC_VR;

     PROCEDURE PROC_VR(X : REC_CHAR) IS
     BEGIN
          NULL;
     END PROC_VR;

     PROCEDURE PROC_VR(X : REC_INT) IS
     BEGIN
          NULL;
     END PROC_VR;

BEGIN
     PROC_VR((1,                                    -- ERROR: AMBIGUOUS.
             'T',
             TRUE));

     PROC_VR((IDENT(1),                             -- ERROR: AMBIGUOUS.
             'T',
             TRUE));

     PROC_VR((X,                                    -- ERROR: AMBIGUOUS.
             'T',
             'U'));

     PROC_VR((3 * X/X,                              -- ERROR: AMBIGUOUS.
             'T',
             7));
END B43102B;
