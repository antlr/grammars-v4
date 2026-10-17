-- BC3013A.ADA

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
-- CHECK THAT A SUBPROGRAM SPECIFICATION CANNOT BE USED
-- IN A GENERIC SUBPROGRAM INSTANTIATION.

-- HISTORY:
--      18 Sep 1981   DAT
--      22 Apr 2021   RLB   Added error location indicators.

PROCEDURE BC3013A IS

     GENERIC
     PROCEDURE PR (X : INTEGER);

     GENERIC 
     FUNCTION F (X : INTEGER) RETURN INTEGER;

     GENERIC
     FUNCTION F1 RETURN INTEGER;

     PROCEDURE PR (X : INTEGER) IS BEGIN NULL; END;

     FUNCTION F (X : INTEGER) RETURN INTEGER IS
     BEGIN RETURN 0; END;

     FUNCTION F1 RETURN INTEGER IS BEGIN RETURN 0; END;

     PACKAGE PKG IS
          PROCEDURE I1 (X : INTEGER) IS NEW PR; -- ERROR: FORMAL PART. {11;1}
          PROCEDURE I2 IS NEW PR;               -- OK. {11;1}
          FUNCTION I3 IS NEW F;                 -- OK. {11;1}
          FUNCTION I4 (X : INTEGER) RETURN INTEGER
               IS NEW F;                        -- ERROR: FORMAL PART. {1:11;1}
          FUNCTION I5 IS NEW F1;                -- OK. {11;1}
          FUNCTION I6 RETURN INTEGER IS NEW F1; -- ERROR: RETURN. {11;1}
     END PKG;

BEGIN
     NULL;
END BC3013A;
