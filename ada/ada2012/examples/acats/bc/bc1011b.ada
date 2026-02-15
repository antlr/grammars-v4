-- BC1011B.ADA

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
-- CHECK THAT A GENERIC SUBPROGRAM MAY NOT BE USED AS A GENERIC
-- ACTUAL SUBPROGRAM PARAMETER.

-- DAT 9/25/81
-- SPS 10/18/82

PROCEDURE BC1011B IS

     GENERIC
     PROCEDURE P1;

     GENERIC
     PROCEDURE P2 (I : INTEGER);

     GENERIC
     FUNCTION F1 RETURN INTEGER;

     GENERIC
     FUNCTION F2 (I : INTEGER) RETURN INTEGER;

     GENERIC
          WITH PROCEDURE X1;
          WITH PROCEDURE X2 (I : INTEGER);
          WITH FUNCTION Z1 RETURN INTEGER;
          WITH FUNCTION Z2 (I : INTEGER) RETURN INTEGER;
     PACKAGE PK IS END PK;

     PROCEDURE P1 IS BEGIN NULL; END;
     PROCEDURE P2 (I : INTEGER) IS BEGIN NULL; END;
     FUNCTION F1 RETURN INTEGER IS BEGIN RETURN 0; END;
     FUNCTION F2 (I : INTEGER) RETURN INTEGER IS BEGIN RETURN I; END;

     PACKAGE PPP IS
          PROCEDURE I1 IS NEW P1;
          PROCEDURE I2 IS NEW P2;
          FUNCTION I3 IS NEW F1;
          FUNCTION I4 IS NEW F2;

          PACKAGE Z1 IS NEW PK (I1, I2, I3, I4); -- OK.
          PACKAGE Z2 IS NEW PK (P1, I2, I3, I4); -- ERROR: P1.
          PACKAGE Z3 IS NEW PK (I1, P2, I3, I4); -- ERROR: P2.
          PACKAGE Z4 IS NEW PK (I1, I2, F1, I4); -- ERROR: F1.
          PACKAGE Z5 IS NEW PK (I1, I2, I3, F2); -- ERROR: F2.
     END PPP;
BEGIN
     NULL;
END BC1011B;
