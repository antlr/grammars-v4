-- B84005B.ADA

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
--     CHECK THAT TWO POTENTIALLY VISIBLE HOMOGRAPHS OF A SUBPROGRAM
--     IDENTIFIER CAN BE MADE DIRECTLY VISIBLE BY A USE CLAUSE, AND THAT
--     UNLESS DIFFERENT FORMAL PARAMETER NAMES ARE USED THE SUBPROGRAMS
--     ARE AMBIGUOUS.

-- HISTORY:
--     JET 07/21/88  CREATED ORIGINAL TEST.

PROCEDURE B84005B IS

     PACKAGE PACK1 IS
          FUNCTION FUNK(X : INTEGER) RETURN INTEGER;
          PROCEDURE PROK(X : INTEGER; Y : OUT INTEGER);
     END PACK1;

     PACKAGE PACK2 IS
          FUNCTION FUNK(X : INTEGER) RETURN INTEGER;
          PROCEDURE PROK(X : INTEGER; Y : OUT INTEGER);
     END PACK2;

     USE PACK1, PACK2;
     I, J : INTEGER := 0;

     PACKAGE BODY PACK1 IS
          FUNCTION FUNK(X : INTEGER) RETURN INTEGER IS
          BEGIN
               RETURN 1;
          END FUNK;

          PROCEDURE PROK(X : INTEGER; Y : OUT INTEGER) IS
          BEGIN
               Y := 1;
          END PROK;
     END PACK1;

     PACKAGE BODY PACK2 IS
          FUNCTION FUNK(X : INTEGER) RETURN INTEGER IS
          BEGIN
               RETURN 2;
          END FUNK;

          PROCEDURE PROK(X : INTEGER; Y : OUT INTEGER) IS
          BEGIN
               Y := 2;
          END PROK;
     END PACK2;

BEGIN

     I := FUNK(J);                         -- ERROR: FUNK IS AMBIGUOUS.

     I := FUNK(X => J);                    -- ERROR: FUNK IS AMBIGUOUS.

     PROK(I, J);                           -- ERROR: PROK IS AMBIGUOUS.

     PROK(X => I, Y => J);                 -- ERROR: PROK IS AMBIGUOUS.

END B84005B;
