-- BC3018A.ADA

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
-- CHECK THAT IF TWO OR MORE FORMAL SUBPROGRAMS HAVE THE SAME NAME
-- OR OPERATOR SYMBOL, THEN THE NAMED ASSOCIATIONS ARE NOT ALLOWED
-- FOR THE CORRESPONDING ACTUAL GENERIC PARAMETERS

-- HISTORY:
--      04 Mar 1982   KEI
--      24 Jan 1984   RJK   Added to ACVC.
--      22 Apr 2021   RLB   Added error location indicators.

PROCEDURE BC3018A IS

     TYPE REAL IS DIGITS 4;

     GENERIC
          WITH FUNCTION "+" (X,Y : INTEGER) RETURN INTEGER;
          WITH FUNCTION "+" (X,Y : REAL) RETURN REAL IS <>;
     PACKAGE PACK1 IS
     END PACK1;

     FUNCTION INT_PLUS (X,Y : INTEGER) RETURN INTEGER;
     FUNCTION REA_PLUS (X,Y : REAL ) RETURN REAL;

     GENERIC
          WITH FUNCTION PLUS (X, Y : INTEGER) RETURN INTEGER;
          WITH FUNCTION PLUS (X, Y : REAL) RETURN REAL IS REA_PLUS;
     PACKAGE PACK2 IS
     END PACK2;

     FUNCTION INT_PLUS (X,Y : INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN X+Y;
     END INT_PLUS;

     FUNCTION REA_PLUS (X,Y : REAL) RETURN REAL IS
     BEGIN
          RETURN X+Y;
     END REA_PLUS;

     PACKAGE PACK_1 IS NEW PACK1(INT_PLUS,REA_PLUS);    -- OK. {6;1}

     PACKAGE PACK_2 IS NEW PACK1(INT_PLUS,
                                 "+" => REA_PLUS);      -- ERROR: "+". {1:6;1}

     PACKAGE PACK_3 IS NEW PACK1("+" => INT_PLUS,
                                 "+" => REA_PLUS);      -- ERROR: "+". {1:6;1}

     PACKAGE PACK_4 IS NEW PACK1 ("+" => INT_PLUS);     -- ERROR: "+". {6;1}

     PACKAGE PACK_5 IS NEW PACK2(INT_PLUS,REA_PLUS);    -- OK. {6;1}

     PACKAGE PACK_6 IS NEW PACK2(INT_PLUS,
                                 PLUS => REA_PLUS);     -- ERROR: PLUS. {1:6;1}

     PACKAGE PACK_7 IS NEW PACK2(PLUS => INT_PLUS,
                                 PLUS => REA_PLUS);     -- ERROR: PLUS. {1:6;1}

     PACKAGE PACK_8 IS NEW PACK2 (PLUS => INT_PLUS);    -- ERROR: PLUS. {6;1}

BEGIN
     NULL;
END BC3018A;
