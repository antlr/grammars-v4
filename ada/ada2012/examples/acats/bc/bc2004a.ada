-- BC2004A.ADA

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
-- CHECK THAT THE BODY (IF THERE IS ONE) OF A GENERIC UNIT
-- DECLARED IN A GIVEN DECLARATIVE PART MUST APPEAR AFTER THE
-- CORRESPONDING GENERIC DECLARATION IN THAT DECLARATIVE PART

-- KEI 3/4/82
-- RJK 1/24/84     ADDED TO ACVC
-- JWC 6/28/85   RENAMED FROM BC20ABA-B.ADA

PROCEDURE BC2004A IS

     TYPE REAL IS DIGITS 4;

     PACKAGE BODY REAL_PLUS_MINUS IS          -- ERROR: BEFORE GENERIC
                                              -- DECLARATION

          FUNCTION REAL_PLUS (P1, P2: REAL) RETURN REAL IS
          BEGIN
               RETURN P1 + P2;
          END REAL_PLUS;

          FUNCTION REAL_MINUS (P1, P2: REAL) RETURN REAL IS
          BEGIN
               RETURN P1 - P2;
          END REAL_MINUS;

     END REAL_PLUS_MINUS;

     GENERIC
          R : REAL;
     PACKAGE REAL_PLUS_MINUS IS
          FUNCTION REAL_PLUS (P1, P2: REAL) RETURN REAL;
          FUNCTION REAL_MINUS (P1, P2: REAL) RETURN REAL;
     END REAL_PLUS_MINUS;

BEGIN
     NULL;
END BC2004A;
