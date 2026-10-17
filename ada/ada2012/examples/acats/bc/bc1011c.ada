-- BC1011C.ADA

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
-- CHECK THAT A GENERIC FUNCTION CAN NOT BE USED AS A
-- CALLED FUNCTION

-- KEI 3/4/82
-- RJK 1/23/84     ADDED TO ACVC
-- JWC 6/28/85   RENAMED FROM BC10ACA-B.ADA

PROCEDURE BC1011C IS

     GENERIC
     FUNCTION F1 (F : BOOLEAN) RETURN BOOLEAN;

     FUNCTION F1 (F : BOOLEAN) RETURN BOOLEAN IS
     BEGIN
          RETURN FALSE;
     END F1;

BEGIN
     DECLARE
          P, B : BOOLEAN := TRUE;

          FUNCTION G1 IS NEW F1;
          FUNCTION G2 IS NEW F1;

     BEGIN
          B := NOT G1 (P) ;     -- OK.
          B := F1 (P);          -- ERROR: GENERIC
          B := G2 (B);          -- OK.
     END;
END BC1011C;
