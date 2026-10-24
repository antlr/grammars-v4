-- B41327B.ADA

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
-- CHECK THAT IMPLICITLY DECLARED EQUALITY AND INEQUALITY OPERATORS
-- MAY NOT BE SELECTED FROM OUTSIDE A PACKAGE USING AN EXPANDED NAME,
-- FOR A LIMITED PRIVATE TYPE.

-- CHANGE HISTORY:
--      18 Jul 1986   TBN
--      22 Apr 2021   RLB   Added error location indicators.
--!

PROCEDURE B41327B IS

     PACKAGE P IS
          TYPE KEY IS LIMITED PRIVATE;
          TYPE CHAR IS LIMITED PRIVATE;
          FUNCTION INIT_KEY (X : NATURAL) RETURN KEY;
          FUNCTION INIT_CHAR (X : CHARACTER) RETURN CHAR;
     PRIVATE
          TYPE KEY IS NEW NATURAL;
          TYPE CHAR IS NEW CHARACTER;
     END P;

     BOOLEAN_1 : BOOLEAN := TRUE;

     PACKAGE BODY P IS

          FUNCTION INIT_KEY (X : NATURAL) RETURN KEY IS
          BEGIN
               RETURN (KEY (X));
          END INIT_KEY;

          FUNCTION INIT_CHAR (X : CHARACTER) RETURN CHAR IS
          BEGIN
               RETURN (CHAR (X));
          END INIT_CHAR;

     BEGIN
          NULL;
     END P;

BEGIN

     BOOLEAN_1 := P."=" (P.INIT_KEY (1), P.INIT_KEY (2));    -- ERROR: {6;1}
     NULL;
     BOOLEAN_1 := P."/=" (P.INIT_CHAR ('A'),
                          P.INIT_CHAR ('B'));                -- ERROR: {1:6;1}

END B41327B;
