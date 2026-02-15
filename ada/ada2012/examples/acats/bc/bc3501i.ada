-- BC3501I.ADA

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
-- CHECK THAT IF A FORMAL GENERIC TYPE FT IS AN ACCESS TYPE, THE
-- CORRESPONDING ACTUAL TYPE PARAMETER MUST BE AN ACCESS TYPE.  CHECK
-- THAT FT DOES NOT MATCH AN ACTUAL PARAMETER THAT IS THE BASE TYPE OF
-- FT'S DESIGNATED TYPE.  CHECK WHEN FT'S DESIGNATED TYPE AND THE ACTUAL
-- TYPE ARE GENERIC FORMAL TYPES APPEARING IN AN ENCLOSING GENERIC
-- DECLARATION.

-- SPS 5/24/82

PROCEDURE BC3501I IS

     GENERIC
          TYPE T IS PRIVATE;
          TYPE U IS PRIVATE;
     PACKAGE PACK IS
          TYPE A IS ACCESS T;
          TYPE AU IS ACCESS U;

          GENERIC 
               TYPE FT IS ACCESS T;
          PACKAGE P IS END P;

          PACKAGE P1 IS NEW P(A);            -- OK.
          PACKAGE P2 IS NEW P(T);            -- ERROR: T IS NOT AN
                                             -- ACCESS TYPE.
          PACKAGE P3 IS NEW P(U);            -- ERROR: U IS NOT AN
                                             -- ACCESS TYPE.

          GENERIC
               TYPE T IS PRIVATE;
               TYPE FT IS ACCESS T;
          PACKAGE PP IS END PP;

          PACKAGE P4 IS NEW PP (U, AU);      -- OK.
          PACKAGE P5 IS NEW PP (U, U);       -- ERROR: U IS NOT AN
                                             -- ACCESS TYPE.
     END PACK;

BEGIN
     NULL;
END BC3501I;
