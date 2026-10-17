-- BC3502K.ADA

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
-- CHECK THAT WHEN A GENERIC FORMAL TYPE IS AN ACCESS TYPE, THE FORMAL
-- TYPE IS ONLY MATCHED WHEN ITS DESIGNATED BASE TYPE IS THE SAME AS THE
-- DESIGNATED BASE TYPE OF THE ACTUAL PARAMTER.

-- CHECK WHEN THE DESIGNATED BASE TYPE OF THE FORMAL IS A GENERIC FORMAL
-- TYPE APPEARING IN AN ENCLOSING GENERIC DECLARATION.

-- SPS 5/26/82

PROCEDURE BC3502K IS

     GENERIC
          TYPE T IS (<>);
     PACKAGE PACK IS

          GENERIC
               TYPE FT IS ACCESS T;
          PACKAGE P IS END P;

          TYPE A IS ACCESS T;
          TYPE AI IS ACCESS INTEGER;
          TYPE AB IS ACCESS BOOLEAN;

          PACKAGE P1 IS NEW P (A);           -- OK.
          PACKAGE P2 IS NEW P (AI);          -- ERROR: AI.
          PACKAGE P3 IS NEW P (AB);          -- ERROR: AB.

     END PACK;

BEGIN
     NULL;
END BC3502K;
