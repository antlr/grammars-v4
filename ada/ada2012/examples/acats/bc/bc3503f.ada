-- BC3503F.ADA

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
-- WHEN A GENERIC FORMAL TYPE FT IS AN ACCESS TYPE AND ITS DESIGNATED
-- TYPE T IS AN ACCESS TYPE TO AN ARRAY TYPE OR A TYPE WITH
-- DISCRIMINANTS, AND T IS MATCHED BY U, THE DESIGNATED TYPE OF THE
-- ACTUAL PARAMETER, CHECK THAT U MUST BE CONSTRAINED IF AND ONLY IF T 
-- IS CONSTRAINED. 

-- CHECK WHEN U IS A GENERIC FORMAL ACCESS TYPE APPEARING IN AN 
-- ENCLOSING GENERIC DECLARATION.

-- SPS 6/2/82

PROCEDURE BC3503F IS

     GENERIC 
          TYPE U IS ACCESS STRING;
     PACKAGE PACK IS
          SUBTYPE CU IS U (1 .. 3);
          TYPE ACU IS ACCESS CU;
          TYPE AU IS ACCESS U;
          TYPE AUC IS ACCESS U (1 .. 3);

          GENERIC
               TYPE FT IS ACCESS CU;
          PACKAGE PCU IS END PCU;

          GENERIC
               TYPE FT IS ACCESS U;
          PACKAGE PU IS END PU;

          PACKAGE PCU1 IS NEW PCU (ACU);     -- OK.
          PACKAGE PCU2 IS NEW PCU (AU);      -- ERROR: AU.
          PACKAGE PCU3 IS NEW PCU (AUC);     -- OK.

          PACKAGE PU1 IS NEW PU (ACU);       -- ERROR: ACU.
          PACKAGE PU2 IS NEW PU (AU);        -- OK.
          PACKAGE PU3 IS NEW PU (AUC);       -- ERROR: AUC.

     END PACK;

BEGIN
     NULL;
END BC3503F;
