-- BC3503C.ADA

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
-- TYPE T IS AN ARRAY TYPE OR A TYPE WITH DISCRIMINANTS AND T IS
-- MATCHED BY U, THE DESIGNATED TYPE OF THE ACTUAL PARAMETER, CHECK
-- THAT U MUST BE CONSTRAINED IF AND ONLY IF T IS CONSTRAINED.

-- CHECK WHEN U IS A GENERIC FORMAL TYPE APPEARING IN AN ENCLOSING
-- GENERIC DECLARATION.

-- SPS 5/27/82
-- JRL 10/19/96 Removed cases under discussion in AI-00034.
-- EDS 12/01/97 Removed case where the actual designated types statically match

PROCEDURE BC3503C IS

     GENERIC
          TYPE U (D : INTEGER) IS PRIVATE;
     PACKAGE PACK IS
          SUBTYPE CU IS U (D => 3);
          TYPE ACU IS ACCESS CU;
          TYPE AU IS ACCESS U;
          SUBTYPE CAU IS AU (D => 3);
          TYPE AUC IS ACCESS U (D => 3);

          GENERIC
               TYPE FT IS ACCESS CU;
          PACKAGE PCU IS END PCU;

          GENERIC
               TYPE FT IS ACCESS U;
          PACKAGE PU IS END PU;

          PACKAGE PCU1 IS NEW PCU (ACU);     -- OK.
          PACKAGE PCU2 IS NEW PCU (AUC);     -- OK.
          PACKAGE PCU4 IS NEW PCU (AU);      -- ERROR: U IS NOT
                                             -- CONSTRAINED.   

          PACKAGE PU1 IS NEW PU (ACU);       -- ERROR: ACU IS
                                             -- CONSTRAINED.      
          PACKAGE PU2 IS NEW PU (AUC);       -- ERROR: AUC IS
                                             -- CONSTRAINED.   
          PACKAGE PU3 IS NEW PU (CAU);       -- OK.
  
          PACKAGE PU4 IS NEW PU (AU);        -- OK.

     END PACK;

BEGIN
     NULL;
END BC3503C;
