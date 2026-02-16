-- BC1206A.ADA

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
-- CHECK THAT AN UNCONSTRAINED FORMAL TYPE WITH DISCRIMINANTS IS NOT 
-- ALLOWED AS THE PARENT TYPE IN THE FULL DECLARATION OF A PRIVATE
-- TYPE.

-- R.WILLIAMS 9/25/86

PROCEDURE BC1206A IS
     
     GENERIC
          TYPE FORM (D : INTEGER) IS PRIVATE;
     PACKAGE PKG IS
          TYPE PRIV1 (D : INTEGER) IS PRIVATE;
          TYPE PRIV2 IS PRIVATE;
          TYPE PRIV3 IS PRIVATE;

     PRIVATE 
          TYPE PRIV1 (D : INTEGER) IS NEW FORM;     -- ERROR: 
                                                    -- UNCONSTRAINED.
          TYPE PRIV2 IS NEW FORM;                   -- ERROR: 
                                                    -- UNCONSTRAINED.
          TYPE PRIV3 IS NEW FORM (5);               -- OK.
     END PKG;

BEGIN
     NULL;
END BC1206A;
