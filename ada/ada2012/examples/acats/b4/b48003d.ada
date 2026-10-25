-- B48003D.ADA

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
-- CHECK THAT ILLEGAL FORMS OF ALLOCATORS ARE FORBIDDEN. IN PARTICULAR,
-- FOR ALLOCATORS OF THE FORM "NEW T'(X)", CHECK THAT IF T IS AN
-- UNCONSTRAINED ARRAY TYPE, (X) CANNOT BE AN AGGREGATE WITH AN OTHERS
-- CHOICE.

-- EG  08/03/84

PROCEDURE B48003D IS

     TYPE UA IS ARRAY(INTEGER RANGE <>) OF INTEGER;

     TYPE A_UA IS ACCESS UA;

     UA1 : A_UA := NEW UA'(1, 2, OTHERS => 3);    -- ERROR:
     UA2 : CONSTANT A_UA := NEW UA'(OTHERS => 1); -- ERROR:
     UA3 : A_UA;

BEGIN

     UA3 := NEW UA'(1, OTHERS => 2);              -- ERROR:

END B48003D;
