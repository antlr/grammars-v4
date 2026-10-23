-- B41202D.ADA

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
-- CHECK THAT THE NAME PART OF A SLICE MAY NOT BE AN IDENTIFIER WHOSE
--   DESIGNATED OBJECT IS AN ACCESS VALUE DESIGNATING AN ARRAY.

-- WKB 8/5/81
-- SPS 2/4/83
-- JBG 2/6/83

PROCEDURE B41202D IS

     TYPE A IS ARRAY (NATURAL RANGE <> ) OF INTEGER;
     TYPE AA IS ACCESS A;
     TYPE AAA IS ACCESS AA;

     I : A (1..3);
     AC : AAA;

BEGIN

     AC := NEW AA'(NEW A'(2,4,6,8,10));
     I := AC(1..3);                    -- ERROR: DOUBLE DEREFERENCING
                                       --        VIA SLICING.

END B41202D;
