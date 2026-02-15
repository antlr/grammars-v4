-- B95081A.ADA

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
-- CHECK THAT FOR A MIXTURE OF NAMED AND POSITIONAL NOTATION, NAMED
-- PARAMETERS CANNOT BE INTERLEAVED WITH NOR PRECEDE POSITIONAL
-- PARAMETERS.  SUBTESTS ARE:

--   (A) NAMED PARAMETERS CANNOT BE INTERLEAVED WITH NOR PRECEDE
--       POSITIONAL PARAMETERS.
--   (B) A NAMED PARAMETER AND A LATER POSITIONAL PARAMETER CANNOT BE
--       SPECIFIED FOR THE SAME FORMAL PARAMETER.
--   (C) TWO OR MORE NAMED PARAMETERS CANNOT SPECIFY THE SAME FORMAL
--       PARAMETER.
--   (D) THE NAME USED IN A NAMED PARAMETER MUST ONLY BE A NAME OF A
--       FORMAL PARAMETER.
--   (E) A NAMED PARAMETER CANNOT BE PROVIDED FOR A FORMAL PARAMETER IF
--       A POSITIONAL PARAMETER HAS ALREADY BEEN GIVEN FOR THAT FORMAL
--       PARAMETER.
--   (F) A FORMAL PARAMETER IN NAMED PARAMETER ASSOCIATION IS NOT
--       CONFUSED WITH AN ACTUAL PARAMETER IDENTIFIER HAVING THE SAME
--       SPELLING.

-- JWC 7/16/85
-- JRK 8/21/85

PROCEDURE B95081A IS

     I, J : INTEGER := 0;
     X1   : INTEGER := 0;

     TASK T IS
          ENTRY E (X1 : INTEGER; X2 : IN OUT INTEGER;
                   X3 : OUT INTEGER);
     END T;

     TASK BODY T IS
     BEGIN
          NULL;
     END T;

BEGIN

     -- (A)
     T.E (X1 => 1, X2 => I, J);       -- ERROR: POSITIONAL AFTER NAMED.
     NULL;
     T.E (1, X2 => I, J);             -- ERROR: POSITIONAL AFTER NAMED.
     NULL;

     -- (B)
     T.E (X2 => I, J, X3 => J);       -- ERROR: X2 SPECIFIED TWICE.
     NULL;

     -- (C)
     T.E (X1 => 1, X2 => J, X1 => I); -- ERROR: X1 SPECIFIED TWICE.
     NULL;
     T.E (X1 => 1, X2 => I, X3 => J, X1 => 2); -- ERROR: X1 SPECIFIED
                                               --        TWICE.
     NULL;

     -- (D)
     T.E (I => 1, X2 => I, X3 => J);  -- ERROR: I NOT A FORMAL.
     NULL;

     -- (E)
     T.E (1, X1 => 1, X2 => J);       -- ERROR: X1 SPECIFIED TWICE.
     NULL;
     T.E (1, X2 => I, X3 => J, X1 => 1); -- ERROR: X1 SPECIFIED TWICE.
     NULL;

     -- (F)
     T.E (X1 => X1, X2 => I, X3 => J);  -- OK: NO CONFUSION OF X1.

END B95081A;
