-- B95080A.ADA

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
-- FOR ENTRIES HAVING NO DEFAULT PARAMETER VALUES, CHECK THAT THE
-- NUMBER OF ACTUAL POSITIONAL PARAMETERS AND NAMED PARAMETERS MUST
-- EQUAL THE NUMBER OF FORMAL PARAMETERS.

-- JWC 7/16/85

PROCEDURE B95080A IS

     A : INTEGER := 0;

     TASK T IS

          ENTRY E1 (X : IN INTEGER);

          ENTRY E2 (X: IN INTEGER; Y: IN OUT INTEGER; Z: OUT INTEGER);

          ENTRY E3 (1 .. 5) (X: IN INTEGER; Y: IN OUT INTEGER;
                             Z: OUT INTEGER);

     END T;

     TASK BODY T IS
     BEGIN
          NULL;
     END T;

BEGIN

     T.E1;                           -- ERROR: TOO FEW ARGS.
     NULL;
     T.E1 (1, 2);                    -- ERROR: TOO MANY ARGS.
     NULL;
     T.E2 (1, A);                    -- ERROR: TOO FEW ARGS.
     NULL;
     T.E2 (X=>1, Y=>A);              -- ERROR: TOO FEW ARGS.
     NULL;
     T.E2 (1, Y=>A);                 -- ERROR: TOO FEW ARGS.
     NULL;
     T.E3 (3) (1, A);                -- ERROR: TOO FEW ARGS.
     NULL;
     T.E3 (3) (X=>1, Y=>A);          -- ERROR: TOO FEW ARGS.
     NULL;
     T.E3 (3) (1, Y=>A);             -- ERROR: TOO FEW ARGS.
     NULL;
     T.E3 (3) (1, A, A, A);          -- ERROR: TOO MANY ARGS.

END B95080A;
