-- B95002A.ADA

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
-- CHECK THAT THE NAME OF A SINGLE (NON-FAMILY) ENTRY CAN NOT BE
--   GIVEN AS AN INDEXED COMPONENT IN AN ACCEPT STATEMENT OR IN
--   AN ENTRY CALL.

-- JRK 10/22/81
-- JWC 6/28/85   RENAMED TO -AB

PROCEDURE B95002A IS

     SUBTYPE INT IS INTEGER RANGE 1..5;

     TASK T IS
          ENTRY E0;
          ENTRY E1 (B : BOOLEAN);
          ENTRY E2 (B1, B2 : BOOLEAN);
     END T;

     TASK BODY T IS
     BEGIN
          ACCEPT E0;                    -- OK.
          ACCEPT E0 (1);                -- ERROR: NON-FAMILY INDEXED.
          NULL;

          ACCEPT E1 (B : BOOLEAN);      -- OK.
          ACCEPT E1 (1) (B : BOOLEAN);  -- ERROR: NON-FAMILY INDEXED.
          NULL;

          ACCEPT E2 (B1, B2 : BOOLEAN); -- OK.
          ACCEPT E2 (1) (B1, B2 : BOOLEAN);  -- ERROR: NON-FAMILY
                                             --        INDEXED.
          NULL;
     END T;

BEGIN

     T.E0;                    -- OK.
     T.E0 (1);                -- ERROR: NON-FAMILY INDEXED.
     NULL;

     T.E1 (TRUE);             -- OK.
     T.E1 (1) (TRUE);         -- ERROR: NON-FAMILY INDEXED.
     NULL;

     T.E2 (TRUE,FALSE);       -- OK.
     T.E2 (1) (TRUE,FALSE);   -- ERROR: NON-FAMILY INDEXED.
     NULL;

END B95002A;
