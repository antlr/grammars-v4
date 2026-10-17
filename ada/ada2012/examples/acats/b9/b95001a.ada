-- B95001A.ADA

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
-- OBJECTIVE:
--     CHECK THAT THE NAME OF AN ENTRY FAMILY MUST BE SPECIFIED AS A
--     SINGLY INDEXED COMPONENT IN AN ENTRY CALL OR IN AN ACCEPT
--     STATEMENT.

-- HISTORY:
--     JRK 10/22/81
--     JWC 06/28/85  RENAMED TO -AB
--     DWC 09/22/87  MOVED SOME OF THE TESTS TO B95001C.ADA.

PROCEDURE B95001A IS

     SUBTYPE INT IS INTEGER RANGE 1..5;

     TASK T IS
          ENTRY E0 (INT);
          ENTRY E1 (INT) (B : BOOLEAN);
          ENTRY E2 (INT) (B1, B2 : BOOLEAN);
     END T;

     TASK BODY T IS
     BEGIN
          ACCEPT E0 (1);                     -- OK.
          ACCEPT E0;                         -- ERROR: FAMILY REQUIRES
                                             --        1 INDEX.
          NULL;

          ACCEPT E1 (1) (B : BOOLEAN);       -- OK.
          ACCEPT E1 (B : BOOLEAN);           -- ERROR: FAMILY REQUIRES
                                             --        1 INDEX.
          NULL;

          ACCEPT E2 (1) (B1, B2 : BOOLEAN);  -- OK.
          ACCEPT E2 (B1, B2 : BOOLEAN);      -- ERROR: FAMILY REQUIRES
                                             --        1 INDEX.
          NULL;
     END T;

BEGIN

     T.E0 (1);                     -- OK.
     T.E0;                         -- ERROR: FAMILY REQUIRES 1 INDEX.
     NULL;
     T.E0 (1,2);                   -- ERROR: FAMILY REQUIRES 1 INDEX.
     NULL;
     T.E0 (1..2);                  -- ERROR: FAMILY SLICE PROHIBITED.
     NULL;

     T.E1 (1) (TRUE);              -- OK.
     T.E1 (TRUE);                  -- ERROR: FAMILY REQUIRES 1 INDEX.
     NULL;
     T.E1 (1,2) (TRUE);            -- ERROR: FAMILY REQUIRES 1 INDEX.
     NULL;
     T.E1 (1..2) (TRUE);           -- ERROR: FAMILY SLICE PROHIBITED.
     NULL;

     T.E2 (1) (TRUE,FALSE);        -- OK.
     T.E2 (TRUE,FALSE);            -- ERROR: FAMILY REQUIRES 1 INDEX.
     NULL;
     T.E2 (1,2) (TRUE,FALSE);      -- ERROR: FAMILY REQUIRES 1 INDEX.
     NULL;
     T.E2 (1..2) (TRUE,FALSE);     -- ERROR: FAMILY SLICE PROHIBITED.
     NULL;

END B95001A;
