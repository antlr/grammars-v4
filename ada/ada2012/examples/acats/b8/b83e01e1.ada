-- B83E01E1.ADA

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
--     THIS FILE CONTAINS THE SUBUNIT BODIES FOR THE B83E01E TEST.

-- HISTORY:
--     DHH 09/15/88 CREATED ORIGINAL TEST.
--     LDC 10/10/90 SPLIT PROCEDURE BODIES INTO SEPARATE FILES.

SEPARATE (B83E01E0M)
PROCEDURE B83E01E_PROC1(PARAM1, PARAM2, PARAM3, PARAM4, PARAM5,
                        PARAM6, PARAM7 : INTEGER) IS

     PARAM1 : INTEGER;                                   -- ERROR:

     PARAM2 : CONSTANT BOOLEAN := TRUE;                  -- ERROR:

     PARAM3 : CONSTANT := 5;                             -- ERROR:

     TYPE PARAM4 IS ARRAY(1 ..2) OF BOOLEAN;             -- ERROR:

     SUBTYPE PARAM5 IS INTEGER;                          -- ERROR:

     PACKAGE PARAM6 IS                                   -- ERROR:
     END;

BEGIN
    PARAM7:                                              -- ERROR:
     DECLARE
     BEGIN
          NULL;
     END PARAM7;
END B83E01E_PROC1;
