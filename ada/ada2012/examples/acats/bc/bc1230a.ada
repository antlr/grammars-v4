-- BC1230A.ADA

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
-- CHECK THAT THE SCOPE OF THE IDENTIFIER OF A FORMAL TYPE STARTS
-- AT THE POINT WHERE THIS IDENTIFIER FIRST OCCURS IN THE GENERIC
-- PARAMETER DECLARATION.

-- KEI 3/4/82
-- RJK 1/24/84      ADDED TO ACVC
-- TBN 11/25/85     RENAMED FROM BC12ABA-B.ADA.  ADDED AN EXTERNAL
--                  TYPE DECLARATION WITH THE SAME IDENTIFIER.

PROCEDURE BC1230A IS

     TYPE INT IS (ONE, TWO, THREE);

     GENERIC

          INT_1 : INT := ONE;              -- OK.
          TYPE INT IS RANGE <>;            -- OK.
          INT_2 : INT := ONE;              -- ERROR:
          INT_3 : INT := 1;                -- OK.

          I : INTEGER := 10;               -- OK.

          TYPE ARG IS ARRAY (NATURAL RANGE <>) OF NUMBER;  -- ERROR:
          OBJ_ENUM : ENUM;                                 -- ERROR:
          N : NUMBER := 3;                                 -- ERROR:

          TYPE NUMBER IS RANGE <>;         -- OK.
          TYPE ENUM IS (<>);               -- OK.

          PARAM : ENUM;                    -- OK.

     PACKAGE PACK IS
     END PACK;

BEGIN
     NULL;
END BC1230A;
