-- B83001A.ADA

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
--     CHECK THAT TWO NONOVERLOADABLE DECLARATIONS, GIVEN IN THE
--     DECLARATIVE PART OF A SUBPROGRAM'S BODY, CANNOT BE HOMOGRAPHS.
--     IN PARTICULAR, CHECK DECLARATIONS FOR VARIABLES, CONSTANTS, NAMED
--     NUMBERS, EXCEPTIONS, TYPES, SUBTYPES, PACKAGES, TASK UNITS, AND
--     GENERIC UNITS.

-- HISTORY:
--     VCL  02/02/88  CREATED ORIGINAL TEST.
--     RLB  04/26/21  Corrected incorrect error markers; moved markers on wrong
--                    lines; added location indicators.
--!

PROCEDURE B83001A IS

-- MULTIPLE SUBPROGRAMS ARE USED SO THAT ONLY ONE DECLARATION WHICH
-- REQUIRES A BODY (TASK AND GENERIC UNITS) IS GIVEN IN EACH SUBPROGRAM.

     PROCEDURE A IS
          TYPE D1 IS RANGE 1..10;

     --  DECLARATIONS OF HOMOGRAPHS.

          TYPE D1 IS ARRAY (1..15) OF CHARACTER;  -- ERROR: HOMOGRAPH. {11;1}
          SUBTYPE D1 IS STRING(1..12);            -- ERROR: HOMOGRAPH. {11;1}
          D1 : CONSTANT CHARACTER := 'A';         -- ERROR: HOMOGRAPH. {11;1}
          D1 : CONSTANT := 5.0;                   -- ERROR: HOMOGRAPH. {11;1}
          D1 : STRING(1..10);                     -- ERROR: HOMOGRAPH. {11;1}
          D1 : EXCEPTION;                         -- ERROR: HOMOGRAPH. {11;1}

          PACKAGE D1 IS END D1;                   -- ERROR: HOMOGRAPH. {11;1}

          TASK D1;                                -- ERROR: HOMOGRAPH. {11;1}

     -- BODY OF HOMOGRAPH.

          TASK BODY D1 IS                         -- OPTIONAL ERROR: {11}
          BEGIN                                   --  BODY OF AN INVALID
               NULL;                              --  TASK OBJECT.
          END D1;


     BEGIN
          NULL;
     END A;


     FUNCTION B RETURN BOOLEAN IS
          SUBTYPE D2 IS INTEGER RANGE 1..10;

     --  DECLARATION OF HOMOGRAPH.

          GENERIC                               
          PACKAGE D2 IS END D2;                   -- ERROR: HOMOGRAPH. {1:11;1}

          TASK TYPE D2;                           -- ERROR: HOMOGRAPH. {11;1}

     -- BODY OF HOMOGRAPH.

          TASK BODY D2 IS                         -- OPTIONAL ERROR: {11}
          BEGIN                                   --  BODY OF AN INVALID
               NULL;                              --  TASK TYPE.
          END D2;


     BEGIN
          RETURN FALSE;
     END B;


     PROCEDURE C IS
          D3 : CONSTANT INTEGER := 10;

     --  DECLARATION OF HOMOGRAPH.

          GENERIC                       
          PROCEDURE D3;                           -- ERROR: HOMOGRAPH. {1:11;1}

     -- BODY OF HOMOGRAPH.

          PROCEDURE D3 IS                         -- OPTIONAL ERROR: {11}
          BEGIN                                   --  BODY OF AN INVALID
               NULL;                              --  GENERIC PROCEDURE.
          END D3;


     BEGIN
          NULL;
     END C;


     FUNCTION D RETURN BOOLEAN IS
          D4 : CONSTANT := 8;

     --  DECLARATION OF HOMOGRAPH.

          GENERIC                                   
          FUNCTION D4 RETURN STRING;              -- ERROR: HOMOGRAPH. {1:11;1}

     -- BODY OF HOMOGRAPH.

          FUNCTION D4 RETURN STRING IS            -- OPTIONAL ERROR: {11}
          BEGIN                                   --  BODY OF AN INVALID
               RETURN "D4";                       --  GENERIC FUNCTION.
          END D4;

     BEGIN
          RETURN FALSE;
     END D;

BEGIN
     NULL;
END B83001A;
