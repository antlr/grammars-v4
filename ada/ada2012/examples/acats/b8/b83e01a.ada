-- B83E01A.ADA

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
--     CHECK THAT NON-GENERIC SUBPROGRAM SPECIFICATIONS CANNOT DECLARE
--     DUPLICATE FORMAL PARAMETERS. INCLUDE DECLARATIONS WITH LATER
--     BODIES, BODIES WITHOUT EARLIER DECLARATIONS, STUBS, AND RENAMING
--     DECLARATIONS.

-- HISTORY:
--     DHH 09/08/88  CREATED ORIGINAL TEST.

PROCEDURE B83E01A IS

     FUNCTION F(PARAM1 : BOOLEAN;
                PARAM1 : INTEGER) RETURN BOOLEAN;   -- ERROR: DUPLICATE.

     PROCEDURE P(PARAM1 : IN BOOLEAN;
                 PARAM1 : OUT INTEGER);             -- ERROR: DUPLICATE.

     PROCEDURE P1(PARAM1 : BOOLEAN;
                  PARAM2 : INTEGER);

     PROCEDURE NEW_P(PARAM1 : BOOLEAN;
                     PARAM1 : INTEGER) RENAMES P1;  -- ERROR: DUPLICATE.

     FUNCTION MULT(X : POSITIVE; Y : INTEGER) RETURN INTEGER;
     FUNCTION "*"(X : POSITIVE;
                  X : INTEGER)                      -- ERROR: DUPLICATE.
                               RETURN INTEGER RENAMES MULT;

     PROCEDURE P2(PARAM1 : IN BOOLEAN;
                  PARAM1 : OUT INTEGER)             -- ERROR: DUPLICATE.
                                   IS SEPARATE;

     FUNCTION F2(PARAM1 : BOOLEAN;
                 PARAM1 : BOOLEAN)                  -- ERROR: DUPLICATE.
                                   RETURN BOOLEAN IS SEPARATE;

     FUNCTION F(PARAM1 : BOOLEAN;
                PARAM1 : INTEGER) RETURN BOOLEAN IS -- ERROR: DUPLICATE.
     BEGIN
          RETURN TRUE;
     END F;

     PROCEDURE P(PARAM1 : IN BOOLEAN;
                 PARAM1 : OUT INTEGER) IS           -- ERROR: DUPLICATE.
     BEGIN
          NULL;
     END P;

     PROCEDURE P1(PARAM1 : BOOLEAN;
                  PARAM2 : INTEGER) IS
     BEGIN
          NULL;
     END P1;

     PROCEDURE A(PARAM1 : BOOLEAN;
                 PARAM1 : INTEGER;                  -- ERROR: DUPLICATE.
                 PARAM2 : CHARACTER;
                 PARAM1 : STRING;                   -- ERROR: DUPLICATE.
                 PARAM3 : POSITIVE;
                 PARAM1 : NATURAL;                  -- ERROR: DUPLICATE.
                 PARAM1 : BOOLEAN)IS                -- ERROR: DUPLICATE.
     BEGIN
          NULL;
     END A;

     FUNCTION B(PARAM1 : BOOLEAN;
                PARAM1 : STRING) RETURN BOOLEAN IS  -- ERROR: DUPLICATE.
     BEGIN
          RETURN TRUE;
     END;

     FUNCTION MULT(X : POSITIVE; Y : INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN X * Y;
     END;

BEGIN
     NULL;
END B83E01A;
