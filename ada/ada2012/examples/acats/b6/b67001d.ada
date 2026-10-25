-- B67001D.ADA

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
--     CHECK THAT CERTAIN FUNCTION DECLARATIONS WITH OPERATOR SYMBOLS
--     ARE NOT ALLOWED IN RENAMING DECLARATIONS.  IN PARTICULAR, THE
--     FOLLOWING SUBTESTS ARE PERFORMED:
--     (A)  CHECK THAT "IN", "NOT IN", "AND THEN", "OR ELSE" AND "/="
--          ARE NOT PERMITTED AS OPERATOR SYMBOLS.
--     (B)  CHECK THAT FUNCTIONS FOR "AND", "OR", "XOR", "=", "<=",
--          "<", ">=", ">", "&", "*", "/", "MOD", "REM", AND "**"
--          CANNOT BE DECLARED WITH ONE PARAMETER.
--     (C)  CHECK THAT THE SAME OPERATOR SYMBOLS AS LISTED IN (B)
--          CANNOT BE DECLARED AS FUNCTIONS WITH THREE PARAMETERS.
--     (D)  CHECK THAT FUNCTIONS FOR "NOT" AND "ABS" CANNOT BE DECLARED
--          WITH TWO PARAMETERS.
--     (E)  CHECK THAT FUNCTIONS FOR "+", "-", "NOT", AND "ABS" CANNOT
--          BE DECLARED WITH ZERO PARAMETERS.
--     (F)  CHECK THAT FUNCTIONS FOR "+" AND "-" CANNOT BE DECLARED
--          WITH THREE PARAMETERS.
--     (G)  CHECK THAT DEFAULT PARAMETERS CANNOT BE SPECIFIED FOR
--          OPERATOR SYMBOL FUNCTIONS.
--     (H)  CHECK THAT NO LEADING OR TRAILING BLANKS ARE ALLOWED.

-- HISTORY:
--     CPP 06/12/84
--     JRK 12/04/84
--     DWC 09/22/87  MOVED CHECK FOR SYMBOL ":=" TO B67001K.ADA.

PROCEDURE B67001D IS

     PACKAGE PKG IS
          TYPE LIM_PRIV IS LIMITED PRIVATE;
          FUNCTION "=" (P1, P2 : LIM_PRIV) RETURN BOOLEAN;
     PRIVATE
          TYPE LIM_PRIV IS NEW INTEGER;
     END PKG;
     USE PKG;

     PACKAGE BODY PKG IS
          FUNCTION "=" (P1, P2 : LIM_PRIV) RETURN BOOLEAN IS
          BEGIN
               RETURN TRUE;
          END "=";
     END PKG;

     FUNCTION NO_PARAMS RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END NO_PARAMS;

     FUNCTION ONE_PARAM (P1 : LIM_PRIV) RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END ONE_PARAM;

     FUNCTION INT_ONE (P1 : INTEGER) RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END INT_ONE;

     FUNCTION BOOL_ONE (P1 : BOOLEAN) RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END BOOL_ONE;

     FUNCTION TWO_PARAMS (P1, P2 : LIM_PRIV) RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END TWO_PARAMS;

     FUNCTION INT_TWO (P1, P2 : INTEGER) RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END INT_TWO;

     FUNCTION BOOL_TWO (P1, P2 : BOOLEAN) RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END BOOL_TWO;

     FUNCTION THREE_PARAMS (P1, P2, P3 : LIM_PRIV) RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END THREE_PARAMS;

     FUNCTION INT_THREE (P1, P2, P3 : INTEGER) RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END INT_THREE;

     FUNCTION BOOL_THREE (P1, P2, P3 : BOOLEAN) RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END BOOL_THREE;

BEGIN

     --------------------------------------------------

     DECLARE   -- (A)

          FUNCTION "IN" (P1, P2, P3 : INTEGER)       -- ERROR: IN.
               RETURN BOOLEAN RENAMES INT_THREE;

          FUNCTION "NOT IN" (P1, P2, P3 : INTEGER)   -- ERROR: NOT IN.
               RETURN BOOLEAN RENAMES INT_THREE;

          FUNCTION "AND THEN" (P1, P2 : BOOLEAN)     -- ERROR: AND THEN.
               RETURN BOOLEAN RENAMES BOOL_TWO;

          FUNCTION "OR ELSE" (P1, P2 : BOOLEAN)      -- ERROR: OR ELSE.
               RETURN BOOLEAN RENAMES BOOL_TWO;

          FUNCTION "/=" (P1, P2 : LIM_PRIV)          -- ERROR: /=.
               RETURN BOOLEAN RENAMES PKG."/=";

     BEGIN     -- (A)
          NULL;
     END; -- (A)

     --------------------------------------------------

     DECLARE   -- (B)

          FUNCTION "AND" (P1 : BOOLEAN)    -- ERROR: TOO FEW PARAMS.
               RETURN BOOLEAN RENAMES BOOL_ONE;

          FUNCTION "OR" (P1 : BOOLEAN)     -- ERROR: TOO FEW PARAMS.
               RETURN BOOLEAN RENAMES BOOL_ONE;

          FUNCTION "XOR" (P1 : BOOLEAN)    -- ERROR: TOO FEW PARAMS.
               RETURN BOOLEAN RENAMES BOOL_ONE;

          FUNCTION "=" (P1 : LIM_PRIV)     -- ERROR: TOO FEW PARAMS.
               RETURN BOOLEAN RENAMES ONE_PARAM;

          FUNCTION "<=" (P1 : INTEGER)     -- ERROR: TOO FEW PARAMS.
               RETURN BOOLEAN RENAMES INT_ONE;

          FUNCTION "<" (P1 : INTEGER)      -- ERROR: TOO FEW PARAMS.
               RETURN BOOLEAN RENAMES INT_ONE;

          FUNCTION ">=" (P1 : INTEGER)     -- ERROR: TOO FEW PARAMS.
               RETURN BOOLEAN RENAMES INT_ONE;

          FUNCTION ">" (P1 : INTEGER)      -- ERROR: TOO FEW PARAMS.
               RETURN BOOLEAN RENAMES INT_ONE;

          FUNCTION "&" (P1 : BOOLEAN)      -- ERROR: TOO FEW PARAMS.
               RETURN BOOLEAN RENAMES BOOL_ONE;

          FUNCTION "*" (P1 : INTEGER)      -- ERROR: TOO FEW PARAMS.
               RETURN BOOLEAN RENAMES INT_ONE;

          FUNCTION "/" (P1 : INTEGER)      -- ERROR: TOO FEW PARAMS.
               RETURN BOOLEAN RENAMES INT_ONE;

          FUNCTION "MOD" (P1 : INTEGER)    -- ERROR: TOO FEW PARAMS.
               RETURN BOOLEAN RENAMES INT_ONE;

          FUNCTION "REM" (P1 : INTEGER)    -- ERROR: TOO FEW PARAMS.
               RETURN BOOLEAN RENAMES INT_ONE;

          FUNCTION "**" (P1 : INTEGER)     -- ERROR: TOO FEW PARAMS.
               RETURN BOOLEAN RENAMES INT_ONE;

     BEGIN     -- (B)
          NULL;
     END; -- (B)

     --------------------------------------------------

     DECLARE   -- (C)

          FUNCTION "AND" (P1, P2, P3 : BOOLEAN) -- ERROR: TOO MANY
               RETURN BOOLEAN RENAMES BOOL_THREE;      -- PARAMS.

          FUNCTION "OR" (P1, P2, P3 : BOOLEAN)  -- ERROR: TOO MANY
               RETURN BOOLEAN RENAMES BOOL_THREE;      -- PARAMS.

          FUNCTION "XOR" (P1, P2, P3 : BOOLEAN) -- ERROR: TOO MANY
               RETURN BOOLEAN RENAMES BOOL_THREE;      -- PARAMS.

          FUNCTION "=" (P1, P2, P3 : LIM_PRIV)  -- ERROR: TOO MANY
               RETURN BOOLEAN RENAMES THREE_PARAMS;    -- PARAMS.

          FUNCTION "<=" (P1, P2, P3 : INTEGER)  -- ERROR: TOO MANY
               RETURN BOOLEAN RENAMES INT_THREE;       -- PARAMS.

          FUNCTION "<" (P1, P2, P3 : INTEGER)   -- ERROR: TOO MANY
               RETURN BOOLEAN RENAMES INT_THREE;       -- PARAMS.

          FUNCTION ">=" (P1, P2, P3 : INTEGER)  -- ERROR: TOO MANY
               RETURN BOOLEAN RENAMES INT_THREE;       -- PARAMS.

          FUNCTION ">" (P1, P2, P3 : INTEGER)   -- ERROR: TOO MANY
               RETURN BOOLEAN RENAMES INT_THREE;       -- PARAMS.

          FUNCTION "&" (P1, P2, P3 : BOOLEAN)   -- ERROR: TOO MANY
               RETURN BOOLEAN RENAMES BOOL_THREE;      -- PARAMS.

          FUNCTION "*" (P1, P2, P3 : INTEGER)   -- ERROR: TOO MANY
               RETURN BOOLEAN RENAMES INT_THREE;       -- PARAMS.

          FUNCTION "/" (P1, P2, P3 : INTEGER)   -- ERROR: TOO MANY
               RETURN BOOLEAN RENAMES INT_THREE;       -- PARAMS.

          FUNCTION "MOD" (P1, P2, P3 : INTEGER) -- ERROR: TOO MANY
               RETURN BOOLEAN RENAMES INT_THREE;       -- PARAMS.

          FUNCTION "REM" (P1, P2, P3 : INTEGER) -- ERROR: TOO MANY
               RETURN BOOLEAN RENAMES INT_THREE;       -- PARAMS.

          FUNCTION "**" (P1, P2, P3 : INTEGER)  -- ERROR: TOO MANY
               RETURN BOOLEAN RENAMES INT_THREE;       -- PARAMS.

     BEGIN     -- (C)
          NULL;
     END; -- (C)

     --------------------------------------------------

     DECLARE   -- (D)

          FUNCTION "NOT" (P1, P2 : BOOLEAN)     -- ERROR: TOO MANY
               RETURN BOOLEAN RENAMES BOOL_TWO;        -- PARAMS.

          FUNCTION "ABS" (P1, P2 : INTEGER)     -- ERROR: TOO MANY
               RETURN BOOLEAN RENAMES INT_TWO;         -- PARAMS.

     BEGIN     -- (D)
          NULL;
     END; -- (D)

     --------------------------------------------------

     DECLARE   -- (E)

          FUNCTION "+" RETURN BOOLEAN         -- ERROR: TOO FEW PARAMS.
               RENAMES NO_PARAMS;

          FUNCTION "-" RETURN BOOLEAN         -- ERROR: TOO FEW PARAMS.
               RENAMES NO_PARAMS;

          FUNCTION "NOT" RETURN BOOLEAN       -- ERROR: TOO FEW PARAMS.
               RENAMES NO_PARAMS;

          FUNCTION "ABS" RETURN BOOLEAN       -- ERROR: TOO FEW PARAMS.
               RENAMES NO_PARAMS;

     BEGIN     -- (E)
          NULL;
     END; -- (E)

     --------------------------------------------------

     DECLARE   -- (F)

          FUNCTION "+" (P1, P2, P3 : INTEGER)   -- ERROR: TOO MANY
               RETURN BOOLEAN RENAMES INT_THREE;       -- PARAMS.

          FUNCTION "-" (P1, P2, P3 : INTEGER)   -- ERROR: TOO MANY
               RETURN BOOLEAN RENAMES INT_THREE;       -- PARAMS.

     BEGIN     -- (F)
          NULL;
     END; -- (F)

     --------------------------------------------------

     DECLARE   -- (G)

          FUNCTION "+" (P1, P2 : INTEGER := 0)  -- ERROR: DEFAULT.
               RETURN BOOLEAN RENAMES INT_TWO;

     BEGIN     -- (G)
          NULL;
     END; -- (G)

     --------------------------------------------------

     DECLARE   -- (H)

          FUNCTION " +" (P1, P2 : INTEGER)      -- ERROR: LEADING BLANK.
               RETURN BOOLEAN RENAMES INT_TWO;

          FUNCTION "+ " (P1, P2 : INTEGER)      -- ERROR: TRLG BLANK.
               RETURN BOOLEAN RENAMES INT_TWO;

          FUNCTION "NOT " (P1, P2 : BOOLEAN)    -- ERROR: TRLG BLANK.
               RETURN BOOLEAN RENAMES BOOL_TWO;

          FUNCTION " NOT" (P1, P2 : BOOLEAN)    -- ERROR: LEADING BLANK.
               RETURN BOOLEAN RENAMES BOOL_TWO;

     BEGIN     -- (H)
          NULL;
     END; -- (H)

     --------------------------------------------------

END B67001D;
