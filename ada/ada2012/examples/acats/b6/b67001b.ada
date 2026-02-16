-- B67001B.ADA

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
--     ARE NOT ALLOWED AS GENERIC FORMAL SUBPROGRAM PARAMETERS.
--     IN PARTICULAR, THE FOLLOWING SUBTESTS ARE PERFORMED:
--     (A)  CHECK THAT "IN", "NOT IN", "AND THEN",
--          "OR ELSE", AND "/=" ARE NOT
--          PERMITTED AS OPERATOR SYMBOLS.
--     (B)  CHECK THAT FUNCTIONS FOR "AND", "OR",
--          "XOR", "=", "<=", "<", ">=", ">", "&",
--          "*", "/", "MOD", "REM", AND "**" CANNOT
--          BE DECLARED WITH ONE PARAMETER.
--     (C)  CHECK THAT THE SAME OPERATOR SYMBOLS
--          AS LISTED IN (B) CANNOT BE DECLARED
--          AS FUNCTIONS WITH THREE PARAMETERS.
--     (D)  CHECK THAT FUNCTIONS FOR "NOT" AND "ABS" CANNOT
--          BE DECLARED WITH TWO PARAMETERS.
--     (E)  CHECK THAT FUNCTIONS FOR "+", "-", "NOT", AND "ABS" CANNOT
--          BE DECLARED WITH ZERO PARAMETERS.
--     (F)  CHECK THAT FUNCTIONS FOR "+" AND "-"
--          CANNOT BE DECLARED WITH THREE PARAMETERS.
--     (G)  CHECK THAT DEFAULT PARAMETERS CANNOT
--          BE SPECIFIED FOR OPERATOR SYMBOL FUNCTIONS.
--     (H)  CHECK THAT NO LEADING OR TRAILING BLANKS ARE ALLOWED.

-- HISTORY::
--     CPP 06/11/84
--     JRK 12/04/84
--     DWC 09/22/87  MOVED CHECK FOR THE SYMBOL ":=" TO B67001I.ADA.

PROCEDURE B67001B IS
     PACKAGE P IS
          TYPE LIM_PRIV IS LIMITED PRIVATE;
     PRIVATE
          TYPE LIM_PRIV IS NEW INTEGER;
     END P;
     USE P;
BEGIN

     --------------------------------------------------

     DECLARE -- (A)
          GENERIC

               WITH FUNCTION "IN" (I1, I2, I3 : INTEGER)   -- ERROR: IN
                    RETURN BOOLEAN;

               WITH FUNCTION "NOT IN"                      -- ERROR:
                    (I1, I2, I3 : INTEGER) RETURN BOOLEAN; -- NOT IN.

               WITH FUNCTION "AND THEN" (B1, B2 : BOOLEAN) -- ERROR:
                    RETURN BOOLEAN;                        -- AND THEN.

               WITH FUNCTION "OR ELSE" (B1, B2 : BOOLEAN)  -- ERROR:
                    RETURN BOOLEAN;                        -- OR ELSE.

               WITH FUNCTION "/=" (I1, I2: LIM_PRIV)       -- ERROR: /=
                    RETURN BOOLEAN;

          PACKAGE PKG IS
          END PKG;

     BEGIN -- (A)
          NULL;
     END; -- (A)

     --------------------------------------------------

     DECLARE -- (B)
          GENERIC

               WITH FUNCTION "AND" (B1 : BOOLEAN)          -- ERROR: TOO
                                                           -- FEW ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "OR" (B1 : BOOLEAN)           -- ERROR: TOO
                                                           -- FEW ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "XOR" (B1 : BOOLEAN)          -- ERROR: TOO
                                                           -- FEW ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "=" (B1 : LIM_PRIV)           -- ERROR: TOO
                                                           -- FEW ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "<=" (B1 : BOOLEAN)           -- ERROR: TOO
                                                           -- FEW ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "<" (B1 : BOOLEAN)            -- ERROR: TOO
                                                           -- FEW ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION ">=" (B1 : BOOLEAN)           -- ERROR: TOO
                                                           -- FEW ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION ">" (B1 : BOOLEAN)            -- ERROR: TOO
                                                           -- FEW ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "&" (B1 : BOOLEAN)            -- ERROR: TOO
                                                           -- FEW ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "*" (B1 : BOOLEAN)            -- ERROR: TOO
                                                           -- FEW ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "/" (B1 : BOOLEAN)            -- ERROR: TOO
                                                           -- FEW ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "MOD" (B1 : BOOLEAN)          -- ERROR: TOO
                                                           -- FEW ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "REM" (B1 : BOOLEAN)          -- ERROR: TOO
                                                           -- FEW ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "**" (B1 : BOOLEAN)           -- ERROR: TOO
                                                           -- FEW ARGS.
                    RETURN BOOLEAN;

          PACKAGE PKG1 IS
          END PKG1;

     BEGIN -- (B)
          NULL;
     END; -- (B)

     --------------------------------------------------

     DECLARE -- (C)
          GENERIC

               WITH FUNCTION "AND" (B1, B2, B3 : BOOLEAN)  -- ERROR: TOO
                                                           -- MANY ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "OR" (B1, B2, B3: BOOLEAN)    -- ERROR: TOO
                                                           -- MANY ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "XOR" (B1, B2, B3 : BOOLEAN)  -- ERROR: TOO
                                                           -- MANY ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "=" (B1, B2, B3 : LIM_PRIV)   -- ERROR: TOO
                                                           -- MANY ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "<=" (B1, B2, B3 : BOOLEAN)   -- ERROR: TOO
                                                           -- MANY ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "<" (B1, B2, B3 : BOOLEAN)    -- ERROR: TOO
                                                           -- MANY ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION ">=" (B1, B2, B3 : BOOLEAN)   -- ERROR: TOO
                                                           -- MANY ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION ">" (B1, B2, B3 : BOOLEAN)    -- ERROR: TOO
                                                           -- MANY ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "&" (B1, B2, B3 : BOOLEAN)    -- ERROR: TOO
                                                           -- MANY ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "*" (B1, B2, B3 : BOOLEAN)    -- ERROR: TOO
                                                           -- MANY ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "/" (B1, B2, B3 : BOOLEAN)    -- ERROR: TOO
                                                           -- MANY ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "MOD" (B1, B2, B3 : BOOLEAN)  -- ERROR: TOO
                                                           -- MANY ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "REM" (B1, B2, B3 : BOOLEAN)  -- ERROR: TOO
                                                           -- MANY ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "**" (B1, B2, B3 : BOOLEAN)   -- ERROR: TOO
                                                           -- MANY ARGS.
                    RETURN BOOLEAN;

          PACKAGE PKG2 IS
          END PKG2;

     BEGIN -- (C)
          NULL;
     END; -- (C)

     ---------------------------------------------------

     DECLARE -- (D)

          GENERIC

               WITH FUNCTION "NOT" (B1, B2 : BOOLEAN)      -- ERROR: TOO
                                                           -- MANY ARGS.
                    RETURN BOOLEAN;

               WITH FUNCTION "ABS" (I1, I2 : INTEGER)      -- ERROR: TOO
                                                           -- MANY ARGS.
                    RETURN INTEGER;

          PACKAGE PKG4 IS
          END PKG4;

     BEGIN -- (D)
          NULL;
     END; -- (D)

     --------------------------------------------------

     DECLARE -- (E)

          GENERIC

               WITH FUNCTION "+" RETURN INTEGER;           -- ERROR: TOO
                                                           -- FEW ARGS.

               WITH FUNCTION "-" RETURN INTEGER;           -- ERROR: TOO
                                                           -- FEW ARGS.

               WITH FUNCTION "NOT" RETURN BOOLEAN;         -- ERROR: TOO
                                                           -- FEW ARGS.

               WITH FUNCTION "ABS" RETURN INTEGER;         -- ERROR: TOO
                                                           -- FEW ARGS.

          PACKAGE PKG5 IS
          END PKG5;

     BEGIN -- (E)
          NULL;
     END; -- (E)

     --------------------------------------------------

     DECLARE -- (F)

          GENERIC

               WITH FUNCTION "+" (I1, I2, I3 : INTEGER)    -- ERROR: TOO
                                                           -- MANY ARGS.
                    RETURN INTEGER;

               WITH FUNCTION "-" (I1, I2, I3 : INTEGER)    -- ERROR: TOO
                                                           -- MANY ARGS.
                     RETURN INTEGER;

          PACKAGE PKG6 IS
          END PKG6;

     BEGIN -- (F)
          NULL;
     END; -- (F)

     --------------------------------------------------

     DECLARE -- (G)

          GENERIC

               WITH FUNCTION "+" (I1, I2: INTEGER := 0)      -- ERROR:
                    RETURN INTEGER;                          -- DEFAULT.

          PACKAGE PKG7 IS
          END PKG7;

     BEGIN -- (G)
          NULL;
     END; -- (G)

     --------------------------------------------------

     DECLARE -- (H)

          GENERIC

               WITH FUNCTION " +" (I1, I2: INTEGER)      -- ERROR:
                    RETURN INTEGER;                      -- LEADING BLK.

               WITH FUNCTION "+ " (I1, I2: INTEGER)      -- ERROR:
                    RETURN INTEGER;                      -- TRAILG BLK.

               WITH FUNCTION "NOT " (B1, B2: BOOLEAN)    -- ERROR:
                    RETURN BOOLEAN;                      -- TRAILG BLK.

               WITH FUNCTION " NOT" (B1, B2: BOOLEAN)    -- ERROR:
                    RETURN BOOLEAN;                      -- LEADING BLK.

          PACKAGE PKG8 IS
          END PKG8;

     BEGIN -- (H)
          NULL;
     END; -- (H)

END B67001B;
