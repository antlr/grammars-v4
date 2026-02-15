-- B67004A.ADA

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
-- CHECK THAT RENAMING DECLARATIONS OF "=" OBEY THE RULES.
-- PARTICULARLY, TESTS ARE:
--   (E) WHEN "=" IS A GENERIC FORMAL PARAMETER, THE DEFAULT NAME NEED
--       NOT BE THE EQUALITY OPERATOR.

-- SPS 2/22/84
-- CPP 6/27/84
-- JBG 5/29/85
-- DTN 11/19/91     DELETED SUBPARTS (A-D).

PROCEDURE B67004A IS

     PACKAGE PACK IS
          TYPE T IS LIMITED PRIVATE;
          FUNCTION "=" (X, Y: T) RETURN BOOLEAN;
          FUNCTION F (X, Y: T) RETURN BOOLEAN;
          FUNCTION "/" (X, Y: T) RETURN BOOLEAN;
     PRIVATE
          TYPE T IS RANGE 1 .. 10;
     END PACK;

     PACKAGE BODY PACK IS 
          FUNCTION "=" (X, Y: T) RETURN BOOLEAN IS
          BEGIN
               RETURN X = Y;
          END "=";

          FUNCTION F (X, Y: T) RETURN BOOLEAN IS
          BEGIN
               RETURN X = Y;
          END F;

          FUNCTION "/" (X, Y: T) RETURN BOOLEAN IS
          BEGIN
               RETURN X = Y;
          END "/";
     END PACK;
     USE PACK;

BEGIN

     DECLARE -- (E)
          FUNCTION FN (X, Y, Z: CHARACTER) RETURN CHARACTER IS
          BEGIN
               RETURN Z;
          END FN;

          GENERIC
               WITH FUNCTION "=" (X, Y: T) RETURN BOOLEAN
                    IS <>;                        -- OK.
          PACKAGE PKG1 IS
          END PKG1;

          PACKAGE PKG1_INST IS NEW PKG1;          -- OK.

          GENERIC
               WITH FUNCTION "=" (X, Y: T) RETURN BOOLEAN
                    IS PACK."/=";                      -- OK.
          PACKAGE PKG2 IS
          END PKG2;

          GENERIC
               WITH FUNCTION "=" (X, Y: T) RETURN BOOLEAN
                    IS PACK.F;                    -- OK.
          PACKAGE PKG3 IS
          END PKG3;

          PACKAGE NP3 IS NEW PKG3;                -- OK.

          GENERIC
               WITH FUNCTION "=" (X, Y: T) RETURN BOOLEAN
                    IS "*";                       -- ERROR:
                                                  -- WRONG PROFILE.
          PACKAGE PKG4 IS
          END PKG4;

          GENERIC
               WITH FUNCTION "=" (X, Y: T) RETURN BOOLEAN
                    IS "OR";                      -- ERROR:
                                                  -- WRONG PROFILE.
          PACKAGE PKG5 IS
          END PKG5;

          GENERIC
               WITH FUNCTION "=" (X, Y: T) RETURN BOOLEAN
                    IS FN;                        -- ERROR:
                                                  -- WRONG PROFILE.
          PACKAGE PKG6 IS
          END PKG6;

     BEGIN -- (E)
          NULL;
     END; -- (E)

     ----------------------------------------------------

END B67004A;
