 -- B49009C.ADA
 
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
 -- CHECK THAT CERTAIN ATTRIBUTES ARE NOT ALLOWED IN STATIC EXPRESSIONS
 -- IF THEIR PREFIXES DENOTE NONSTATIC SUBTYPES.
 -- CHECK THE FOLLOWING: 'AFT, 'DELTA, 'DIGITS, 'EMAX, 'EPSILON, 'FIRST,
 -- 'FORE, 'LARGE, 'LAST, 'MACHINE_EMAX, 'MACHINE_EMIN, 
 -- 'MACHINE_MANTISSA, 'MACHINE_OVERFLOWS, 'MACHINE_RADIX,
 -- 'MACHINE_ROUNDS, 'MANTISSA, 'SAFE_EMAX, 'SAFE_LARGE, 'SAFE_SMALL,
 -- 'SIZE, 'SMALL, AND 'WIDTH.
 
 -- L.BROWN  09/02/86
 -- KAS 11/16/95 UPDATE TO ADA95; REMOVE REFERENCES TO OBSOLESCENT/CHANGED
 --              ATTRIBUTES
 -- KAS 11/29/95 ELIMINATED USE OF 'EPSILON
 
 PROCEDURE  B49009C  IS
 
 BEGIN
      DECLARE
           TYPE FLT IS DIGITS 4 RANGE 0.1 .. 25.0;
           TYPE FIX IS DELTA 0.125 RANGE 0.1 .. 255.0;
           OBJ1 : FIX := 20.0;
           SUBTYPE SUFIX IS FIX RANGE 0.1 .. OBJ1;
           TYPE INT1 IS RANGE 1 .. SUFIX'AFT;            -- ERROR:'AFT.
           I1 : INTEGER;
           TYPE FIX1 IS DELTA SUFIX'DELTA                -- ERROR:'DELTA.
                                    RANGE 0.1 .. 14.0;
           I2 : INTEGER;
           TYPE ARR IS ARRAY(1 .. 4) OF BOOLEAN;
           OBJ2 : FLT := 10.0;
           SUBTYPE SUFLT IS FLT RANGE 0.1 .. OBJ2;
           AR_OBJ : ARR := (1 => FALSE,
                            2 .. SUFLT'DIGITS => TRUE);  -- ERROR:
                                                         -- 'DIGITS.
           I3 : INTEGER;
           I4 : INTEGER;
           I5 : INTEGER;
           TYPE FIX3 IS DELTA SUFIX'FIRST                -- ERROR:'FIRST.
                        RANGE 0.1 .. 12.0;
           I6 : INTEGER;
           TYPE INT2 IS RANGE 1 .. SUFIX'FORE;           -- ERROR:'FORE.
           I7 : INTEGER;
           I8 : INTEGER;
           CON1 : CONSTANT := SUFLT'MACHINE_EMAX;        -- ERROR:
                                                         --'MACHINE_EMAX.
           I9 : INTEGER;
           TYPE FLT1 IS DIGITS SUFLT'MACHINE_EMIN;       -- ERROR:
                                                         --'MACHINE_EMIN.
           I10 : INTEGER;
           TYPE INT3 IS
                 RANGE 1 .. SUFLT'MACHINE_MANTISSA;      -- ERROR:
                                                     --'MACHINE_MANTISSA.
           OBJ_BO : BOOLEAN := TRUE;
           TYPE INT4 IS RANGE 1 .. SUFLT'MACHINE_RADIX;  -- ERROR:
                                                        --'MACHINE_RADIX.
           I11 : INTEGER;
           I12 : INTEGER;
           I13 : INTEGER;
           I14 : INTEGER;
           I15 : INTEGER;
           TYPE FLT3 IS DIGITS SUFLT'SIZE;               -- ERROR:'SIZE.
           I16 : INTEGER;
           TYPE FIX7 IS DELTA SUFIX'SMALL                -- ERROR:'SMALL.
                                    RANGE 0.1 .. 15.0;
           TYPE ENUM IS (RED,YELLOW,GREEN,BLUE);
           OBJ_EN : ENUM := GREEN;
           SUBTYPE SUENUM IS ENUM RANGE RED .. OBJ_EN;
           TYPE FLT4 IS DIGITS SUENUM'WIDTH;             -- ERROR:'WIDTH.
      BEGIN
           CASE OBJ_EN IS
                WHEN SUENUM'LAST =>                      -- ERROR:'LAST.
                     OBJ1 := 10.0;
                WHEN OTHERS =>
                     NULL;
           END CASE;
 
           CASE OBJ_BO IS
                WHEN SUFIX'MACHINE_OVERFLOWS =>          -- ERROR:
                     OBJ2 := 5.0;                   --'MACHINE_OVERFLOWS.
                WHEN OTHERS =>
                     NULL;
           END CASE;
 
           CASE OBJ_BO IS
                WHEN SUFLT'MACHINE_ROUNDS =>             -- ERROR:
                     OBJ1 := 10.0;                     --'MACHINE_ROUNDS.
                WHEN OTHERS =>
                     NULL;
           END CASE;
      END;
 
 END B49009C;
 
