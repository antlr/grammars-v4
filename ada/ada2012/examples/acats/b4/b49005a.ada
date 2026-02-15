-- B49005A.ADA

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
--     CHECK THAT A STATIC EXPRESSION MUST NOT CONTAIN A NAME DENOTING A
--     VARIABLE OR A COMPONENT OF AN ARRAY OR RECORD.
--
-- CHANGE HISTORY:
--      24 Feb 1986   RJW   Created original test.
--      31 Aug 1988   SDA   Revised code so that there is only one error per
--                          aggregate.
--      23 Apr 2021   RLB   Added error location indicators.
--!

PROCEDURE B49005A IS

BEGIN
     DECLARE

          SUBTYPE SI IS INTEGER RANGE 0 .. 10;
          V : SI := 5;

          TYPE ARR IS ARRAY (1 .. 5) OF SI;
          CARR : CONSTANT ARR := (1, 2, 3, 4, 5);

          TYPE INT_REC IS
               RECORD
                    I : SI;
               END RECORD;

          REC : CONSTANT INT_REC := (I => 3);

          TYPE INT IS RANGE 1 .. V;        -- ERROR: VARIABLE. {11;1}
          TYPE INT2 IS RANGE REC.I .. 5;   -- ERROR: RECORD COMPONENT. {11;1}
          TYPE INT3 IS RANGE CARR (1) .. 5;   -- ERROR: ARRAY          {11;1}
                                              --        COMPONENT.

          NUM  : CONSTANT := SI'POS (V);           -- ERROR: VARIABLE. {11;1}
          NUM2 : CONSTANT := SI'POS (CARR (1));    -- ERROR: ARRAY     {11;1}
                                                   --        COMPONENT.
          NUM3 : CONSTANT := SI'POS (REC.I);       -- ERROR: RECORD    {11;1}  
                                                   --        COMPONENT.

          TYPE OK_RANGE IS RANGE 1 .. 10;
          FOR OK_RANGE'SIZE USE V;                 -- ERROR: VARIABLE. {11;1}

          TYPE INT_REC2 (I : INTEGER) IS
               RECORD
                    CASE I IS
                         WHEN V        =>      -- ERROR: VARIABLE.    {26}  
                              NULL;
                         WHEN REC.I    =>      -- ERROR: RECORD       {26}
                                               --        COMPONENT.
                              NULL;
                         WHEN CARR (1) =>      -- ERROR: ARRAY        {26}
                                               --        COMPONENT.
                              NULL;
                         WHEN OTHERS   =>
                              NULL;
                    END CASE;
               END RECORD;

          TYPE OK_REC (I : INTEGER) IS
               RECORD
                    CASE I IS
                         WHEN 1  =>
                              I1 : INTEGER;
                         WHEN OTHERS =>
                              NULL;
                    END CASE;
               END RECORD;

          I2 : OK_REC (1) := (V, 0);              -- ERROR: VARIABLE. {30;1}

          ARR1 : ARR := ARR' (V          => 1,    -- ERROR: VARIABLE. {25;-1:1}
                              OTHERS     => 0);
          ARR2 : ARR := ARR' (CARR (1)   => 2,    -- ERROR: ARRAY     {25;-2:1}
                                                  --        COMPONENT.
                              OTHERS     => 0);
          ARR3 : ARR := ARR' (REC.I      => 3,    -- ERROR: RECORD    {25;-2:1}
                                                  --        COMPONENT.
                              OTHERS     => 0);

          TYPE MEDIUM IS RANGE 0 .. 1000;
          FOR MEDIUM'SIZE USE CARR (1) * 10;      -- ERROR: ARRAY     {11;1}
                                                  --        COMPONENT.

          TYPE SHORT IS DELTA 0.1 RANGE -100.0 .. 100.0;
          FOR SHORT'SIZE USE 4 * REC.I;          -- ERROR: RECORD     {11;1}
                                                 --        COMPONENT.
          F : SHORT := 0.01;

          SUBTYPE SUBSHORT IS SHORT DELTA F;      -- ERROR: VARIABLE. {11;1}

          TYPE REAL IS DIGITS CARR (1) RANGE -1.0 .. 1.0; -- ERROR:   {11;1}
                                                          -- ARRAY
                                                          -- COMPONENT.

          TYPE OK_TYPE IS (A, B);
          FOR OK_TYPE USE (A => 1, B => V);       -- ERROR: VARIABLE. {11;1}

          I : INTEGER;

     BEGIN
          CASE I IS
               WHEN V =>                -- ERROR: VARIABLE.    {16}
                    NULL;
               WHEN REC.I =>            -- ERROR: RECORD COMPONENT.  {16}
                    NULL;
               WHEN CARR (1) =>         -- ERROR: ARRAY COMPONENT.   {16}
                    NULL;
               WHEN OTHERS =>
                    NULL;
          END CASE;
     END;

END B49005A;
