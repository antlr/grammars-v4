-- B66001B.ADA

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
-- CHECK THAT SUBPROGRAM REDECLARATIONS ARE FORBIDDEN.  CHECK THAT TASK
-- ENTRIES CANNOT OVERLOAD OTHER TASK ENTRIES OR PROCEDURES WHEN THERE
-- IS NO WAY TO DISTINGUISH BETWEEN THEM.
-- SUBTESTS ARE FOR TWO ENTRIES, AND AN ENTRY AND A PROCEDURE, IN THE
-- SAME DECLARATIVE PART THAT ARE IDENTICAL EXCEPT FOR:
--        (A) A PARAMETER NAME IS DIFFERENT.
--        (B) THE SUBTYPE OF A PARAMETER IS DIFFERENT.
--        (C) A PARAMETER MODE IS DIFFERENT.
--        (D) A PARAMETER HAS A DIFFERENT DEFAULT VALUE.
--        (E) THE PRESENCE OF A DEFAULT PARAMETER VALUE.

-- CPP  6/7/84
-- RLB 11/19/19  Added error location indicators.

PROCEDURE B66001B IS

BEGIN

     --------------------------------------------------

 A : DECLARE
          TASK TYPE T IS
               ENTRY ENT_ENT (I1 : IN INTEGER; I2 : IN OUT INTEGER;
                             I3 : OUT INTEGER);
               ENTRY ENT_ENT (X1 : IN INTEGER; I2 : IN OUT INTEGER;
                             I3 : OUT INTEGER);        -- ERROR:     {1:16}
                                                       -- ENT_ENT (A).
               ENTRY ENT_PROC (X : INTEGER);
          END T;

     TASK BODY T IS
               PROCEDURE ENT_PROC (Y : INTEGER) IS     -- ERROR:     {16}
               BEGIN                                   -- ENT_PROC (A).
                    NULL;
               END ENT_PROC;
          BEGIN
               NULL;
          END;
     BEGIN     -- A
          NULL;
     END A;

     --------------------------------------------------

 B : DECLARE

          SUBTYPE INT10 IS INTEGER RANGE 1..10;
          SUBTYPE INT20 IS INTEGER RANGE 1..20;

          TASK TYPE T IS
               ENTRY ENT_ENT (I : INT10);
               ENTRY ENT_ENT (I : INT20);              -- ERROR:     {16}
                                                       -- ENT_ENT (B).
               ENTRY ENT_ENT (I : INTEGER);            -- ERROR:     {16}
                                                       -- ENT_ENT (B).

               ENTRY ENT_PROC (X : INT10);
          END T;

          TASK BODY T IS
               PROCEDURE ENT_PROC (X : INT20) IS       -- ERROR:     {16}
               BEGIN                                   -- ENT_PROC (B).
                    NULL;
               END ENT_PROC;

               PROCEDURE ENT_PROC (X : INTEGER) IS     -- ERROR:     {16}
               BEGIN                                   -- ENT_PROC (B).
                    NULL;
               END ENT_PROC;

          BEGIN
               NULL;
          END T;

     BEGIN     -- B
          NULL;
     END B;

     --------------------------------------------------

 C : DECLARE

          TASK TYPE T IS
               ENTRY ENT_ENT (X : IN OUT INTEGER);
               ENTRY ENT_ENT (X : OUT INTEGER);        -- ERROR: ENT_ENT  {16}
                                                       -- (C).
               ENTRY ENT_ENT (X : IN INTEGER);         -- ERROR: ENT_ENT  {16}
                                                       -- (C).
               ENTRY ENT_PROC (X : IN INTEGER);
          END T;

          TASK BODY T IS
               PROCEDURE ENT_PROC
                    (X : IN OUT INTEGER) IS            -- ERROR:       {1:16}
                                                       -- ENT_PROC (C).
               BEGIN
                    NULL;
               END ENT_PROC;

               PROCEDURE ENT_PROC (X : OUT INTEGER) IS -- ERROR:       {16}
               BEGIN                                   -- ENT_PROC (C).
                    NULL;
               END ENT_PROC;

          BEGIN
               NULL;
          END T;

     BEGIN     -- C
          NULL;
     END C;

     --------------------------------------------------

 D : DECLARE

          TASK TYPE T IS
               ENTRY ENT_ENT (X : IN INTEGER := 1);
               ENTRY ENT_ENT (X : IN INTEGER := 2);    -- ERROR: ENT_ENT   {16}
                                                       -- (D).
               ENTRY ENT_PROC (X : INTEGER := 1);
          END T;

          TASK BODY T IS
               PROCEDURE ENT_PROC
                    (X : INTEGER := 2) IS              -- ERROR:         {1:16}
                                                       -- ENT_PROC (D).
               BEGIN
                    NULL;
               END ENT_PROC;

          BEGIN
               NULL;
          END T;

     BEGIN     -- D
          NULL;
     END D;

     --------------------------------------------------

 E : DECLARE

          TASK TYPE T IS
               ENTRY ENT_ENT (I1 : INTEGER; S1 : STRING := "LMN");
               ENTRY ENT_ENT (I1 : INTEGER; S1 : STRING);  -- ERROR:     {16}
                                                       -- ENT_ENT (E).
               ENTRY ENT_PROC (I2 : INTEGER; S2 : STRING := "QRS");
          END T;

          TASK BODY T IS
               PROCEDURE ENT_PROC
                    (I2 : INTEGER; S2 : STRING) IS     -- ERROR:        {1:16}
                                                       -- ENT_PROC (E).
               BEGIN
                    NULL;
               END ENT_PROC;

          BEGIN
               NULL;
          END T;

     BEGIN     -- E
          NULL;
     END E;

     --------------------------------------------------

END B66001B;
