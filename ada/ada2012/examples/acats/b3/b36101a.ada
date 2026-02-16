-- B36101A.ADA

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
-- CHECK THAT NON-DISCRETE DISCRETE RANGES ARE DISALLOWED IN ALL
-- CONTEXTS.
-- CONTEXTS CHECKED ARE LOOP RANGES, ARRAY TYPE DEFINITIONS, CASE
-- CHOICES, VARIANT PART CHOICES, ARRAY AGGREGATES, SLICES, AND INDEX
-- CONSTRAINTS IN OBJECT AND TYPE DECLARATIONS.

-- DAT 2/2/81
-- JRK 2/2/83
-- PWN 11/05/95  QUALIFIERS ADDED TO RESOLVE CHARACTER LITERALS.
-- TMB 11/19/96  PREVIOUS CHANGE BACKED OUT AND IS NOW AN ERROR FOR ADA95

PROCEDURE B36101A IS

     TYPE FX IS DELTA 1.0 RANGE -2.0 .. 2.0;     -- NON-DISCRETE TYPE.
     F1 : CONSTANT FX := 1.0;
     FM1 : CONSTANT FX := -1.0;

     PACKAGE P IS
          TYPE PRIV IS PRIVATE;                  -- NON-DISCRETE TYPE.
          PM1, P1 : CONSTANT PRIV;
     PRIVATE
          TYPE PRIV IS NEW INTEGER RANGE -2 .. 2;
          PM1 : CONSTANT PRIV := -1;
          P1 : CONSTANT PRIV := 1;
     END P;
     USE P;

     TYPE DISC IS NEW INTEGER RANGE -2 .. 2;     -- LEGAL DISCRETE TYPE.
     D1 : CONSTANT DISC := 1;
     DM1 : CONSTANT DISC := -1;

     TYPE A1 IS ARRAY (DISC RANGE -1 .. 1) OF FX;     -- OK.
     TYPE A2 IS ARRAY (DISC RANGE -1.0 .. 1) OF FX;   -- ERROR: -1.0
     TYPE A3 IS ARRAY (DISC RANGE -1 .. 1.0) OF FX;   -- ERROR: 1.0
     TYPE A4 IS ARRAY (DISC RANGE -1.0 .. 1.0) OF FX; -- ERROR: -1.0,1.0
     TYPE A5 IS ARRAY (DISC RANGE FM1 .. 1) OF FX;    -- ERROR: FM1
     TYPE A6 IS ARRAY (DISC RANGE -1 .. P1) OF FX;    -- ERROR: P1
     TYPE A7 IS ARRAY (DISC RANGE FM1 .. F1) OF FX;   -- ERROR: FM1,F1
     TYPE A8 IS ARRAY (DISC RANGE DM1 .. D1) OF FX;   -- OK.

     TYPE B1 IS ARRAY (FX RANGE -1 .. 1) OF FX;       -- ERROR: FX
     TYPE B2 IS ARRAY (FX RANGE FM1 .. 1) OF FX;      -- ERROR: FX,FM1
     TYPE B3 IS ARRAY (FX RANGE -1 .. F1) OF FX;      -- ERROR: FX,F1
     TYPE B4 IS ARRAY (PRIV RANGE PM1 .. P1) OF FX;   -- ERROR: PRIV,
                                                      --        PM1,P1
     TYPE B5 IS ARRAY (PRIV RANGE -1 .. 1) OF FX;     -- ERROR: PRIV
     B6 : ARRAY (FX RANGE -1.0 .. 1.0) OF FX;         -- ERROR: FX,-1.0,
                                                      --        1.0

     C1 : ARRAY (FM1 .. F1) OF FX;                    -- ERROR: FM1,F1
     C2 : ARRAY (FM1 .. 1) OF FX;                     -- ERROR: FM1
     C3 : ARRAY (-1.0 .. F1) OF FX;                   -- ERROR: -1.0,F1
     C4 : ARRAY (-1.0 .. 1.0) OF FX;                  -- ERROR: -1.0,1.0
     TYPE C5 IS ARRAY (PM1 .. P1) OF FX;              -- ERROR: PM1,P1
     TYPE C6 IS ARRAY (-1 .. P1) OF FX;               -- ERROR: P1
     TYPE C7 IS ARRAY (PM1 .. 1) OF FX;               -- ERROR: PM1
     TYPE C8 IS ARRAY ("A" .. "Z") OF FX;             -- ERROR: "A","Z"
     CB_OK : ARRAY (CHARACTER RANGE 'A' .. 'Z') OF FX;    -- OK.
     CB : ARRAY ('A' .. 'Z') OF FX;                   -- ERROR: 'A' AMBIGUOUS

     TYPE REC IS RECORD                          -- NON-DISCRETE TYPE.
          V : INTEGER;
     END RECORD;
     RM1 : CONSTANT REC := (OTHERS => -1);
     R1 : CONSTANT REC := (OTHERS => 1);
     TYPE DD1 IS ARRAY (REC RANGE RM1 .. R1) OF FX;   -- ERROR: REC,RM1,
                                                      --        R1
     TYPE D2 IS ARRAY (RM1 .. R1) OF FX;              -- ERROR: RM1,R1
     TYPE D3 IS ARRAY (REC RANGE -1 .. 1) OF FX;      -- ERROR: REC

     TYPE RR1 (D1 : INTEGER) IS RECORD
          CASE D1 IS
               WHEN FM1-2.0 .. F1-2.0 => NULL;        -- ERROR: FM1,2.0,
                                                      --        F1,2.0
               WHEN -5.0 .. -4.0 => NULL;             -- ERROR: 5.0,4.0
               WHEN PM1 .. P1 => NULL;                -- ERROR: PM1,P1
               WHEN FM1 .. F1 => NULL;                -- ERROR: FM1,F1
               WHEN OTHERS => NULL;                   -- OK.
          END CASE;
     END RECORD;

     S1 : STRING ("A" .. "Z");                        -- ERROR: "A","Z"
     S2 : STRING (1 .. F1);                           -- ERROR: F1
     TYPE S3 IS NEW STRING ("A" .. "A");              -- ERROR: "A","A"
     TYPE S4 IS NEW STRING (F1 .. F1);                -- ERROR: F1,F1
     S5 : STRING (2.0 .. 4.0);                        -- ERROR: 2.0,4.0
     TYPE S6 IS ARRAY ("A" .. "A") OF CHARACTER;      -- ERROR: "A","A"

     S : STRING (1 .. 100);                           -- OK.
     A_OK : ARRAY (CHARACTER RANGE 'A' .. 'Z') OF CHARACTER;  -- OK.
     A : ARRAY ('A' .. 'Z') OF CHARACTER;             -- ERROR: 'A' AMBIGUOUS

     TYPE IA IS ARRAY (INTEGER RANGE <>) OF INTEGER;
     I1 : IA (FM1 .. F1);                             -- ERROR: FM1,F1
     I2 : IA (PM1 .. P1);                             -- ERROR: PM1,P1
     I3 : IA (-1.0 .. 1.0);                           -- ERROR: -1.0,1.0
     I4 : IA (1 .. 3.0);                              -- ERROR: 3.0
     I : IA (-1 .. 1);                                -- OK.

BEGIN

     I := (FM1 .. F1 => 0);                           -- ERROR: FM1,F1
     I := (FX RANGE FM1 .. 1.0 => 0);                 -- ERROR: FX,FM1,
                                                      --        1.0
     I := (PM1 .. P1 => 0);                           -- ERROR: PM1,P1
     I := (-1.0 .. 1.0 => 0);                         -- ERROR: -1.0,1.0
     I := I (INTEGER RANGE -1.0 .. 1);                -- ERROR: -1.0
     I := I (FX RANGE FM1 .. F1);                     -- ERROR: FX,FM1,
                                                      --        F1
     I := I (PM1 .. P1);                              -- ERROR: PM1,P1

     CASE 3 IS
          WHEN FM1 .. F1 => NULL;                     -- ERROR: FM1,F1
          WHEN PM1 .. 0 => NULL;                      -- ERROR: PM1
          WHEN FX RANGE -1.0 .. 1.0 => NULL;          -- ERROR: FX,-1.0,
                                                      --        1.0
          WHEN OTHERS => NULL;                        -- OK.
     END CASE;

     FOR I IN 1.0 .. 3.0 LOOP                         -- ERROR: 1.0,3.0
          NULL;
     END LOOP;

     FOR I IN 1.0 .. 3 LOOP                           -- ERROR: 1.0
          NULL;
     END LOOP;

     FOR I IN 1 .. 3.0 LOOP                           -- ERROR: 3.0
          NULL;
     END LOOP;

     FOR I IN FM1 .. F1 LOOP                          -- ERROR: FM1,F1
          NULL;
     END LOOP;

     FOR I IN FM1 .. 1 LOOP                           -- ERROR: FM1
          NULL;
     END LOOP;

     FOR I IN -1 .. P1 LOOP                           -- ERROR: P1
          NULL;
     END LOOP;

     FOR I IN PRIV RANGE PM1 .. 1 LOOP                -- ERROR: PRIV,PM1
          NULL;
     END LOOP;

     FOR I IN PRIV RANGE -1 .. P1 LOOP                -- ERROR: PRIV,P1
          NULL;
     END LOOP;

     FOR I IN FX RANGE FM1 .. F1 LOOP                 -- ERROR: FX,FM1,
                                                      --        F1
          NULL;
     END LOOP;

     FOR I IN PRIV RANGE PM1 .. P1 LOOP               -- ERROR: PRIV,
                                                      --        PM1,P1
          NULL;
     END LOOP;

     FOR I IN FX RANGE -1.0 .. 1.0 LOOP               -- ERROR: FX,-1.0,
                                                      --        1.0
          NULL;
     END LOOP;

     FOR I IN FX RANGE -1 .. 1 LOOP                   -- ERROR: FX
          NULL;
     END LOOP;

     FOR I IN FX RANGE 1.0 .. 3 LOOP                  -- ERROR: FX,1.0
          NULL;
     END LOOP;

     FOR I IN INTEGER RANGE 1 .. 1.0 LOOP             -- ERROR: 1.0
          NULL;
     END LOOP;

     FOR I IN INTEGER RANGE 1 .. F1 LOOP              -- ERROR: F1
          NULL;
     END LOOP;

     FOR I IN INTEGER RANGE F1 .. 0 LOOP              -- ERROR: F1
          NULL;
     END LOOP;

     FOR I IN INTEGER RANGE 1.0 .. 3 LOOP             -- ERROR: 1.0
          NULL;
     END LOOP;

     FOR I IN INTEGER RANGE F1 .. FM1 LOOP            -- ERROR: F1,FM1
          NULL;
     END LOOP;

     FOR I IN INTEGER RANGE 1.0 .. 3.0 LOOP           -- ERROR: 1.0,3.0
          NULL;
     END LOOP;

     FOR I IN CHARACTER RANGE "A" .. "Z" LOOP         -- ERROR: "A","Z"
          NULL;
     END LOOP;

     FOR I IN RM1 .. R1 LOOP                          -- ERROR: RM1,R1
          NULL;
     END LOOP;

END B36101A;
