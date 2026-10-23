-- B83011A.ADA

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
--     CHECK THAT WITHIN THE DECLARATION OF AN OBJECT, NUMBER, TYPE,
--     SUBTYPE, FORMAL PARAMETER OF A SUBPROGRAM OR ENTRY, OR GENERIC
--     FORMAL PARAMETER, THE DECLARED ENTITY IS NOT VISIBLE, EITHER
--     DIRECTLY OR BY SELECTION.
--     INCLUDE CHECKS OF RENAMING DECLARATIONS FOR OBJECTS, EXCEPTIONS,
--     AND PACKAGES.

-- HISTORY:
--     VCL  03/16/88  CREATED ORIGINAL TEST.
--     DTN  11/29/95  REMOVED CONFORMANCE CHECKS WHERE RULES RELAXED.
--     PWN  03/28/96  Restored checks in Ada95 legal format.

PROCEDURE B83011A IS
     PACKAGE PACK IS
          V1 : INTEGER := 5;
          C1 : FLOAT   := 6.5;

          TYPE V2 IS RANGE 0 .. 10;
          TYPE C2 IS DIGITS 5;

          N2 : CONSTANT := 5;

          T1 : CONSTANT := 2;
          T5 : INTEGER := 1;
          TYPE T6 IS (ONE, TWO);
          TYPE T8 IS RANGE 1 .. 10;
          TYPE T10 IS RANGE 1 .. 5;
          TYPE T12 IS DIGITS 5 RANGE 0.0 .. 10.0;
          TYPE T13 IS (TE1, TE2, TE3);

          TYPE ST1 IS (A, B, C, D, E);
          ST2 : INTEGER := 1;

          TYPE FP1 IS DIGITS 5 RANGE 0.0 .. 10.0;
          FP2 : CONSTANT := 3;
          TYPE FP4 IS DIGITS 5 RANGE 0.0 .. 10.0;
          FP5 : CONSTANT := 3;

          TYPE GFP1 IS RANGE 0 .. 5;
          GFP2 : CONSTANT := 2;
          TYPE GFP3 IS (E1, E2, E3);
          TYPE GFP4 IS DIGITS 5;
          TYPE GFP5 IS RANGE 1 .. 3;
          TYPE GFP6 IS RANGE 0 .. 5;
          GFP7 : CHARACTER := 'Z';
          TYPE GFP8 IS RANGE 0 .. 10;

          TYPE RD1 IS (R1, R2, R3);
          X : RD1;
          RD2 : FLOAT;
          RD4 : EXCEPTION;
          PACKAGE RD6 IS END RD6;
     END PACK;
     USE PACK;

     V1 : INTEGER := V1;                                   -- ERROR: V1.
     V2 : V2;                                              -- ERROR: V2.
     V3 : STRING(1..5) := B83011A.V3;                      -- ERROR: V3.
     V4 : ARRAY (1..5) OF INTEGER := (V4'RANGE => 0);      -- ERROR: V4.
     C1 : CONSTANT FLOAT := C1;                            -- ERROR: C1.
     C2 : CONSTANT C2 := 2.5;                              -- ERROR: C2.
     C3 : CONSTANT INTEGER := B83011A.C3;                  -- ERROR: C3.

     N1 : CONSTANT := B83011A.N1;                          -- ERROR: N1.
     N2 : CONSTANT := N2;                                  -- ERROR: N2.

     TYPE T1 IS RANGE T1 .. 9;                             -- ERROR: T1.
     TYPE T4 IS (Q4, R4, S4, T4, U4);                      -- ERROR: T4.
     TYPE T5 IS ARRAY (T5 .. 10) OF CHARACTER;             -- ERROR: T5.
     TYPE T6 IS ARRAY (1 .. 5) OF T6;                      -- ERROR: T6.
     TYPE T7 IS NEW B83011A.T7;                            -- ERROR: T7.
     TYPE T8 IS
          RECORD
               C1 : T8;                                    -- ERROR: T8.
          END RECORD;
     TYPE T9 IS
          RECORD
               C1 : B83011A.T9;                            -- ERROR: T9.
          END RECORD;
     TYPE T10 IS ACCESS T10;                              -- ERROR: T10.
     TASK TYPE T12 IS
          ENTRY E1 (P1 : T12);                            -- ERROR: T12.
     END T12;
     TASK TYPE T13 IS
          ENTRY E1 (T13);                                 -- ERROR: T13.
     END T13;
     TASK TYPE T14 IS
          ENTRY E1 (P1 : B83011A.T14);                    -- ERROR: T14.
     END T14;

     SUBTYPE ST1 IS ST1;                                  -- ERROR: ST1.
     SUBTYPE ST2 IS STRING (ST2 .. 10);                   -- ERROR: ST2.
     SUBTYPE ST3 IS B83011A.ST3;                          -- ERROR: ST3.

     PROCEDURE PX1 (FP1 : FP1);                           -- ERROR: FP1.
     FUNCTION FX1 (FP2 : INTEGER := FP2)                  -- ERROR: FP2.
                                   RETURN BOOLEAN;
     TASK TK1 IS
          ENTRY E1 (FP4 : FP4);                           -- ERROR: FP4.
     END TK1;
     TASK TK2 IS
          ENTRY E1 (FP5 : INTEGER := FP5);                -- ERROR: FP5.
     END TK2;
     TASK TK3 IS
          ENTRY E1 (FP6 : STRING := E1.FP6);              -- ERROR: FP6.
     END TK3;

     GENERIC
          GFP1 : GFP1;                                   -- ERROR: GFP1.
          GFP2 : INTEGER := GFP2;                        -- ERROR: GFP2.
          TYPE GFP3 IS ARRAY (GFP3 RANGE <>) OF INTEGER; -- ERROR: GFP3.
          TYPE X IS (<>);
          TYPE GFP4 IS ARRAY (X RANGE <>) OF GFP4;       -- ERROR: GFP4.
          TYPE GFP5 IS ACCESS GFP5;                      -- ERROR: GFP5.
          WITH PROCEDURE GFP6 (P1 : GFP6);               -- OK.
          WITH PROCEDURE GFP7 (P1 : CHARACTER := GFP7);  -- OK.
          WITH FUNCTION GFP8 (P1 : INTEGER) RETURN GFP8; -- OK.
          WITH FUNCTION GFP9 (P1 : STRING := GFP9("AA")) -- ERROR: GFP9.
                         RETURN STRING;
     PROCEDURE PX;

     RD1 : RD1 RENAMES X;                                 -- ERROR: RD1.
     RD2 : FLOAT RENAMES RD2;                             -- ERROR: RD2.
     RD4 : EXCEPTION RENAMES RD4;                         -- ERROR: RD4.
     RD5 : EXCEPTION RENAMES B83011A.RD5;                 -- ERROR: RD5.
     PACKAGE RD6 RENAMES RD6;                             -- ERROR: RD6.
     PACKAGE RD7 RENAMES B83011A.RD7;                     -- ERROR: RD7.


     -- REQUIRED BODIES FOR THE ABOVE DECLARATIONS.


     TASK BODY T12 IS
     BEGIN
          NULL;
     END T12;

     TASK BODY T13 IS
     BEGIN
          NULL;
     END T13;

     TASK BODY T14 IS
     BEGIN
          NULL;
     END T14;

     PROCEDURE PX1 (FP1 : FP1) IS               -- OPTIONAL ERR MESSAGE:
     BEGIN                                      --  FP1.
          NULL;
     END PX1;

     FUNCTION FX1 (FP2 : INTEGER := FP2)        -- OPTIONAL ERR MESSAGE:
                   RETURN BOOLEAN IS            --  FP2.
     BEGIN
          RETURN FALSE;
     END FX1;

     TASK BODY TK1 IS
     BEGIN
          NULL;
     END TK1;

     TASK BODY TK2 IS
     BEGIN
          NULL;
     END TK2;

     TASK BODY TK3 IS
     BEGIN
          NULL;
     END TK3;

     PROCEDURE PX IS
     BEGIN
          NULL;
     END PX;
BEGIN
     NULL;
END B83011A;
