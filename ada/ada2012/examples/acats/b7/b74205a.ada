-- B74205A.ADA

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
-- CHECK THAT THE ADDITIONAL OPERATIONS FOR A COMPOSITE TYPE WITH A
-- COMPONENT OF A PRIVATE TYPE ARE NOT DECLARED BEFORE THE EARLIEST
-- PLACE WITHIN THE IMMEDIATE SCOPE OF THE DECLARATION OF THE COMPOSITE
-- TYPE AND AFTER THE FULL DECLARATION OF THE PRIVATE TYPE.

-- IN PARTICULAR, CHECK FOR THE FOLLOWING:

-- (1)  RELATIONAL OPERATORS WITH ARRAYS OF SCALAR TYPES
-- (2)  LOGICAL OPERATORS WITH ARRAYS OF BOOLEAN TYPES
-- (3)  STRING LITERALS WITH ARRAYS OF CHARACTER
-- (4)  EQUALITY WITH ARRAYS AND RECORDS OF LIMITED PRIVATE TYPES
-- (5)  CATENATION WITH ARRAYS OF LIMITED PRIVATE TYPES
-- (6)  AGGREGATES WITH ARRAYS AND RECORDS OF LIMITED PRIVATE TYPES
-- (7)  INITIALIZATION WITH ARRAYS AND RECORDS OF LIMITED PRIVATE TYPES
-- (8)  ASSIGNMENT WITH ARRAYS AND RECORDS OF LIMITED PRIVATE TYPES

-- DSJ 5/2/83
-- JBG 8/21/83
-- JBG 10/24/83
-- JRK 1/31/84
-- JBG 4/6/84
-- RLB 3/23/07 Corrected to allow limited aggregates.
-- RLB 4/05/07 Fixed missed case.
PROCEDURE B74205A IS

     PACKAGE PACK1 IS
          TYPE LP_CHAR IS LIMITED PRIVATE;
          PACKAGE PACK_LP IS
               TYPE LP_ARR_BASE IS ARRAY (INTEGER RANGE <>) OF LP_CHAR;
               SUBTYPE ARR_LP_CHAR_2 IS LP_ARR_BASE ( 1 .. 2 );
               SUBTYPE ARR_LP_CHAR_4 IS LP_ARR_BASE ( 1 .. 4 );
          END PACK_LP;

          TYPE P_BOOL IS PRIVATE;
          TYPE P_CHAR IS PRIVATE;
          PACKAGE PACK2 IS
               TYPE ARR_BASE IS ARRAY (INTEGER RANGE <>) OF P_BOOL;
               SUBTYPE ARR_P_BOOL_2 IS ARR_BASE ( 1 .. 2 );
               SUBTYPE ARR_P_BOOL_4 IS ARR_BASE ( 1 .. 4 );
               TYPE ARR_P_CHAR_2 IS ARRAY (1..2) OF P_CHAR;
          END PACK2;

          TYPE P_INT_ARR IS PRIVATE;
          TYPE P_REC IS PRIVATE;
          PACKAGE PACK3 IS
               TYPE ARR_P_INT_ARR IS ARRAY ( 1 .. 2 ) OF P_INT_ARR;
               TYPE ARR_P_REC IS ARRAY ( 1 .. 2 ) OF P_REC;
          END PACK3;
     PRIVATE
          TYPE LP_CHAR IS NEW CHARACTER;
          TYPE P_BOOL IS NEW BOOLEAN;
          TYPE P_CHAR IS NEW CHARACTER;
          TYPE P_INT_ARR IS ARRAY ( 1 .. 2 ) OF INTEGER;
          TYPE P_REC IS
               RECORD
                    C1, C2 : INTEGER;
               END RECORD;
     END PACK1;

     PACKAGE BODY PACK1 IS
          USE PACK_LP;
          USE PACK2;
          USE PACK3;

          L1, L2 : ARR_LP_CHAR_2;
          A0     : ARR_P_BOOL_2 := (1..2 => TRUE);     -- OK.
          A1, A2 : ARR_P_BOOL_2;
          X2     : ARR_P_INT_ARR;
          X3     : ARR_P_REC;

          A3 : ARR_LP_CHAR_2 := L1;                    -- ERROR: :=
          A4 : ARR_P_BOOL_2  := ARR_P_BOOL_2'(A0);     -- OK.
          B1 : BOOLEAN := A1 <  A2;                    -- ERROR: <
          B2 : BOOLEAN := A1 >= A2;                    -- ERROR: >=
          B3 : BOOLEAN := L1 =  L2;                    -- ERROR: =
          B4 : BOOLEAN := L1 /= L2;                    -- ERROR: /=
          N1 : INTEGER := X3(1).C1;                    -- OK.
          N2 : INTEGER := X2(1)(2);                    -- OK.
          N3 : INTEGER := A1'SIZE;                     -- OK.
          N4 : P_INT_ARR := X2(1)(1..2);               -- OK.
          L3 : ARR_LP_CHAR_2 := "AB";                  -- ERROR: "AB".
          L4 : ARR_LP_CHAR_2 := ('A', 'B');            -- OK.
          L5 : ARR_LP_CHAR_2 := L4;                    -- ERROR: LIM OBJ.
          L6 : ARR_P_CHAR_2  := "AB";                  -- ERROR: "AB".
          L7 : ARR_P_CHAR_2  := ('A', 'B');            -- OK.
          L8 : ARR_P_CHAR_2 := L7;                     -- OK.

          PROCEDURE G1 (X : ARR_P_BOOL_2 := NOT A1) IS     -- ERROR: NOT
          BEGIN
               NULL;
          END G1;

          PROCEDURE G2 (X : ARR_P_BOOL_2 := A1 AND A2) IS  -- ERROR: AND
          BEGIN
               NULL;
          END G2;

          PROCEDURE G3 (X : ARR_P_BOOL_4 := A1 & A2) IS    -- OK: &
          BEGIN
               NULL;
          END G3;

          PROCEDURE G4 (X : ARR_P_BOOL_2 := (FALSE,TRUE) ) IS  -- OK.
          BEGIN
               NULL;
          END G4;

          PROCEDURE G5 (X : ARR_LP_CHAR_4 := L1 & L2) IS   -- ERROR: &
          BEGIN
               NULL;
          END G5;

          PROCEDURE G6 (X : ARR_LP_CHAR_2 := ('A', 'B')) IS  -- OK: AGGR.
          BEGIN
               NULL;
          END G6;

          PACKAGE BODY PACK_LP IS
               L1, L2 : ARR_LP_CHAR_2;
               A3 : ARR_LP_CHAR_2 := L1;                   -- OK: :=
               B3 : BOOLEAN := L1 =  L2;                   -- OK: =
               B4 : BOOLEAN := L1 /= L2;                   -- OK: /=
               L3 : ARR_LP_CHAR_2 := "AB";                 -- OK: "AB".
               L4 : ARR_LP_CHAR_2 := ('A', 'B');           -- OK: AGGR.
               L5 : ARR_LP_CHAR_2 := L4;                   -- OK: OBJ.
          END PACK_LP;

          PACKAGE BODY PACK2 IS
               A0     : ARR_P_BOOL_2 := (1..2 => TRUE);    -- OK.
               A1, A2 : ARR_P_BOOL_2;
               A4 : ARR_P_BOOL_2  := ARR_P_BOOL_2'(A0);    -- OK.
               B1 : BOOLEAN := A1 <  A2;                   -- OK: <
               B2 : BOOLEAN := A1 >= A2;                   -- OK: >=
               N3 : INTEGER := A1'SIZE;                    -- OK.
               N4 : P_INT_ARR := X2(1)(1..2);              -- OK.
               L5 : ARR_P_CHAR_2  := "AB";                 -- OK: "AB".
               L6 : ARR_P_CHAR_2  := ('A', 'B');           -- OK: AGGR.
               L7 : ARR_P_CHAR_2  := L6;                   -- OK: OBJ.
          END PACK2;

          PACKAGE BODY PACK3 IS
               X2     : ARR_P_INT_ARR;
               X3     : ARR_P_REC;
               N1 : INTEGER := X3(1).C1;                   -- OK.
               N2 : INTEGER := X2(1)(2);                   -- OK.
               N4 : P_INT_ARR := X2(1)(1..2);              -- OK.
          END PACK3;

     END PACK1;

BEGIN

     NULL;

END B74205A;
