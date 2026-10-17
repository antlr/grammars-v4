-- B36171A.ADA

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
-- CHECK THAT AN INDEX CONSTRAINT CANNOT BE APPLIED TO
-- A SCALAR TYPE, RECORD TYPE, PRIVATE TYPE, OR ACCESS TYPE
-- THAT DESIGNATES ANY OF THESE TYPES.

-- CHECK THAT THE NUMBER OF DISCRETE RANGES MUST EQUAL
-- THE NUMBER OF DIMENSIONS FOR BOTH ARRAY AND ACCESS TYPES.

-- CHECK THAT THE BASE TYPES OF THE INDEX CONSTRAINTS MUST
-- MATCH THOSE OF THE INDEXES BEING CONSTRAINED.

-- CHECK THAT THE INDEX_CONSTRAINT CANNOT BE GIVEN FOR
-- AN ARRAY TYPE ALREADY CONSTRAINED.

-- CHECK THAT AN INDEX_CONSTRAINT CANNOT BE OMITTED IN A
-- NON-CONSTANT OBJECT DECLARATION, AN ARRAY_TYPE_DEFINITION
-- (THE COMPONENT PART), OR AS A COMPONENT
-- IN A RECORD DEFINITION.

-- DAT 2/9/81
-- ABW 6/9/82
-- L.BROWN  7/8/86   1) ADDED CASES FOR ACCESS TYPES BEING CONSTRAINED.
-- PWN 11/05/95  REMOVED CHECK INVOLVING ARRAY RANGE CONSTRAINTS.
-- PWN 03/21/96  Restored checks in Ada 95 legal format.

PROCEDURE B36171A IS

     -- CHECK THAT INDEX_CONSTRAINT IS APPLIED ONLY TO ARRAY OR  
     -- ACCESS ARRAY TYPES.

     A : INTEGER (1 .. 2);                   -- ERROR: (1 .. 2).
     A1 : INTEGER (BOOLEAN);                 -- ERROR: (BOOLEAN).

     TYPE B IS ARRAY (INTEGER) OF BOOLEAN;
     TYPE REC IS RECORD
          C : B;
     END RECORD;
     A3 : REC (3 .. 4);                      -- ERROR: (3..4).

     PACKAGE P1 IS
          TYPE PRIV IS PRIVATE;
     PRIVATE
          TYPE PRIV IS ARRAY (BOOLEAN) OF BOOLEAN;
     END P1;
     USE P1;
     A5 : PRIV (FALSE .. TRUE);              -- ERROR: CONSTRAINED.

     TYPE AB IS ACCESS B;
     TYPE AI IS ACCESS INTEGER;
     TYPE AR IS ACCESS REC;
     TYPE AP IS ACCESS PRIV;
     AB1 : AB (1 .. 2);                      -- ERROR: (1..2).
     AI1 : AI (1 .. 2);                      -- ERROR: (1..2).
     AR1 : AR (0 .. 1);                      -- ERROR: (0..1).
     AP1 : AP;                               -- OK.
     AP2 : AP (FALSE .. TRUE);               -- ERROR: (FALSE .. TRUE).
     AP3 : AP (BOOLEAN);                     -- ERROR: (BOOLEAN).

     -- CHECK THAT NUMBER OF DIMENSIONS MUST MATCH.

     TYPE D3 IS ARRAY (INTEGER RANGE <>, INTEGER RANGE <>,
          INTEGER RANGE <> ) OF BOOLEAN RANGE FALSE .. FALSE;
     TYPE AD3 IS ACCESS D3;
     TYPE D4 IS ARRAY (BOOLEAN RANGE <>,
          BOOLEAN RANGE <>, BOOLEAN RANGE <>,
          BOOLEAN RANGE <> ) OF STRING (1 .. 0);
     TYPE AD4 IS ACCESS D4;
     D3A : D3 (1..2,3..4,5..6,7..8);         -- ERROR: 4 DIMS.
     D3B : D3 (1..2,3..4);                   -- ERROR: 2 DIMS.
     D3C : D3 (1..2,3..4,5..6);              -- OK.
     AD3A : AD3 (1..2,3..4);                 -- ERROR: 2 DIMS.
     AD3B : AD3 (1..2);                      -- ERROR: 1 DIM.
     D4A : D4(FALSE..FALSE,BOOLEAN,BOOLEAN); -- ERROR: 3 DIMS.
     SUBTYPE BFT IS BOOLEAN RANGE FALSE .. FALSE;
     AD4B : AD4 (BFT,BFT,BFT,BFT);           -- OK.
     AD4C : AD4 (BFT, BFT, BFT);             -- ERROR: 3 DIMS.
     AD4D : AD4;                             -- OK.
     D4F : D4;                               -- ERROR: UNCONSTRAINED.
     AD4G : AD4 (BFT,BFT,BFT,BFT,BFT);       -- ERROR: 5 DIMS.

     -- CHECK THAT INDEX BASE TYPES MUST MATCH.

     TYPE NB IS (FALSE, TRUE);
     TYPE NI IS RANGE 1 .. 10;
     DNB : AD4 (NB, NB, NB, NB);             -- ERROR: NB.
     DNB1 : AD4 (BFT,BFT,BFT,NB);            -- ERROR: NB.
     D3X : D3 (NI,NI,NI);                    -- ERROR: NI.
     D3Y : AD3 (1..10,1..10,NI RANGE 1..10); -- ERROR: NI.

     -- CHECK THAT TYPE_MARK MUST BE UNCONSTRAINED.

     TYPE ND3 IS NEW D3 (1..2,1..2,1..2);
     SUBTYPE ND3A IS ND3 (1..2,1..2,1..2);   -- ERROR: CONSTRAINED.
     ND3B : ND3 (1..2,1..2,1..2);            -- ERROR: CONSTRAINED.

     -- CHECK THAT INDEX_CONSTRAINT IS REQUIRED IN CERTAIN CONTEXTS.

     C1 : CONSTANT D3 :=                     -- OK.
          (1..2=>(1..2=>(1..2=>FALSE)));     -- OK.
     V1 : D3 :=                              -- OK.
          (1..2=>(1..2=>(1..2=>FALSE)));     -- OK.

     SUBTYPE NULL_INT IS INTEGER RANGE 1 .. 0;
     SUBTYPE INT_1 IS INTEGER RANGE 1 .. 1;
     TYPE U1 IS ARRAY (INT_1 RANGE <> ) OF INT_1;

     CU1 : CONSTANT U1 := (1 => 1);          -- OK.
     VU1 : U1 := (1 => 1);                   -- OK.

     TYPE REC2 IS RECORD
          E1 : U1;                           -- ERROR: UNCONSTRAINED.
          E2 : INTEGER;
          E3 : BOOLEAN;
     END RECORD;

     TYPE REC3 IS RECORD
          E1 : U1 := (1 => 1);               -- ERROR: UNCONSTRAINED.
     END RECORD;

     VA2 : ARRAY (1..1) OF
          U1 := (1=>(1=>1));                 -- ERROR: UNCONSTRAINED.
     CA2 : CONSTANT ARRAY (1..1) OF
          U1 := (1=>(1=>1));                 -- ERROR: UNCONSTRAINED U1.
     TYPE AA IS ARRAY (INTEGER RANGE <>) OF
          U1;                                -- ERROR: UNCONSTRAINED U1.

     S : STRING;                             -- ERROR: UNCONSTRAINED.
     TYPE SREC IS RECORD
          E : STRING;                        -- ERROR: UNCONSTRAINED.
     END RECORD;
     TYPE T IS ARRAY (2..1) OF STRING;       -- ERROR: UNCONSTRAINED.
     CST : CONSTANT ARRAY(1..1) OF
          STRING := (1 => "");               -- ERROR: UNCONSTRAINED.
     TYPE T2 IS NEW STRING
          (NATURAL RANGE 1..0);              -- OK.

BEGIN
     NULL;
END B36171A;
