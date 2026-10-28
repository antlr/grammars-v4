-- B74201A.ADA

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
-- CHECK THAT OBJECTS OF A LIMITED PRIVATE TYPE CANNOT BE COMPARED
-- FOR EQUALITY OR INEQUALITY OUTSIDE THE PACKAGE DEFINING THE TYPE,
-- AND CANNOT BE ASSIGNED.

-- CHECK THAT WITHIN THE PACKAGE, ASSIGNMENT AND EQUALITY
-- ARE AVAILABLE (EVEN INSIDE NESTED PACKAGES), UNLESS
-- THE FULL DEFINITION IS DERIVED FROM A LIMITED PRIVATE
-- TYPE IN A DIFFERENT NON-ENCLOSING PACKAGE.

-- CHECK THAT THE RESTRICTIONS OF THE USE OF ASSIGNMENT AND 
-- EQUALITY EXTEND TO OBJECTS WITH A COMPONENT OF A LIMITED PRIVATE
-- TYPE, AND TO TYPES DERIVED FROM LIMITED PRIVATE TYPES.

-- ALSO, CHECK THAT OBJECTS WITH COMPONENTS OF COMPONENTS WHICH ARE
-- LIMITED PRIVATE ARE ALSO RESTRICTED.

-- DAT  3/13/81
-- SPS 10/18/83
-- EG  12/19/83
-- JRK  1/12/84

PROCEDURE B74201A IS

     SUBTYPE B IS BOOLEAN;

     PACKAGE P1 IS
          TYPE ILP IS ACCESS B;
          CILP : CONSTANT ILP := NULL;
          TYPE LP IS LIMITED PRIVATE;
          CLP : CONSTANT LP;
     PRIVATE
          TYPE LP IS NEW ILP;
          CLP : CONSTANT LP := LP(CILP);
     END P1;
     USE P1;

     PACKAGE PK IS

          TYPE L1 IS LIMITED PRIVATE;

          C1 : CONSTANT L1;
          TYPE A1 IS ARRAY (0..1) OF L1;
          TYPE R1 IS RECORD
               C : L1;
          END RECORD;
          TYPE A2 IS ARRAY (0..1) OF R1;
          TYPE R2 IS RECORD
               C : A2;
          END RECORD;

          TYPE L2 IS LIMITED PRIVATE;
          C2 : CONSTANT L2;
          TYPE R22 IS RECORD
               C : L2;
          END RECORD;

          TYPE L3 IS LIMITED PRIVATE;
          C3 : CONSTANT L3;
          TYPE TT3 IS ARRAY (0..0) OF L3;
          TYPE R3 IS RECORD
               C : TT3;
          END RECORD;

     PRIVATE

          TYPE L3 IS NEW ILP;
          TYPE L4 IS NEW LP;
          TYPE L2 IS NEW L4;
          TYPE L1 IS NEW ILP;
          TYPE TT4 IS ARRAY (0..0) OF L4; 
          TYPE R4 IS RECORD
               C : TT4;
          END RECORD;

          WR3 : R3;
          WA1 : A1;
          WA2 : A2;
          WR1 : R1;
          WR2 : R2;
          WR22 : R22;
          WR4 : R4;

          C1 : CONSTANT L1 := L1(CILP);      -- OK.
          C2 : CONSTANT L2 := L2(CLP);       -- ERROR: L2 LIM PRIV.
          C3 : CONSTANT L3 := L3(L1(CILP));  -- OK.
          C4 : CONSTANT L4 := L4(L2(CILP));  -- ERROR: L4 LIM PRIV.

          W4 : L4;
          W2 : L2;

          B1 : B := C1 = C1;                 -- OK.
          B2 : B := W2 = W2;                 -- ERROR: L2 LIM PRIV.
          B3 : B := C3 = C3;                 -- OK.
          B4 : B := W4 = W4;                 -- ERROR: L4 LIM PRIV.
          BN1 : B := C1 /= C1;               -- OK.
          BN2 : B := W2 /= W2;               -- ERROR: L2 LIM PRIV.
          BN3 : B := C3 /= C3;               -- OK.
          BN4 : B := W4 /= W4;               -- ERROR: L4 LIM PRIV.

          BA1 : B := CILP = CILP;            -- OK.
          BA2 : B := CLP = CLP;              -- ERROR: LP LIM PRIV.
          BA3 : B := CILP /= CILP;           -- OK.
          BA4 : B := CLP /= CLP;             -- ERROR: LP LIM PRIV.
          BA5 : B := WA1 = WA1;              -- OK.
          BA6 : B := WA1 /= WA1;             -- OK.
          BA7 : B := WA2 = WA2;              -- OK.
          BA8 : B := WA2 /= WA2;             -- OK.
          BA9 : B := WR1 = WR1;              -- OK.
          BA10 : B := WR1 /= WR1;            -- OK.
          BA11 : B := WR2 = WR2;             -- OK.
          BA12 : B := WR2 /= WR2;            -- OK.
          BA13 : B := WR22 = WR22;           -- ERROR: L2 LIM PRIV.
          BA14 : B := WR22 /= WR22;          -- ERROR: L2 LIM PRIV.
          BA15 : B := WR3 = WR3;             -- OK.
          BA16 : B := WR3 /= WR3;            -- OK.
          BA17 : B := WR4 = WR4;             -- ERROR: L4 LIM PRIV.
          BA18 : B := WR4 /= WR4;            -- ERROR: L4 LIM PRIV.

          WO1 : L1 := C1;                    -- OK.
          WO2 : L2 := W2;                    -- ERROR: L2 LIM PRIV.
          WO3 : L3 := C3;                    -- OK.
          WO4 : L4 := W4;                    -- ERROR: L4 LIM PRIV.
          WO5 : ILP := CILP;                 -- OK.
          WO6 : LP := CLP;                   -- ERROR: LP LIM PRIV.
          WO7 : A1 := (0..1=>C1);            -- OK.
          WO8 : R1 := (C=>C1);               -- OK.
          WO9 : A2 := (0..1=>WO8);           -- OK.
          WO10 : R2 := (C => WO9);           -- OK.
          WO11 : L2 := W2;                   -- ERROR: L2 LIM PRIV.
          WO12 : R22 := (C => W2);           -- ERROR: R22 LIM PRIV.
          WO13 : L3 := C3;                   -- OK.
          WO14 : R3 := (C=>(0..0=>WO13));    -- OK.
          WO15 : L4 := W4;                   -- ERROR: L4 LIM PRIV.
          WO16 : R4 := (C => (0..0=>W4));    -- ERROR: L4 LIM PRIV.

          PACKAGE NESTED IS
               Q1 : L2 := W2;                -- ERROR: L2 LIM PRIV.
               Q2 : L1 := C1;                -- OK.
               Q3 : L3 := L3(L1(CILP));      -- OK.
               Q4 : L4 := W4;                -- ERROR: L4 LIM PRIV.
               Q5 : B := C1 = C1;            -- OK.
               Q6 : B := C1 /= C1;           -- OK.
               Q7 : B := W2 = W2;            -- ERROR: L2 LIM PRIV.
               Q8 : B := W2 /= W2;           -- ERROR: L2 LIM PRIV.
               Q9 : B := C3 = C3;            -- OK.
               Q10 : B := C3 /= C3;          -- OK.
               Q11 : B := W4 = W4;           -- ERROR: L4 LIM PRIV.
               Q12 : B := W4 /= W4;          -- ERROR: L4 LIM PRIV.
               Q13 : B := CILP = CILP;       -- OK.
               Q14 : B := CILP /= CILP;      -- OK.
               Q15 : B := CLP = CLP;         -- ERROR: LP LIM PRIV.
               Q16 : B := CLP /= CLP;        -- ERROR: LP LIM PRIV.
               Q17 : B := WA1 = WA1;         -- OK.
               Q18 : B := WA1 /= WA1;        -- OK.
               Q19 : B := WA2 = WA2;         -- OK.
               Q20 : B := WA2 /= WA2;        -- OK.
               Q21 : B := WR1 = WR1;         -- OK.
               Q22 : B := WR1 /= WR1;        -- OK.
               Q23 : B := WR2 = WR2;         -- OK.
               Q24 : B := WR2 /= WR2;        -- OK.
               Q25 : B := WR22 = WR22;       -- ERROR: L2 LIM PRIV.
               Q26 : B := WR22 /= WR22;      -- ERROR: L2 LIM PRIV.
               Q27 : B := WR3 = WR3;         -- OK.
               Q28 : B := WR3 /= WR3;        -- OK.
               Q29 : B := WR4 = WR4;         -- ERROR: L4 LIM PRIV.
               Q30 : B := WR4 /= WR4;        -- ERROR: L4 LIM PRIV.
               Q31 : ILP := CILP;            -- OK.
               Q32 : LP := CLP;              -- ERROR: LP LIM PRIV.
               Q33 : A1 := WO7;              -- OK.
               Q34 : R1 := WO8;              -- OK.
               Q35 : A2 := WO9;              -- OK.
               Q36 : R2 := WO10;             -- OK.
               Q37 : R22 := (C => W2);       -- ERROR: L2 LIM PRIV.
               Q38 : R3 := WO14;             -- OK.
               Q39 : R4 := (C => (0..0=>W4));-- ERROR: L4 LIM PRIV.
          END NESTED;

     END PK;

     USE PK;

     WR3 : R3;
     WA1 : A1;
     WA2 : A2;
     WR1 : R1;
     WR2 : R2;
     WR22 : R22;

     Z1 : L1 := C1;                          -- ERROR: L1 LIM PRIV.
     Z2 : L1;                                -- OK.
     Z3 : A1 := (C1, C1);                    -- ERROR: L1 LIM PRIV.
     Z4 : A1 := WA1;                         -- ERROR: A1 LIM PRIV.
     Z5 : R1 := WR1;                         -- ERROR: R1 LIM PRIV.
     Z6 : A2 := WA2;                         -- ERROR: A2 LIM PRIV.
     Z7 : R2 := WR2;                         -- ERROR: R2 LIM PRIV.
     Z8 : L2 := WR22.C;                      -- ERROR: L2 LIM PRIV.
     Z9 : R22 := WR22;                       -- ERROR: R22 LIM PRIV.
     Z10 : L3 := WR3.C(0);                   -- ERROR: L3 LIM PRIV.
     Z11 : R3 := WR3;                        -- ERROR: R3 LIM PRIV.
     Z12 : ILP := CILP;                      -- OK.
     Z13 : LP := CLP;                        -- ERROR: LP LIM PRIV.

     TYPE L4 IS NEW L2;
     TYPE TT5 IS ARRAY (0..0) OF L4;
     TYPE R4 IS RECORD
          C : TT5;
     END RECORD;
     W4 : R4;

     Z20 : B := C1 = C1;                     -- ERROR: NO = DEF.
     Z21 : B := C1 /= C1;                    -- ERROR: NO = DEF.
     Z22 : B := WA1 = WA1;                   -- ERROR: NO = DEF.
     Z23 : B := WA1 /= WA1;                  -- ERROR: NO = DEF.
     Z24 : B := WR1 = WR1;                   -- ERROR: NO = DEF.
     Z25 : B := WR1 /= WR1;                  -- ERROR: NO = DEF.
     Z26 : B := WA2 = WA2;                   -- ERROR: NO = DEF.
     Z27 : B := WA2 /= WA2;                  -- ERROR: NO = DEF.
     Z28 : B := WR2 = WR2;                   -- ERROR: NO = DEF.
     Z29 : B := WR2 /= WR2;                  -- ERROR: NO = DEF.
     Z30 : B := C2 = C2;                     -- ERROR: NO = DEF.
     Z31 : B := C2 /= C2;                    -- ERROR: NO = DEF.
     Z32 : B := WR22.C = WR22.C;             -- ERROR: NO = DEF.
     Z33 : B := WR22.C /= WR22.C;            -- ERROR: NO = DEF.
     Z34 : B := WR22 = WR22;                 -- ERROR: NO = DEF.
     Z325 : B := WR22 /= WR22;               -- ERROR: NO = DEF.
     Z36 : B := C3 = C3;                     -- ERROR: NO = DEF.
     Z37 : B := C3 /= C3;                    -- ERROR: NO = DEF.
     Z38 : B := WR3 = WR3;                   -- ERROR: NO = DEF.
     Z39 : B := WR3 /= WR3;                  -- ERROR: NO = DEF.
     Z40 : B := W4.C(0) = W4.C(0);           -- ERROR: NO = DEF.
     Z41 : B := W4.C(0) /= W4.C(0);          -- ERROR: NO = DEF.
     Z42 : B := W4 = W4;                     -- ERROR: NO = DEF.
     Z43 : B := W4 /= W4;                    -- ERROR: NO = DEF.

BEGIN
     NULL;
END B74201A;
