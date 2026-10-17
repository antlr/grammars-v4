-- B83041E.ADA

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
--     CHECK FOR TYPES DECLARED IN THE VISIBLE PART
--     OF A PACKAGE, THAT IN THE ABSENCE OF A USE
--     CLAUSE OPERATOR SYMBOLS AND ENUMERATION LITERALS
--     ARE NOT DIRECTLY VISIBLE OUTSIDE THE PACKAGE AND
--     THAT EXPLICIT CONVERSIONS AND QUALIFICATIONS ARE
--     ILLEGAL.

-- HISTORY:
--     SDA 09/09/88 CREATED ORIGINAL TEST.
--     PWN 12/01/95 REMOVED CHECKS WHERE VISIBILITY RULES HAVE CHANGED.
--     PWN 03/28/96 Restored checks in Ada95 legal format.

PROCEDURE B83041E IS

     PACKAGE P IS
          TYPE A IS RANGE 1..10;
          PA : A := 1;
          TYPE B IS (MIKE,JACKIE,BELINDA);
          TYPE C IS DIGITS 3 RANGE 0.0 .. 10.0;
          PC : C := 5.0;
          TYPE D IS DELTA 0.5 RANGE 0.0 .. 10.0;
          PD : D := 5.0;
          TYPE E IS ARRAY(INTEGER RANGE <>) OF A;
          TYPE F IS ACCESS B;
          TYPE G IS PRIVATE;
          G1 : CONSTANT G;
          TYPE R IS
               RECORD
                    X : INTEGER;
               END RECORD;
          TYPE NB IS NEW BOOLEAN;
     PRIVATE
          TYPE G IS RANGE 1..5;
          G1 : CONSTANT G := 2;
     END P;

BEGIN
     DECLARE
          A0 : P.A := 3 + 4;      -- ERROR: "+" NOT DEFINED OUTSIDE P
                                  --        FOR TYPE A.
          A1 : P.A := 5 - 4;      -- ERROR: "-" NOT DEFINED OUTSIDE P.
          A2 : P.A := 5 / 5;      -- ERROR: "/" NOT DEFINED OUTSIDE P.
          A3 : P.A := 5 * 1;      -- ERROR: "*" NOT DEFINED OUTSIDE P.
          A4 : P.A := A(5);       -- ERROR: CONVERSION TO A NOT VISIBLE.
          A5 : P.A := A'(5);      -- ERROR: QUALIFICATION WITH A NOT
                                  --        VISIBLE.
          B2 : P.B := B(P.MIKE);  -- ERROR: CONVERSION TO B NOT VISIBLE.
          B3 : P.B := B'(P.MIKE); -- ERROR: QUALIFICATION WITH B NOT
                                  --        VISIBLE.
          B4 : P.B := MIKE;       -- ERROR: ENUMERATION LITERALS NOT
                                  --        VISIBLE.
          C0 : P.C := 3.0 + 4.0;  -- ERROR: "+" NOT DEFINED OUTSIDE P
                                  --        FOR TYPE C.
          C1 : P.C := 5.0 - 4.0;  -- ERROR: "-" NOT DEFINED OUTSIDE P.
          C2 : P.C := P.PC / 5.0; -- ERROR: "/" NOT DEFINED OUTSIDE P.
          C3 : P.C := P.PC * 1.0; -- ERROR: "*" NOT DEFINED OUTSIDE P.
          C4 : P.C := C(5);       -- ERROR: CONVERSION TO C NOT VISIBLE.
          C5 : P.C := C'(5.0);    -- ERROR: QUALIFICATION WITH C NOT
                                  --        VISIBLE.

          D0 : P.D := 3.0 + 4.0;  -- ERROR: "+" NOT DEFINED OUTSIDE P
                                  --        FOR TYPE D.
          D1 : P.D := 5.0 - 4.0;  -- ERROR: "-" NOT DEFINED OUTSIDE P.
          D2 : P.D := P.D'(P.PD / 5.0);                -- OK.
          D3 : P.D := P.D'(P.PD * 1.0);                -- OK.
          D4 : P.D := D(5);       -- ERROR: CONVERSION TO D NOT VISIBLE.
          D5 : P.D := D'(5.0);    -- ERROR: QUALIFICATION WITH D NOT
                                  --        VISIBLE.
          E0 : P.E(1 .. 3);
          E1 : CONSTANT P.E := E0 & E0;    -- ERROR: CATENATION NOT
                                           -- DEFINED OUTSIDE P.
          F1 : P.F;
          G2 : P.G := G(P.G1);     -- ERROR: CONVERSION TO G NOT
                                   --        VISIBLE.
          G3 : P.G := G'(P.G1);    -- ERROR: QUALIFICATION WITH G NOT
                                   --        VISIBLE.
          NB1 : P.NB := P.NB (TRUE) AND P.NB (FALSE); -- ERROR: "AND"
                                                      --   NOT VISIBLE.
          NB2 : P.NB := P.NB (TRUE) OR P.NB (FALSE);  -- ERROR: "OR"
                                                      --   NOT VISIBLE.
          NB3 : P.NB := P.NB (TRUE) XOR P.NB (FALSE); -- ERROR: "XOR"
                                                      --   NOT VISIBLE.
          NB4 : P.NB := NOT P.NB (TRUE);   -- ERROR: "NOT" NOT VISIBLE.
          BOOL1 : BOOLEAN := P.PA = P.PA;  -- ERROR: "=" NOT VISIBLE.
          BOOL2 : BOOLEAN := P.PA >= P.PA; -- ERROR: ">=" NOT VISIBLE.
          BOOL3 : BOOLEAN := P.PA <= P.PA; -- ERROR: "<=" NOT VISIBLE.
     BEGIN
          NULL;
     END;
END B83041E;
