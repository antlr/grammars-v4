-- B38103C1.ADA

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
-- JRK 2/16/84
-- PWN 11/09/95  REMOVED CHECKS WHERE CONFORMANCE RULES RELAXED.
-- PWN 02/16/96  Restored checks in Ada 95 legal format.
-- MRM 12/15/96  Restored infix/prefix check per Ada95 rules

WITH B38103C0; USE B38103C0;

PACKAGE B38103C1 IS

          USE P1;
          USE P3;

PRIVATE

          TYPE T1A (D : I := V);
          TYPE T1B (D : I := V);

          TYPE T2A (D : I := VV1);
          TYPE T2B (D : I1 := VV1);

          TYPE T3 (D1, D2 : I; D3 : I);

          TYPE T4A (D : BOOLEAN := (TRUE = TRUE));
          TYPE T4B (D : BOOLEAN := (TRUE = TRUE));

          TYPE T7A (D : I1 := W1);
          TYPE T7B (D : I := W2);

          TYPE T8A (D : P1.I2 := P1.W2);

          TYPE T9A (D : I := 2 + 0);
          TYPE T9B (D : P1.I2 := P1.W1);
          TYPE T9C (D : I2 := 2 + 0);

          TYPE T10 (D : BOOLEAN := FALSE);
          TYPE T11 (D : BOOLEAN := FALSE);

          TYPE T12A (D : INTEGER := 100);
          TYPE T12B (D : INTEGER := 1E2);

          TYPE T13 (D : INTEGER              -- A COMMENT.
                        := 0                 -- ANOTHER COMMENT.
                   );

          TYPE T14 (D : INTEGER := 3 * 1);
          FUNCTION "*" (L, R : INTEGER) RETURN INTEGER
                       RENAMES STANDARD."*";
          TYPE T15 (D : INTEGER := 3 / 1);
          FUNCTION "/" (L, R : INTEGER) RETURN INTEGER RENAMES "+";

          PACKAGE NP3 RENAMES P3;
          TYPE T16A (D : T);
          TYPE T16B (D : NP3.T);
          TYPE T16C (D : P3.T);
          TYPE T17 (D : P3.T);

          TYPE T18 (D : I := F1(1,0));
          FUNCTION F1 (L, R : INTEGER) RETURN INTEGER
                      RENAMES STANDARD."+";

          TYPE T19A (D : I := "+"(1,0));
          TYPE T19B (D : CHARACTER := STANDARD.'A');
          TYPE T19C (D : CHARACTER := STANDARD.'A');

          TYPE T20A (D : I);
          TYPE T20B (D : I := V1);

          PACKAGE P5 IS
               V1 : I RENAMES B38103C0.V1;
               SUBTYPE I IS B38103C0.I;
          END P5;

          TYPE T21A (D : I);
          TYPE T21B (D : I := 0);

          TYPE T22 (D : I := 0);

          TYPE T23 (D : I := 0);

          TYPE T24 (D : I := 0);

          F : CONSTANT BOOLEAN := FALSE;
          TYPE NONBOOL IS (TRUE, TOO_TRUE);
          FALSE : CONSTANT BOOLEAN := F;

END B38103C1;
