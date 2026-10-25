-- B95020B0.ADA

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
-- JRK 2/27/84
-- PWN 11/05/95  REMOVED CONFORMANCE CHECKS WHERE RULES RELAXED.
-- PWN 04/09/96  Restored checks in Ada95 legal format.

PACKAGE B95020B0 IS

     SUBTYPE I IS INTEGER RANGE 0..2;
     SUBTYPE I1 IS I;

     V1, V2, V : I := 0;

     PACKAGE P1 IS
          SUBTYPE I2 IS I1;
          W1, W2 : I := 0;
     END P1;
     USE P1;

     WW1 : I RENAMES P1.W1;
     VV1 : I RENAMES V1;

     PACKAGE P3 IS
          SUBTYPE T IS I;
     END P3;
     USE P3;

     PACKAGE NP3 RENAMES P3;

     FUNCTION F1 (L, R : INTEGER) RETURN INTEGER RENAMES STANDARD."+";

     TASK TYPE TASK1 IS

          ENTRY T1A (D : I := V);
          ENTRY T1B (D : I := V);

          ENTRY T2A (D : I := VV1);
          ENTRY T2B (D : I1 := VV1);

          ENTRY T3 (D1, D2 : I; D3 : I);

          ENTRY T4A (D : BOOLEAN := (TRUE = TRUE));
          ENTRY T4B (D : BOOLEAN := (TRUE = TRUE));

          ENTRY T7A (D : I1 := W1);
          ENTRY T7B (D : I := W2);

          ENTRY T8A (D : P1.I2 := P1.W2);

          ENTRY T9A (D : I := 2 + 0);
          ENTRY T9B (D : P1.I2 := P1.W1);
          ENTRY T9C (D : I2 := 2 + 0);

          ENTRY T10 (D : BOOLEAN := FALSE);
          ENTRY T11 (D : BOOLEAN := FALSE);

          ENTRY T12A (D : INTEGER := 100);
          ENTRY T12B (D : INTEGER := 1E2);

          ENTRY T13 (D : INTEGER             -- A COMMENT.
                         := 0                -- ANOTHER COMMENT.
                    );

          ENTRY T14 (D : INTEGER := 3 * 1);
          ENTRY T15 (D : INTEGER := 3 / 1);

          ENTRY T16A (D : T);
          ENTRY T16B (D : NP3.T);
          ENTRY T16C (D : P3.T);
          ENTRY T17 (D : P3.T);

          ENTRY T18 (D : I := F1(1,0));

          ENTRY T19A (D : I := "+"(1,0));
          ENTRY T19B (D : CHARACTER := STANDARD.'A');
          ENTRY T19C (D : CHARACTER := STANDARD.'A');

          ENTRY T20A (D : I);
          ENTRY T20B (D : I := V1);

          ENTRY T21A (D : I);
          ENTRY T21B (D : I := 0);

          ENTRY T22A (D : I);
          ENTRY T22B (D : IN I);
          ENTRY T22C (D : IN I);
          ENTRY T22D (D : IN OUT I);
          ENTRY T22E (D : IN I);
          ENTRY T22F (D : IN OUT I);
          ENTRY T22G (D : OUT I);

     END TASK1;

END B95020B0;
