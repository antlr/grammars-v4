-- B38103E0M.ADA

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
-- CHECK THAT IF AN INCOMPLETE TYPE IS DECLARED WITH DISCRIMINANTS, THE
-- COMPLETE DECLARATION, WHICH IS IN A PACKAGE BODY SUBUNIT IN ANOTHER
-- COMPILATION FILE, MUST HAVE THE SAME DISCRIMINANT PART, THE
-- CASE OF LETTERS IN STRING LITERALS USED AS OPERATOR SYMBOLS, AND THE
-- PRESENCE OF COMMENTS.

-- IN THIS TEST THE INCOMPLETE DECLARATION IS IN A PACKAGE PRIVATE PART 
-- AND THE FULL DECLARATION IS IN THE CORRESPONDING PACKAGE BODY, WHICH 
-- IS SEPARATELY COMPILED.

-- CASES:
--   (A)  USE OF SIMPLE VS. EXPANDED NAMES.
--   (B)  MISUSE OF RENAMED ENTITIES.
--   (D)  USE OF DIFFERENT FORMS OF INTEGER LITERALS.
--   (E)  MISUSE OF SIMPLE NAMES DENOTING DIFFERENT FUNCTIONS OR
--        OBJECTS.
--   (F)  USE OF STRING LITERALS AS OPERATOR SYMBOLS VS. MISUSE OF
--        STRING LITERALS AS SOMETHING OTHER THAN OPERATOR SYMBOLS.
--        (TESTED ELSEWHERE).
--   (G)  USE/MISUSE OF INFIX VS. FUNCTIONAL FORM OF OPERATOR INVOCATION;
--        MISUSE OF COMMUTATIVITY OF CERTAIN OPERATORS.
--   (I)  USE/MISUSE OF EXPANDED VS. EXPANDED NAMES.
--   (J)  MISUSE OF PRAGMAS (TESTED ELSEWHERE).
--   (K)  USE OF COMMENTS.
--   (L)  MISUSE OF (DIFFERENT) DISCRIMINANT NAMES.
--   (M)  MISUSE OF LEXICAL FORM OF EXPRESSIONS, E.G., EXTRA PARENTHESES
--        OR QUALIFICATION.
--   (N)  MISUSE OF SAME OPERATORS WITH DIFFERENT MEANINGS.
--   (O)  USE/MISUSE OF SIMPLE VS. EXPANDED NAMES FOR OPERATOR SYMBOLS AND
--        CHARACTER LITERALS.
--   (P)  USE/MISUSE OF EXPANDED NAMES ASSOCIATED WITH RENAMING OR SUBTYPE
--        DECLARATIONS.
--   (Q)  MISUSE OF PRESENCE/ABSENCE OF DEFAULT EXPRESSIONS.
--   (R)  MISUSE OF DERIVED RECORD TYPES WITH DISCRIMINANTS TO SUPPLY
--        MISSING DISCRIMINANT PARTS.
--   (S)  MISUSE OF RECORD TYPES WITH DISCRIMINANT-LIKE COMPONENTS TO
--        SUPPLY MISSING DISCRIMINANT PARTS.
--   (T)  MISUSE OF DERIVED RECORD TYPES.

-- OTHER UNIT:  B38103E1 - THE PACKAGE BODY.
-- THIS UNIT SHOULD COMPILE WITH NO ERRORS.

-- AH  8/27/86
-- PWN 11/09/95  REMOVED CHECKS WHERE CONFORMANCE RULES RELAXED.
-- PWN 02/16/96  Restored checks in Ada 95 legal format.
-- MRM 12/15/96  Restored infix/prefix check per Ada95 rules
PROCEDURE B38103E0M IS

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

     FUNCTION F1 (L, R : INTEGER) RETURN INTEGER RENAMES STANDARD."+";

     TYPE RD (D : I := 0) IS
          RECORD NULL; END RECORD;

     TYPE R IS
          RECORD NULL; END RECORD;

     PACKAGE B38103E1 IS

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
               V1 : I RENAMES B38103E0M.V1;
               SUBTYPE I IS B38103E0M.I;
          END P5;

          TYPE T21A (D : I);
          TYPE T21B (D : I := 0);

          TYPE T22 (D : I := 0);

          TYPE T23 (D : I := 0);

          TYPE T24 (D : I := 0);

          F : CONSTANT BOOLEAN := FALSE;
          TYPE NONBOOL IS (TRUE, TOO_TRUE);
          FALSE : CONSTANT BOOLEAN := F;

     END B38103E1;

     PACKAGE BODY B38103E1 IS SEPARATE;

BEGIN
     NULL;
END B38103E0M;
