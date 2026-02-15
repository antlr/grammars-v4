-- B66001A.ADA

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
-- CHECK THAT SUBPROGRAM REDECLARATIONS ARE FORBIDDEN.
-- SUBTESTS ARE:

--        (A) TWO SUBPROGRAMS IN THE SAME DECLARATIVE PART
--             THAT ARE IDENTICAL EXCEPT FOR A PARAMETER NAME.

--        (B) TWO SUBPROGRAMS IN THE SAME DECLARATIVE PART
--             THAT ARE IDENTICAL EXCEPT THAT SUBTYPES OF A PARAMETER
--             ARE DIFFERENT.

--        (C) TWO FUNCTIONS IN THE SAME DECLARATIVE PART
--             THAT ARE THE SAME EXCEPT FOR RESULT SUBTYPES.

--        (D) TWO PROCEDURES IN THE SAME DECLARATIVE PART
--             THAT ARE THE SAME EXCEPT FOR PARAMETER
--             MODES.

--        (E) TWO SUBPROGRAMS IN THE SAME DECLARATIVE PART THAT
--             ARE THE SAME EXCEPT THAT A PARAMETER HAS A
--             DIFFERENT DEFAULT VALUE.

--        (F) TWO SUBPROGRAMS IN THE SAME DECLARATIVE PART THAT ARE
--             THE SAME EXCEPT ONE HAS DEFAULT PARAMETER
--             VALUES.

-- DAS 2/3/81
-- SPS 12/10/82
-- SPS 1/17/83
-- CPP 5/22/84
-- JBG 5/23/85
-- L.BROWN  10/08/86  REMOVED EXTRANEOUS ^M FROM LINES 40 AND 64
-- RLB 11/19/19  Added error location indicators.

PROCEDURE B66001A IS

BEGIN

     --------------------------------------------------

     DECLARE   -- (A)

          PROCEDURE PA (I1 : IN INTEGER; I2 : IN OUT INTEGER;
                        I3 : OUT INTEGER) IS
          BEGIN
               NULL;
          END PA;

          PROCEDURE PA (X1 : IN INTEGER; I2 : IN OUT INTEGER;
                        I3 : OUT INTEGER) IS    -- ERROR:          {1:11}
                                                -- REDECLARES PA.
          BEGIN
               NULL;
          END PA;

          FUNCTION FA (X : INTEGER) RETURN INTEGER IS
          BEGIN
               RETURN 1;
          END FA;

          FUNCTION FA (Y : INTEGER) RETURN INTEGER IS -- ERROR:    {11}
                                                -- REDECLARES FA.
          BEGIN
               RETURN 2;
          END FA;

     BEGIN     -- (A)
          NULL;
     END;      -- (A)

     --------------------------------------------------

     DECLARE   -- (B)

          SUBTYPE INT10 IS INTEGER RANGE 1..10;
          SUBTYPE INT20 IS INTEGER RANGE 1..20;

          PROCEDURE PB (I : INT10) IS
          BEGIN
               NULL;
          END PB;

          PROCEDURE PB (I : INT20) IS           -- ERROR: DUPLICATES PB. {11}
          BEGIN
               NULL;
          END PB;

          PROCEDURE PB (I : INTEGER) IS         -- ERROR: DUPLICATES PB. {11}
          BEGIN
               NULL;
          END PB;

          FUNCTION FB (X : INT10) RETURN INTEGER IS
          BEGIN
               RETURN 1;
          END FB;

          FUNCTION FB (X : INT20) RETURN INTEGER IS -- ERROR:      {11}
                                                -- DUPLICATES FB.
          BEGIN
               RETURN 2;
          END FB;

          FUNCTION FB (X : INTEGER) RETURN INTEGER IS -- ERROR:    {11}
                                                -- DUPLICATES FB.
          BEGIN
               RETURN 3;
          END FB;

     BEGIN     -- (B)
          NULL;
     END;      -- (B)

     --------------------------------------------------

     DECLARE   -- (C)

          SUBTYPE ST IS STRING (1..5);
          SUBTYPE STR IS STRING (1..10);

          FUNCTION FC (X : INTEGER) RETURN ST IS
          BEGIN
               RETURN "12345";
          END FC;

          FUNCTION FC (X : INTEGER) RETURN STR IS -- ERROR:        {11}
                                                -- DUPLICATES FC.
          BEGIN
               RETURN "1234567890";
          END FC;

     BEGIN     -- (C)
          NULL;
     END;      -- (C)

     --------------------------------------------------

     DECLARE   -- (D)

          PROCEDURE PD (X : IN OUT INTEGER) IS
          BEGIN
               NULL;
          END PD;

          PROCEDURE PD (X : OUT INTEGER) IS     -- ERROR: DUPLICATES PD. {11}
          BEGIN
               NULL;
          END PD;

          PROCEDURE PD (X : IN INTEGER) IS      -- ERROR: DUPLICATES PD. {11}
          BEGIN
               NULL;
          END PD;

     BEGIN    -- (D)
          NULL;
     END;      -- (D)

     --------------------------------------------------

     DECLARE   -- (E)

          PROCEDURE PE (X : IN INTEGER := 1) IS
          BEGIN
               NULL;
          END PE;

          PROCEDURE PE (X : IN INTEGER := 2) IS -- ERROR: DUPLICATES PE. {11}
          BEGIN
               NULL;
          END PE;

          FUNCTION FE (X : INTEGER := 1) RETURN INTEGER IS
          BEGIN
               RETURN 1;
          END FE;

          FUNCTION FE (X : INTEGER := 2) RETURN INTEGER IS -- ERROR:   {11}
                                                -- DUPLICATES FE.
          BEGIN
               RETURN 2;
          END FE;

     BEGIN     -- (E)
          NULL;
     END;      -- (E)

     --------------------------------------------------

     DECLARE   -- (F)

          PROCEDURE P (I1 : INTEGER; S1 : STRING := "LMN";
                       B1 : IN OUT BOOLEAN) IS
          BEGIN
               NULL;
          END;

          PROCEDURE P ( I1 : INTEGER; S1 : STRING;     -- ERROR:    {11}
                                                       -- DUPLICATE P.
                        B1 : IN OUT BOOLEAN) IS
          BEGIN
               NULL;
          END;

     BEGIN     -- (F)
          NULL;
     END;      -- (F)

     --------------------------------------------------

END B66001A;
