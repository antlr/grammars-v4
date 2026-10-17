-- B83E01B.ADA

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
--     CHECK THAT GENERIC SUBPROGRAM SPECIFICATIONS AND GENERIC FORMAL
--     SUBPROGRAM SPECIFICATIONS CANNOT CONTAIN DUPLICATE FORMAL
--     PARAMETERS.

-- HISTORY:
--     DHH 09/13/88  CREATED ORIGINAL TEST.
--     RLB 02/14/18  REMOVED A POTENTAL ERROR CASCADE. REMOVED AND SPLIT SOME
--                   TEST CASES TO AVOID ERROR CONFLICTS. ADDED ERROR LOCATION
--                   INDICATORS.

PROCEDURE B83E01B IS

     GENERIC
     FUNCTION F(PARAM1 : BOOLEAN;
                PARAM1 : INTEGER) RETURN BOOLEAN;-- ERROR: {1:17;1} DUPLICATE.

     GENERIC
     PROCEDURE P(PARAM1 : OUT BOOLEAN;
                 PARAM1 : IN INTEGER);           -- ERROR: {1:18;1} DUPLICATE.

     GENERIC
          TYPE PARAM1 IS (<>);
          TYPE PARAM1 IS ARRAY(INTEGER RANGE <>) -- ERROR: {1:11} DUPLICATE.
                                      OF BOOLEAN;
     FUNCTION F1(PARAM2 : BOOLEAN;
                 PARAM3 : INTEGER) RETURN BOOLEAN;

     GENERIC
          TYPE PARAM1 IS (<>);
          TYPE PARAM1 IS ARRAY(INTEGER RANGE <>) -- ERROR: {1:11} DUPLICATE.
                                      OF BOOLEAN;
     PROCEDURE P1(PARAM3 : BOOLEAN;
                  PARAM2 : INTEGER);

     GENERIC
          TYPE PARAM1 IS (<>);
     FUNCTION F2(PARAM2 : BOOLEAN;
                 PARAM1 : INTEGER) RETURN BOOLEAN;-- ERROR: {2:11;1} DUPLICATE.

     GENERIC
          TYPE PARAM1 IS (<>);
     PROCEDURE P2(PARAM1 : IN BOOLEAN;           -- ERROR: {1:11;1} DUPLICATE.
                  PARAM2 : OUT INTEGER);

     GENERIC
          PARAM1 : IN OUT INTEGER;
          PARAM1 : IN BOOLEAN := TRUE;           -- ERROR: {1:11;1} DUPLICATE.
          PARAM1 : IN INTEGER;                   -- ERROR: {11;1} DUPLICATE.
     FUNCTION F3(PARAM2 : BOOLEAN;
                 PARAM3 : INTEGER) RETURN BOOLEAN;

     GENERIC
          PARAM1 : IN OUT INTEGER;
          PARAM1 : IN BOOLEAN := TRUE;           -- ERROR: {1:11;1} DUPLICATE.
          PARAM1 : IN OUT INTEGER;               -- ERROR: {11;1} DUPLICATE.
     PROCEDURE P3(PARAM3 : IN BOOLEAN;
                  PARAM2 : OUT INTEGER);

     GENERIC
          TYPE PARAM1 IS (<>);
          PARAM1 : IN INTEGER;                   -- ERROR: {1:11;1} DUPLICATE.
     FUNCTION F4(PARAM2 : BOOLEAN;
                 PARAM3 : INTEGER) RETURN BOOLEAN;

     GENERIC
          PARAM1 : IN OUT INTEGER;
          TYPE PARAM1 IS (<>);                   -- ERROR: {1:11;1} DUPLICATE.
     PROCEDURE P4(PARAM3 : IN BOOLEAN;
                  PARAM2 : OUT INTEGER);

     GENERIC
          WITH FUNCTION SUM(PARAM3 : INTEGER;
                            PARAM4 : INTEGER;
                            PARAM3 : INTEGER;    -- ERROR: {2:29;1} DUPLICATE.
                            PARAM5 : INTEGER;
                            PARAM6 : BOOLEAN) RETURN INTEGER;
          WITH PROCEDURE ADD(PARAM3 : INTEGER;
                             PARAM4 : INTEGER;
                             PARAM3 : INTEGER;   -- ERROR: {2:30;1} DUPLICATE.
                             PARAM5 : INTEGER;
                             PARAM3 : BOOLEAN);  -- ERROR: {30;2} DUPLICATE.
     FUNCTION F5(PARAM2 : BOOLEAN;
                 PARAM3 : INTEGER) RETURN BOOLEAN;

     GENERIC
          WITH FUNCTION SUM(PARAM3 : INTEGER;
                            PARAM4 : INTEGER;
                            PARAM3 : INTEGER;    -- ERROR: {2:29;1} DUPLICATE.
                            PARAM5 : INTEGER;
                            PARAM3 : BOOLEAN)    -- ERROR: {29;1} DUPLICATE.
                                             RETURN INTEGER;
          WITH PROCEDURE ADD(PARAM3 : INTEGER;
                             PARAM4 : INTEGER;
                             PARAM1 : INTEGER;
                             PARAM5 : INTEGER;
                             PARAM3 : BOOLEAN);  -- ERROR: {4:30;2} DUPLICATE.
     PROCEDURE P5(PARAM3 : IN BOOLEAN;
                  PARAM2 : OUT INTEGER);

     PROCEDURE P(PARAM1 : OUT BOOLEAN;
                 PARAM1 : IN INTEGER) IS         -- OPTIONAL ERROR: {1:6}
     BEGIN
          NULL;
     END;

     PROCEDURE P1(PARAM3 : BOOLEAN;
                  PARAM2 : INTEGER) IS
     BEGIN
          NULL;
     END;

     PROCEDURE P2(PARAM1 : IN BOOLEAN;
                  PARAM2 : OUT INTEGER) IS       -- OPTIONAL ERROR: {1:6}
     BEGIN
          PARAM2 := 1;
     END;

     PROCEDURE P3(PARAM3 : IN BOOLEAN;
                  PARAM2 : OUT INTEGER) IS
     BEGIN
          PARAM2 := 1;
     END;

     PROCEDURE P4(PARAM3 : IN BOOLEAN;
                  PARAM2 : OUT INTEGER) IS
     BEGIN
          PARAM2 := 1;
     END;

     PROCEDURE P5(PARAM3 : IN BOOLEAN;
                  PARAM2 : OUT INTEGER) IS
     BEGIN
          PARAM2 := 1;
     END;

     FUNCTION F(PARAM1 : BOOLEAN;
                PARAM1 : INTEGER) RETURN BOOLEAN IS  -- OPTIONAL ERROR: {1:6}
     BEGIN
          RETURN TRUE;
     END F;

     FUNCTION F1(PARAM2 : BOOLEAN;
                 PARAM3 : INTEGER) RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END;

     FUNCTION F2(PARAM2 : BOOLEAN;
                 PARAM1 : INTEGER) RETURN BOOLEAN IS -- OPTIONAL ERROR: {1:6}
     BEGIN
          RETURN TRUE;
     END;

     FUNCTION F3(PARAM2 : BOOLEAN;
                 PARAM3 : INTEGER) RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END;

     FUNCTION F4(PARAM2 : BOOLEAN;
                 PARAM3 : INTEGER) RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END;

     FUNCTION F5(PARAM2 : BOOLEAN;
                 PARAM3 : INTEGER) RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END;

BEGIN
     NULL;
END B83E01B;
