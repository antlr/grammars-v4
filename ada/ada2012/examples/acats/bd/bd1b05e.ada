-- BD1B05E.ADA

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
--     CHECK THAT AN ENUMERATION REPRESENTATION CLAUSE FOR T CANNOT
--     FOLLOW A QUALIFIED EXPRESSION, AN EXPLICIT TYPE
--     CONVERSION, OR MEMBERSHIP TEST IF THE TYPE MARK
--     IS: A SUBTYPE OF T; A RECORD OR ARRAY TYPE WITH A
--     (SUB)COMPONENT OF A SUBTYPE OF T; AN ARRAY TYPE WITH AN INDEX
--     SUBTYPE OF A SUBTYPE OF T.

-- HISTORY:
--     DHH 06/09/88 CREATED ORIGINAL TEST.
--     BCB 03/13/90 CHANGED "--  ERROR:" TO "-- ERROR:".
--     JRL 11/17/95 Removed allocator case (legal in Ada95).
--     PWN 03/28/96 Restored allocator case in Ada95 legal format.

PROCEDURE BD1B05E IS

     INT : INTEGER := 0;
----------------------- QUALIFIED EXPRESSIONS -----------------------

     PACKAGE ENCLOSE IS
     END ENCLOSE;

     PACKAGE BODY ENCLOSE IS
     BEGIN

          DECLARE   -- SUBTYPE
               TYPE T IS (A, B, C);

               PACKAGE P IS
                    SUBTYPE SUB_T IS T;
                    SUBTYPE PT IS T RANGE SUB_T'(A) .. SUB_T'(B);
               END P;
               USE P;
               FOR T USE (A => 1, B =>2, C => 3);             -- ERROR:
          BEGIN
               NULL;
          END;   -- SUBTYPE

          DECLARE   -- RECORD COMPONENT
               TYPE T1 IS (A, B, C);

               PACKAGE P1 IS
                    SUBTYPE SUB_T1 IS T1;

                    TYPE REC IS
                         RECORD
                              Z : SUB_T1;
                         END RECORD;

                    FUNCTION USELESS1(Y : REC := REC'(Z => A))
                                                        RETURN INTEGER;

               END P1;
               USE P1;
               FOR T1 USE (A => 1, B =>2, C => 3);           -- ERROR:

               PACKAGE BODY P1 IS
                    FUNCTION USELESS1(Y : REC := REC'(Z => A))
                                                      RETURN INTEGER IS
                    BEGIN
                         RETURN 1;
                    END USELESS1;
               END P1;

          BEGIN
               NULL;
          END;   -- RECORD COMPONENT

          DECLARE   -- RECORD SUBCOMPONENT
               TYPE T2 IS (A, B, C);

               PACKAGE P2 IS
                    SUBTYPE SUB_T2 IS T2;

                    TYPE EC IS
                         RECORD
                              Z : SUB_T2;
                         END RECORD;

                    TYPE REC1 IS
                         RECORD
                              Z : EC;
                         END RECORD;

                    FUNCTION USELESS2(Y : REC1 := REC1'(Z => (Z => A)))
                                                        RETURN INTEGER;

               END P2;
               USE P2;

               FOR T2 USE (A => 1, B =>2, C => 3);           -- ERROR:

               PACKAGE BODY P2 IS
                    FUNCTION USELESS2(Y : REC1 := REC1'(Z => (Z => A)))
                                                      RETURN INTEGER IS
                    BEGIN
                         RETURN 1;
                    END USELESS2;
               END P2;

          BEGIN
               NULL;
          END;   -- RECORD SUBCOMPONENT

          DECLARE   -- ARRAY COMPONENT
               TYPE T3 IS (A, B, C);

               PACKAGE P3 IS
                    SUBTYPE SUB_T3 IS T3;

                    TYPE ARR IS ARRAY(1 .. 5) OF SUB_T3;

                    FUNCTION USELESS3(Y : ARR := ARR'(1 .. 5 => (A)))
                                                        RETURN INTEGER;
               END P3;
               USE P3;

               FOR T3 USE (A => 1, B =>2, C => 3);           -- ERROR:

               PACKAGE BODY P3 IS
                    FUNCTION USELESS3(Y : ARR := ARR'(1 .. 5 => (A)))
                                                      RETURN INTEGER IS
                    BEGIN
                         RETURN 1;
                    END USELESS3;
               END P3;

          BEGIN
               NULL;
          END;   -- ARRAY COMPONENT

          DECLARE      -- ARRAY SUBCOMPONENT
               TYPE T4 IS (A, B, C);

               PACKAGE P4 IS
                    SUBTYPE SUB_T4 IS T4;

                    TYPE ARR4 IS ARRAY(1 .. 5) OF SUB_T4;
                    TYPE ARY IS ARRAY(1 .. 2) OF ARR4;

                    FUNCTION USELESS4(Y : ARY := ARY'(1 .. 2 =>
                                     (1 .. 5 => (A)))) RETURN INTEGER;
               END P4;
               USE P4;

               FOR T4 USE (A => 1, B =>2, C => 3);           -- ERROR:

               PACKAGE BODY P4 IS
                    FUNCTION USELESS4(Y : ARY := ARY'(1 .. 2 =>
                                   (1 .. 5 => (A)))) RETURN INTEGER IS
                    BEGIN
                         RETURN 1;
                    END USELESS4;
               END P4;

          BEGIN
               NULL;
          END;   --   ARRAY SUBCOMPONENT

          DECLARE      -- ARRAY INDEX
               TYPE TA IS (A, B, C);

               PACKAGE PA IS
                    SUBTYPE SUB_TA IS TA;

                    TYPE ARRA IS ARRAY(SUB_TA) OF INTEGER;

                    FUNCTION USELESS5(Y : ARRA := ARRA'(A .. C => (3)))
                                                        RETURN INTEGER;
               END PA;
               USE PA;

               FOR TA USE (A => 1, B =>2, C => 3);          -- ERROR:

               PACKAGE BODY PA IS
                    FUNCTION USELESS5(Y : ARRA := ARRA'(A .. C => (3)))
                                                      RETURN INTEGER IS
                    BEGIN
                         RETURN 1;
                    END USELESS5;
               END PA;

          BEGIN
               NULL;
          END;   -- ARRAY INDEX

----------------------- EXPLICIT TYPE CONVERSION -----------------------

          DECLARE   -- SUBTYPE
               TYPE T IS (A, B, C);

               PACKAGE P IS
                    SUBTYPE SUB_T IS T;

                    FUNCTION USELESS(Y : T := SUB_T(A))
                                                      RETURN INTEGER;
               END P;
               USE P;
               FOR T USE (A => 1, B =>2, C => 3);             -- ERROR:

               PACKAGE BODY P IS
                    FUNCTION USELESS(Y : T := SUB_T(A))
                                                      RETURN INTEGER IS
                    BEGIN
                         RETURN 1;
                    END USELESS;
               END P;

          BEGIN
               NULL;
          END;   -- SUBTYPE

------------------------------ ALLOCATION ------------------------------

           DECLARE   -- RECORD COMPONENT
               TYPE T1 IS (A, B, C);

               PACKAGE P1 IS
                    SUBTYPE SUB_T1 IS T1;

                    TYPE REC IS
                         RECORD
                              Z : SUB_T1;
                         END RECORD;

                    TYPE PTR IS ACCESS REC;

                    FUNCTION USELESS1(Y : PTR := NEW REC)
                                                        RETURN INTEGER;

               END P1;
               USE P1;
               FOR T1 USE (A => 1, B =>2, C => 3);           -- OK.

               PACKAGE BODY P1 IS
                    FUNCTION USELESS1(Y : PTR := NEW REC)
                                                      RETURN INTEGER IS
                    BEGIN
                         RETURN 1;
                    END USELESS1;
               END P1;

          BEGIN
               NULL;
          END;   -- RECORD COMPONENT

------------------------------ MEMBERSHIP ------------------------------

          DECLARE   -- ARRAY COMPONENT
               TYPE T3 IS (A, B, C);

               PACKAGE P3 IS
                    SUBTYPE SUB_T3 IS T3;

                    TYPE ARR IS ARRAY(1 .. 5) OF SUB_T3;

                    FUNCTION USELESS3(Y : BOOLEAN := ((1 .. 5 => (A))
                                               IN ARR)) RETURN INTEGER;
               END P3;
               USE P3;

               FOR T3 USE (A => 1, B =>2, C => 3);           -- ERROR:

               PACKAGE BODY P3 IS
                    FUNCTION USELESS3(Y : BOOLEAN := ((1 .. 5 => (A))
                                             IN ARR)) RETURN INTEGER IS
                    BEGIN
                         RETURN 1;
                    END USELESS3;
               END P3;

          BEGIN
               NULL;
          END;   -- ARRAY COMPONENT

     END ENCLOSE;

BEGIN
     NULL;
END BD1B05E;
