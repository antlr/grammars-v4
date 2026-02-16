-- B54B02D.ADA

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
-- CHECK THAT WHEN A CASE EXPRESSION IS A GENERIC IN OUT PARAMETER
-- THE OTHERS ALTERNATIVE CAN BE OMITTED IF
-- ALL VALUES IN THE BASE TYPE'S RANGE ARE COVERED.
-- CHECK THAT IF ONE OR MORE OF THE BASE TYPE'S VALUES ARE OMITTED,
-- THE OTHERS ALTERNATIVE MUST NOT BE OMITTED.

--     SPS 05/05/82
--     JRK 02/02/83
--     RLB 02/08/18 ADDED ERROR LOCATION INDICATORS TO REFLECT COMMON
--                  ERROR REPORTING STRATEGIES.

PROCEDURE B54B02D IS

     SUBTYPE STATIC IS INTEGER RANGE 1 .. 5;

     PROCEDURE P (N : IN STATIC) IS
          SUBTYPE NONSTAT IS STATIC RANGE 1 .. N;
          SUBTYPE NS IS NONSTAT RANGE 1 .. 2;
          GENERIC
               V1 : IN OUT STATIC; -- SUBTYPE OF GENERIC IN OUT FORMAL
                                   -- PARAMETER IS THAT OF ACTUAL
                                   -- PARAMETER; HENCE SUBTYPE IS
                                   -- NONSTATIC.
               V2 : IN OUT NONSTAT;
               V3 : IN OUT NS;
          PROCEDURE PROC;

          PROCEDURE PROC IS
          BEGIN
               CASE V1 IS
                    WHEN INTEGER'FIRST .. INTEGER'LAST => NULL;
               END CASE;                          -- OK. {2:16;1}

               CASE V1 IS
                    WHEN  1 .. 4 => NULL;
                    WHEN OTHERS  => NULL;         -- OK. {2:16;1}
               END CASE;

               CASE V1 IS
                    WHEN 1 .. 5 => NULL;
               END CASE;                          -- ERROR: {2:16;1} OTHERS
                                                  --                 REQUIRED.

               CASE V1 IS
                    WHEN INTEGER'FIRST .. INTEGER'POS(5) => NULL;
               END CASE;                          -- ERROR: {2:16;1} OTHERS
                                                  --                 REQUIRED.

               CASE V1 IS
                    WHEN 1 .. 5 => NULL;
                    WHEN OTHERS => NULL;
               END CASE;                          -- OK. {3:16;1}

               CASE V2 IS
                    WHEN INTEGER'FIRST .. INTEGER'LAST => NULL;
               END CASE;                          -- OK. {2:16;1}

               CASE V2 IS
                    WHEN INTEGER'FIRST .. INTEGER'POS(2) => NULL;
               END CASE;                          -- ERROR: {2:16;1} OTHERS
                                                  --                 REQUIRED.

               CASE V2 IS
                    WHEN 1 .. 5 => NULL;
               END CASE;                          -- ERROR: {2:16;1} OTHERS
                                                  --                 REQUIRED.

               CASE V2 IS
                    WHEN INTEGER'FIRST .. INTEGER'POS(2) => NULL;
                    WHEN OTHERS => NULL;
               END CASE;                          -- OK. {3:16;1}

               CASE V3 IS
                    WHEN INTEGER'FIRST .. INTEGER'LAST => NULL;
               END CASE;                          -- OK. {2:16;1}

               CASE V3 IS
                    WHEN 1 .. 2 => NULL;
               END CASE;                          -- ERROR: {2:16;1} OTHERS
                                                  --                 REQUIRED.

               CASE V3 IS
                    WHEN 1 .. 2 => NULL;
                    WHEN OTHERS => NULL;
               END CASE;                          -- OK. {3:16;1}
          END;

     BEGIN
          NULL;
     END;

BEGIN
     NULL;
END B54B02D;
