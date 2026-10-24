-- B95004B.ADA

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
-- CHECK THAT AN ACCEPT STATEMENT MAY NOT APPEAR IN A LOCAL
--    SUBPROGRAM, PACKAGE, OR TASK OF A TASK BODY.

-- JRK 10/26/81

PROCEDURE B95004B IS

     TASK T IS
          ENTRY E0;
          ENTRY E1 (I : INTEGER);
          ENTRY E2 (BOOLEAN) (I : INTEGER);
     END T;

     TASK BODY T IS

          PROCEDURE P IS
          BEGIN
               ACCEPT E0;                         -- ERROR: NOT LOCAL.
          END P;

          FUNCTION F RETURN INTEGER IS
          BEGIN
               ACCEPT E1 (I : INTEGER);           -- ERROR: NOT LOCAL.
               RETURN 0;
          END F;

          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               ACCEPT E2 (TRUE) (I : INTEGER);    -- ERROR: NOT LOCAL.
          END PKG;

          TASK T1 IS
               ENTRY E;
          END T1;

          TASK BODY T1 IS
          BEGIN
               ACCEPT E;                          -- OK.
               ACCEPT E0;                         -- ERROR: NOT LOCAL.
          END T1;

     BEGIN

          ACCEPT E0;                              -- OK;

          DECLARE

               PROCEDURE P IS
               BEGIN
                    ACCEPT E0;                    -- ERROR: NOT LOCAL.
               END P;

               FUNCTION F RETURN INTEGER IS
               BEGIN
                    ACCEPT E1 (I : INTEGER);      -- ERROR: NOT LOCAL.
                    RETURN 0;
               END F;

               PACKAGE PKG IS
               END PKG;

               PACKAGE BODY PKG IS
               BEGIN
                    ACCEPT E2 (TRUE) (I : INTEGER); -- ERROR: NOT LOCAL.
               END PKG;

               TASK T1 IS
                    ENTRY E;
               END T1;

               TASK BODY T1 IS
               BEGIN
                    ACCEPT E;                     -- OK.
                    ACCEPT E0;                    -- ERROR: NOT LOCAL.
               END T1;

          BEGIN
               ACCEPT E1 (I : INTEGER);           -- OK.
               ACCEPT E2 (TRUE) (I : INTEGER);    -- OK.
          END;

     END T;

BEGIN
     NULL;
END B95004B;
