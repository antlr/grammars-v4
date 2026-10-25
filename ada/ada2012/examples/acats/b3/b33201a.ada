-- B33201A.ADA

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
-- CHECK THAT IN A SUBTYPE INDICATION IN AN ACCESS TYPE DEFINITION,
-- A RANGE CONSTRAINT IS NOT PERMITTED FOR ARRAY, RECORD, ACCESS,
-- TASK, OR PRIVATE TYPES. INCLUDE ACCESS TYPE DEFINITIONS IN GENERIC
-- FORMAL PARAMETER DECLARATIONS.

-- JRK 4/2/81
-- JWC 10/9/85  RENAMED FROM B33003A.ADA AND DIVIDED INTO FIVE SEPARATE
--              TESTS. EACH TYPE IS NOW TESTED IN AN ACCESS TYPE
--              DEFINITION.  THE TEST OF TASK TYPE WAS ADDED.

PROCEDURE B33201A IS

     TYPE ARR IS ARRAY (NATURAL RANGE <>) OF INTEGER;

     TYPE REC IS
          RECORD
               I : INTEGER;
          END RECORD;

     TYPE ACC IS ACCESS INTEGER;

     PACKAGE PKG IS
          TYPE PRV IS PRIVATE;
     PRIVATE
          TYPE PRV IS NEW INTEGER;
     END PKG;
     USE PKG;

     TASK TYPE TSK IS
     END TSK;

     TYPE AA IS ACCESS ARR RANGE 0 .. 9;      -- ERROR: RANGE CONSTRAINT
                                              -- ON ARRAY TYPE.
     TYPE AR IS ACCESS REC RANGE 0 .. 9;      -- ERROR: RANGE CONSTRAINT
                                              -- ON RECORD TYPE.
     TYPE AC IS ACCESS ACC RANGE 0 .. 9;      -- ERROR: RANGE CONSTRAINT
                                              -- ON ACCESS TYPE.
     TYPE AP IS ACCESS PRV RANGE 0 .. 9;      -- ERROR: RANGE CONSTRAINT
                                              -- ON PRIVATE TYPE.
     TYPE ATSK IS ACCESS TSK RANGE 0 .. 9;    -- ERROR: RANGE CONSTRAINT
                                              -- ON TASK TYPE.
     GENERIC
          TYPE GA IS ACCESS ARR RANGE 0 .. 9; -- ERROR: RANGE CONSTRAINT
                                              -- ON ARRAY TYPE.
          TYPE GR IS ACCESS REC RANGE 0 .. 9; -- ERROR: RANGE CONSTRAINT
                                              -- ON RECORD TYPE.
          TYPE GC IS ACCESS ACC RANGE 0 .. 9; -- ERROR: RANGE CONSTRAINT
                                              -- ON ACCESS TYPE.
          TYPE GP IS ACCESS PRV RANGE 0 .. 9; -- ERROR: RANGE CONSTRAINT
                                              -- ON PRIVATE TYPE.
          TYPE GT IS ACCESS TSK RANGE 0 .. 9; -- ERROR: RANGE CONSTRAINT
                                              -- ON TASK TYPE.
     PACKAGE GENPCK IS
     END GENPCK;

     TASK BODY TSK IS
     BEGIN
          NULL;
     END TSK;

BEGIN
     NULL;
END B33201A;
