-- B33201C.ADA

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
-- CHECK THAT IN A SUBTYPE INDICATION IN A COMPONENT OF A RECORD OR
-- ARRAY TYPE DECLARATION, A RANGE CONSTRAINT IS NOT PERMITTED FOR
-- ARRAY, RECORD, ACCESS, TASK, OR PRIVATE TYPES. INCLUDE A CASE OF
-- A CONSTRAINED ARRAY TYPE DEFINITION IN A GENERIC FORMAL
-- PARAMETER DECLARATION.

-- JRK 4/2/81
-- JWC 10/9/85  RENAMED FROM B33003A.ADA AND DIVIDED INTO FIVE SEPARATE
--              TESTS. EACH TYPE IS NOW TESTED IN A COMPONENT OF A
--              RECORD AND A COMPONENT OF AN ARRAY TYPE DECLARATION.
--              THE TEST OF TASK TYPE WAS ADDED.


PROCEDURE B33201C IS

     TYPE ARR IS ARRAY (NATURAL RANGE <>) OF INTEGER;

     TYPE REC IS
          RECORD
               I : INTEGER;
          END RECORD;

     TYPE ACC IS ACCESS INTEGER;

     PACKAGE PKG IS
          TYPE PRIV IS PRIVATE;
     PRIVATE
          TYPE PRIV IS NEW INTEGER;
     END PKG;
     USE PKG;

     TASK TYPE TSK IS
     END TSK;

     SUBTYPE INT IS INTEGER RANGE 1 .. 2;

     TYPE RT IS
          RECORD
               A : ARR RANGE 0 .. 9;        -- ERROR: RANGE CONSTRAINT
                                            -- ON ARRAY TYPE.
               R : REC RANGE 0 .. 9;        -- ERROR: RANGE CONSTRAINT
                                            -- ON RECORD TYPE.
               C : ACC RANGE 0 .. 9;        -- ERROR: RANGE CONSTRAINT
                                            -- ON ACCESS TYPE.
               P : PRIV RANGE 0 .. 9;       -- ERROR: RANGE CONSTRAINT
                                            -- ON PRIVATE TYPE.
               T : TSK RANGE 0 .. 9;        -- ERROR: RANGE CONSTRAINT
                                            -- ON TASK TYPE.
          END RECORD;

     TYPE ARRARR IS ARRAY (1 .. 2)
                 OF ARR RANGE 0 .. 9;       -- ERROR: RANGE CONSTRAINT
                                            -- ON ARRAY TYPE.
     TYPE ARRREC IS ARRAY (1 .. 2)
                 OF REC RANGE 0 .. 9;       -- ERROR: RANGE CONSTRAINT
                                            -- ON RECORD TYPE.
     TYPE ARRACC IS ARRAY (1 .. 2)
                 OF ACC RANGE 0 .. 9;       -- ERROR: RANGE CONSTRAINT
                                            -- ON ACCESS TYPE.
     TYPE ARRPRIV IS ARRAY (1 .. 2)
                 OF PRIV RANGE 0 .. 9;      -- ERROR: RANGE CONSTRAINT
                                            -- ON PRIVATE TYPE.
     TYPE ARRTSK IS ARRAY (1 .. 2)
                 OF TSK RANGE 0 .. 9;       -- ERROR: RANGE CONSTRAINT
                                            -- ON TASK TYPE.

     GENERIC
          TYPE GENARR IS ARRAY (INT)
                 OF ARR RANGE 0 .. 9;       -- ERROR: RANGE CONSTRAINT
                                            -- ON ARRAY TYPE.
          TYPE GENREC IS ARRAY (INT)
                 OF REC RANGE 0 .. 9;       -- ERROR: RANGE CONSTRAINT
                                            -- ON RECORD TYPE.
          TYPE GENACC IS ARRAY (INT)
                 OF ACC RANGE 0 .. 9;       -- ERROR: RANGE CONSTRAINT
                                            -- ON ACCESS TYPE.
          TYPE GENPRIV IS ARRAY (INT)
                 OF PRIV RANGE 0 .. 9;      -- ERROR: RANGE CONSTRAINT
                                            -- ON PRIVATE TYPE.
          TYPE GENTSK IS ARRAY (INT)
                 OF TSK RANGE 0 .. 9;       -- ERROR: RANGE CONSTRAINT
                                            -- ON TASK TYPE.
     PACKAGE GENPAK IS
     END GENPAK;

     TASK BODY TSK IS
     BEGIN
          NULL;
     END TSK;

BEGIN
     NULL;
END B33201C;
