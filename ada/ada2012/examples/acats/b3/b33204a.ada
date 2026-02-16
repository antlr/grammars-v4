-- B33204A.ADA

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
--     CHECK THAT IN A SUBTYPE INDICATION IN AN ACCESS TYPE DEFINITION,
--     AN INDEX CONSTRAINT IS NOT ALLOWED IF THE TYPE MARK DENOTES
--     AN ENUMERATION, INTEGER, FLOATING POINT, CONSTRAINED ARRAY,
--     RECORD, CONSTRAINED ACCESS, TASK OR PRIVATE TYPE.  INCLUDE ACCESS
--     TYPE DEFINITIONS IN GENERIC FORMAL PARAMETER DECLARATIONS.

-- HISTORY:
--     BCB 01/20/88  CREATED ORIGINAL TEST.

PROCEDURE B33204A IS

     TYPE FX IS DELTA 1.0 RANGE 0.0 .. 5.0;

     TYPE E IS (E1, E2);

     TYPE I IS RANGE 0 .. 100;

     TYPE FL IS DIGITS 3;

     TYPE AR IS ARRAY (NATURAL RANGE <>) OF FX;
     SUBTYPE SAR IS AR(1..10);

     TYPE R IS
          RECORD
              FR : AR(1..10);
          END RECORD;

     TYPE AR10 IS ARRAY(1..10) OF INTEGER;
     TYPE TAR10 IS ACCESS AR10;

     TYPE AC1 IS ACCESS AR10;

     TYPE AC2 IS ACCESS AR(1..10);

     TASK TYPE TK IS
          ENTRY E(1..10);
     END TK;

     PACKAGE PKG IS
          TYPE P IS PRIVATE;
     PRIVATE
          TYPE P IS ARRAY(1..10,1..10) OF FX;
     END PKG;
     USE PKG;

     TYPE T1 IS ACCESS E(0..1);          -- ERROR: INDEX CONSTRAINT
                                         -- ON ENUMERATION TYPE.
     TYPE T2 IS ACCESS I(1..10);         -- ERROR: INDEX CONSTRAINT
                                         -- ON INTEGER TYPE.
     TYPE T3 IS ACCESS FL(1..10);        -- ERROR: INDEX CONSTRAINT
                                         -- ON FLOATING POINT TYPE.
     TYPE T4 IS ACCESS SAR(1..10);       -- ERROR: INDEX CONSTRAINT
                                         -- ON CONSTRAINED ARRAY TYPE.
     TYPE T5 IS ACCESS R(1..10);         -- ERROR: INDEX CONSTRAINT
                                         -- ON RECORD TYPE.
     TYPE T6 IS ACCESS AC1(1..10);       -- ERROR: INDEX CONSTRAINT
                                         -- ON CONSTRAINED ACCESS TYPE.
     TYPE T7 IS ACCESS TK(1..10);        -- ERROR: INDEX CONSTRAINT
                                         -- ON TASK TYPE.
     TYPE T8 IS ACCESS P(1..10,1..10);   -- ERROR: INDEX CONSTRAINT
                                         -- ON PRIVATE TYPE.

     GENERIC
          TYPE GT1 IS
               ACCESS E(0..1);           -- ERROR: INDEX CONSTRAINT
                                         -- ON ENUMERATION TYPE.
          TYPE GT2 IS
               ACCESS I(1..10);          -- ERROR: INDEX CONSTRAINT
                                         -- ON INTEGER TYPE.
          TYPE GT3 IS
               ACCESS FL(1..10);         -- ERROR: INDEX CONSTRAINT
                                         -- ON FLOATING POINT TYPE.
          TYPE GT4 IS
               ACCESS SAR(1..10);        -- ERROR: INDEX CONSTRAINT
                                         -- ON CONSTRAINED ARRAY TYPE.
          TYPE GT5 IS
               ACCESS R(1..10);          -- ERROR: INDEX CONSTRAINT
                                         -- ON RECORD TYPE.
          TYPE GT6 IS
               ACCESS AC1(1..10);        -- ERROR: INDEX CONSTRAINT
                                         -- ON CONSTRAINED ACCESS TYPE.
          TYPE GT7 IS
               ACCESS TK(1..10);         -- ERROR: INDEX CONSTRAINT
                                         -- ON TASK TYPE.
          TYPE GT8 IS
               ACCESS P(1..10,1..10);    -- ERROR: INDEX CONSTRAINT
                                         -- ON PRIVATE TYPE.
     PACKAGE GENPCK IS
     END GENPCK;

     TASK BODY TK IS
     BEGIN
          NULL;
     END TK;

BEGIN
     NULL;
END B33204A;
