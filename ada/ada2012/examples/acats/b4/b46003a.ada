-- B46003A.ADA

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
--     CHECK THAT IF THE TARGET TYPE OF A CONVERSION IS A NUMERIC TYPE,
--     THE OPERAND TYPE CANNOT BE AN ENUMERATION TYPE, AN ARRAY TYPE OR
--     RECORD TYPE WITH NUMERIC COMPONENTS, AN ACCESS TYPE, OR A
--     PRIVATE TYPE WHOSE FULL DECLARATION DECLARES A NUMERIC TYPE.

-- HISTORY:
--     JET 07/07/88  CREATED ORIGINAL TEST.

PROCEDURE B46003A IS
     TYPE NUM1 IS NEW INTEGER RANGE -100 .. 100;
     TYPE NUM2 IS DELTA 2#0.01# RANGE -16#80.0# .. 16#80.0#;
     SUBTYPE NUM3 IS FLOAT RANGE -100.0 .. 100.0;

     TYPE ENUM IS (WE, LOVE, WRITING, TESTS);

     TYPE ARR IS ARRAY (1..10) OF NUM1;

     TYPE REC IS RECORD
          F : NUM2;
     END RECORD;

     TYPE ACC IS ACCESS NUM3;

     PACKAGE P IS
          TYPE PRIV IS PRIVATE;
          ZERO : CONSTANT PRIV;
     PRIVATE
          TYPE PRIV IS NEW FLOAT;
          ZERO : CONSTANT PRIV := 0.0;
     END P;

     N1 : NUM1;
     N2 : NUM2;
     N3 : NUM3;
     N4 : INTEGER;
     N5 : FLOAT;

     EN : ENUM := WE;
     AR : ARR := (OTHERS => 0);
     RC : REC := (F => 0.0);
     AX : ACC := NEW NUM3'(0.0);
     PV : P.PRIV := P.ZERO;

BEGIN
     N1 := NUM1(AR);                      -- ERROR: ILLEGAL CONVERSION.
     N2 := NUM2(RC);                      -- ERROR: ILLEGAL CONVERSION.
     N3 := NUM3(AX);                      -- ERROR: ILLEGAL CONVERSION.
     N4 := INTEGER(LOVE);                 -- ERROR: ILLEGAL CONVERSION.
     N4 := INTEGER(EN);                   -- ERROR: ILLEGAL CONVERSION.
     N5 := FLOAT(P.ZERO);                 -- ERROR: ILLEGAL CONVERSION.
     N5 := FLOAT(PV);                     -- ERROR: ILLEGAL CONVERSION.
END B46003A;
