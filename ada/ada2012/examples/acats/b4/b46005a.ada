-- B46005A.ADA

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
--     CHECK THAT IF THE TARGET TYPE OF A CONVERSION IS AN ENUMERATION
--     TYPE, A RECORD TYPE, AN ACCESS TYPE, OR A PRIVATE TYPE, THE
--     OPERAND TYPE CANNOT BE A DIFFERENT ENUMERATION TYPE, RECORD
--     TYPE, ACCESS TYPE, OR PRIVATE TYPE UNRELATED BY DERIVATION.

-- HISTORY:
--     JET 07/07/88  CREATED ORIGINAL TEST.

PROCEDURE B46005A IS
     TYPE ENUM1 IS (WE, LOVE, WRITING, TESTS);
     TYPE ENUM2 IS (JOHN, LYNN, BRIAN, VINCE);
     TYPE ENUMD IS NEW ENUM1 RANGE LOVE..TESTS;

     TYPE REC1 IS RECORD
          F : INTEGER;
     END RECORD;

     TYPE REC2 IS RECORD
          F : INTEGER;
     END RECORD;

     TYPE RECD IS NEW REC1;

     TYPE ACC1 IS ACCESS INTEGER;
     TYPE ACC2 IS ACCESS INTEGER;
     TYPE ACCD IS NEW ACC1;

     PACKAGE P IS
          TYPE PRIV1 IS PRIVATE;
          TYPE PRIV2 IS PRIVATE;
          ZERO : CONSTANT PRIV1;
          ONE : CONSTANT PRIV2;
     PRIVATE
          TYPE PRIV1 IS RANGE -100 .. 100;
          TYPE PRIV2 IS RANGE -100 .. 100;
          ZERO : CONSTANT PRIV1 := 0;
          ONE : CONSTANT PRIV2 := 1;
     END P;

     TYPE PRIVD IS NEW P.PRIV1;

     E1 : ENUM1 := WE;
     R1 : REC1 := (F => 0);
     A1 : ACC1 := NEW INTEGER'(0);
     P1 : P.PRIV1 := P.ZERO;
     E2 : ENUM2 := JOHN;
     R2 : REC2 := (F => 1);
     A2 : ACC2 := NEW INTEGER'(1);
     P2 : P.PRIV2 := P.ONE;
     ED : ENUMD := LOVE;
     RD : RECD := (F => 2);
     AD : ACCD := NEW INTEGER'(2);
     PD : PRIVD;

BEGIN
     E1 := ENUM1(ED);                    -- OK.
     E1 := ENUM1(E2);                    -- ERROR: ILLEGAL CONVERSION.
     R1 := REC1(RD);                     -- OK.
     R1 := REC1(R2);                     -- ERROR: ILLEGAL CONVERSION.
     AD := ACCD(A1);                     -- OK.
     AD := ACCD(A2);                     -- ERROR: ILLEGAL CONVERSION.
     PD := PRIVD(P1);                    -- OK.
     PD := PRIVD(P2);                    -- ERROR: ILLEGAL CONVERSION.
END B46005A;
