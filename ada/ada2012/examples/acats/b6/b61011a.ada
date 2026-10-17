-- B61011A.ADA

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
-- CHECK THAT A NAME REFERRING TO A FORMAL PARAMETER CANNOT BE USED
-- LATER IN THE SAME FORMAL PART (ALTHOUGH A PARAMETER'S IDENTIFIER CAN
-- BE USED (E.G., AS A SELECTOR) IF IT DOES NOT REFER TO THE PARAMETER).

-- BHS 7/2/84

PROCEDURE B61011A IS

     TYPE REC IS
          RECORD
               COMP : INTEGER;
          END RECORD;

     TYPE ARR IS ARRAY (1..10) OF INTEGER;

     I1 : INTEGER;
     R1 : REC;
     A1 : ARR;

     TYPE T IS
          RECORD
               T : INTEGER;
          END RECORD;

     SUBTYPE TT IS T;
     T_VAR : T;

     FUNCTION FUN (T : INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN T + 1;
     END FUN;

     -- CHECK SUBPROGRAMS USING GLOBAL VARIABLE NAMES AS PARAMETER
     -- NAMES, AS WELL AS IMPLICITLY DECLARED PARAMETER NAMES.

     PROCEDURE P1A (I1 : INTEGER; J : INTEGER := I1) IS   -- ERROR: 
                                                          -- USE OF I1.
     BEGIN
          NULL;
     END P1A;


     PROCEDURE P1B (I2 : INTEGER; J : INTEGER := I2) IS  -- ERROR:
                                                         -- USE OF I2.
     BEGIN
          NULL;
     END P1B;


     PROCEDURE P2A (R1 : REC; C : INTEGER := R1.COMP) IS  -- ERROR: 
                                                          -- USE OF R1.
     BEGIN
          NULL;
     END P2A;


     PROCEDURE P2B (R2 : REC; C : INTEGER := R2.COMP) IS  -- ERROR:
                                                          -- USE OF R2.
     BEGIN
          NULL;
     END P2B;


     PROCEDURE P3A (A1 : ARR; J : INTEGER := A1(1)) IS   -- ERROR:
                                                         -- USE OF A1.
     BEGIN
          NULL;
     END P3A;


     PROCEDURE P3B (A2 : ARR; J : INTEGER := A2(1)) IS   -- ERROR:
                                                         -- USE OF A2.
     BEGIN
          NULL;
     END P3B;


     PROCEDURE P4A (A1 : ARR; J : INTEGER := A1'FIRST) IS  -- ERROR:
                                                           -- USE OF A1.
     BEGIN
          NULL;
     END P4A;


     PROCEDURE P4B (A2 : ARR; J : INTEGER := A2'FIRST) IS  -- ERROR:
                                                           -- USE OF A2.
     BEGIN
          NULL;
     END P4B;


     PROCEDURE P5A (I1 : INTEGER; J : INTEGER := FUN (I1)) IS  -- ERROR:
                                                           -- USE OF I1.
     BEGIN
          NULL;
     END P5A;


     PROCEDURE P5B (I2 : INTEGER; J : INTEGER := FUN (I2)) IS  -- ERROR:
                                                           -- USE OF I2.
     BEGIN
          NULL;
     END P5B;


     PROCEDURE P6 (T : INTEGER;
                   X : TT := (T => 3);              -- LEGAL USE OF T.
                   Y : INTEGER := T_VAR.T;          -- LEGAL USE OF T.
                   Z : INTEGER := FUN (T => 3)) IS  -- LEGAL USE OF T.
     BEGIN
          NULL;
     END P6;


BEGIN

     NULL;

END B61011A;  
