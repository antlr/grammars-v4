-- BC3201C.ADA

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
-- CHECK THAT WHEN A GENERIC UNIT HAS A NON-LIMITED GENERIC FORMAL
-- PRIVATE TYPE, THE ACTUAL PARAMETER MUST NOT BE A LIMITED TYPE.

-- CHECK WHEN THE ACTUAL PARAMETER IS A FORMAL LIMITED PRIVATE TYPE
-- DECLARED IN AN ENCLOSING GENERIC UNIT.

-- SPS 7/8/82
-- SPS 12/10/82

PROCEDURE BC3201C IS
     GENERIC
          TYPE LP IS LIMITED PRIVATE;
          TYPE ARLP IS ARRAY (INTEGER) OF LP;
     PACKAGE PK IS
          GENERIC
               TYPE PV IS PRIVATE;
          PACKAGE P IS END P;

          TYPE NLP IS NEW LP;

          TYPE REC IS RECORD
               C1 : ARLP;
               C2 : LP;
          END RECORD;

          SUBTYPE INT IS INTEGER RANGE 1 .. 10;
          TYPE VREC (D : INT) IS
          RECORD
               CASE D IS
                    WHEN 1..3 =>
                         C1 : ARLP;
                    WHEN 4..9 =>
                         C2 : LP;
                    WHEN OTHERS =>
                         C3 : INTEGER := 3;
               END CASE;
          END RECORD;

          SUBTYPE CVREC IS VREC (D => 10);

          PACKAGE P1 IS NEW P(LP);     -- ERROR: LP.
          PACKAGE P2 IS NEW P(NLP);    -- ERROR: NLP.
          PACKAGE P3 IS NEW P(ARLP);   -- ERROR: ARLP.
          PACKAGE P4 IS NEW P(REC);    -- ERROR: REC.
          PACKAGE P5 IS NEW P(VREC);   -- ERROR: VREC.
          PACKAGE P6 IS NEW P(CVREC);  -- ERROR: CVREC.
     END PK;

BEGIN
     NULL;
END BC3201C;
