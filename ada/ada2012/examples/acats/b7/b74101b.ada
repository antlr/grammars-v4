-- B74101B.ADA

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
-- CHECK THAT A FULL TYPE DECLARATION FOR A PRIVATE TYPE CANNOT BE
-- DERIVED FROM A LIMITED PRIVATE GENERIC FORMAL TYPE.

-- JBG 11/18/85
-- PWB 02/20/86  CORRECTED ERROR: DELETED EXTRA 'BEGIN'.

PROCEDURE B74101B IS

     GENERIC
          TYPE LP IS LIMITED PRIVATE;
     PACKAGE P IS
          TYPE P1 IS PRIVATE;
          TYPE P2 IS PRIVATE;
          TYPE P3 IS PRIVATE;
          TYPE P4 IS PRIVATE;
          TYPE P5 IS PRIVATE;
          TYPE P6 IS PRIVATE;
          TYPE P7 IS PRIVATE;
          TYPE P8 IS PRIVATE;
     PRIVATE
          TYPE P1 IS NEW LP;            -- ERROR: LP IS LIM PRIV.

          TYPE ARR IS ARRAY (1..3) OF LP;
          TYPE P2 IS NEW ARR;           -- ERROR: ARR IS LIM PRIV.

          TYPE REC IS
               RECORD
                    C : LP;
               END RECORD;
          TYPE P3 IS NEW REC;           -- ERROR: REC IS LIM PRIV.

          TYPE NLP IS NEW LP;
          TYPE P4 IS NEW NLP;           -- ERROR: NLP IS LIM PRIV.

          TYPE NARR IS NEW ARR;
          TYPE P5 IS NEW NARR;          -- ERROR: NARR IS LIM PRIV.

          TYPE NREC IS NEW REC;
          TYPE P6 IS NEW NREC;          -- ERROR: NREC IS LIM PRIV.

          TYPE RECREC IS
               RECORD
                    C : REC;
               END RECORD;

          TYPE RECARR IS
               RECORD
                    C : ARR;
               END RECORD;

          TYPE P7 IS NEW RECREC;        -- ERROR: LIM PRIV.
          TYPE P8 IS NEW RECARR;        -- ERROR: LIM PRIV.
     END P;

BEGIN
     NULL;
END B74101B;
