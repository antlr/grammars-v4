-- BC3201A.ADA

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
-- CHECK THAT IN A GENERIC INSTANTIATION, A GENERIC ACTUAL TYPE
-- PARAMETER MUST NOT BE A LIMITED PRIVATE TYPE (OR A TYPE WITH
-- LIMITED PRIVATE COMPONENTS) IF THE GENERIC FORMAL IS A NONLIMITED 
-- PRIVATE TYPE PARAMETER.

-- ASL 8/24/81
-- SPS 7/8/82
-- SPS 12/10/82

PROCEDURE BC3201A IS

     PACKAGE P IS
          TYPE LIM IS LIMITED PRIVATE;
     PRIVATE
          TYPE LIM IS (X,Y,Z);
     END P;

     USE P;

     TYPE LIMREC IS
          RECORD
               COMP : LIM;
          END RECORD;
     TYPE VARREC (D:INTEGER ) IS
          RECORD
               CASE D IS
               WHEN 1..2 =>
                    C1 : LIM;
               WHEN OTHERS =>
                    C2 : INTEGER;
               END CASE;
          END RECORD;
     SUBTYPE CVARREC IS VARREC (D=>3);

     TYPE LIMARR IS ARRAY(INTEGER RANGE 1..2) OF LIM;

     GENERIC
          TYPE GFT IS PRIVATE;
     PACKAGE TEMPLATE IS
     END TEMPLATE;
    
     PACKAGE INST1 IS NEW TEMPLATE(LIM);     -- ERROR: LIM.
     PACKAGE INST2 IS NEW TEMPLATE(LIMREC);  -- ERROR: LIMREC.
     PACKAGE INST3 IS NEW TEMPLATE(LIMARR);  -- ERROR: LIMARR.
     PACKAGE INST4 IS NEW TEMPLATE(CVARREC); -- ERROR: CVARREC.
     PACKAGE INST5 IS NEW TEMPLATE(VARREC);  -- ERROR: VARREC.

BEGIN
     NULL;
END BC3201A;
