-- BC3202C.ADA

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
-- CHECK THAT IF A GENERIC FORMAL TYPE HAS DISCRIMINANTS, THE ACTUAL
-- PARAMETER MUST BE AN UNCONSTRAINED TYPE, HAVE THE SAME NUMBER OF
-- DISCRIMINANTS, AND THE CORRESPONDING DISCRIMINANTS MUST HAVE THE SAME
-- BASE TYPE.

-- CHECK WHEN THE DISCRIMINANT IS A GENERIC FORMAL TYPE DECLARED IN AN
-- ENCLOSING GENERIC UNIT.

-- SPS 7/9/82
-- SPS 2/10/83

PROCEDURE BC3202C IS
     SUBTYPE NATURAL IS INTEGER RANGE 1..INTEGER'LAST;

     GENERIC
          TYPE DESC IS (<>);
     PACKAGE PACK IS

          TYPE REC (C : DESC) IS RECORD NULL; END RECORD;
          TYPE DREC (D : DESC := DESC'FIRST) IS RECORD NULL; END RECORD;
          SUBTYPE CREC IS REC (C => DESC'FIRST);
          TYPE D2REC (A : INTEGER; B : DESC) IS RECORD NULL;
               END RECORD;
          SUBTYPE SREC IS D2REC (A => 3, B => DESC'LAST);
          TYPE RECB (D : BOOLEAN) IS RECORD NULL; END RECORD;
          TYPE ND IS NEW DESC;
          TYPE RECND (D : ND) IS RECORD NULL; END RECORD;

          GENERIC
               TYPE PV (D : DESC) IS PRIVATE;
          PACKAGE PPV IS END PPV;

          GENERIC
               TYPE LP (D : DESC) IS LIMITED PRIVATE;
          PACKAGE PLP IS END PLP;

          GENERIC
               TYPE PV (A : INTEGER; B : DESC) IS PRIVATE;
          PACKAGE GPV IS END GPV;

          GENERIC
               TYPE LP (A : INTEGER; B : DESC) IS LIMITED PRIVATE;
          PACKAGE GLP IS END GLP;

          PACKAGE PPV1 IS NEW PPV(REC);   -- OK.
          PACKAGE PPV2 IS NEW PPV(DREC);  -- OK.
          PACKAGE PPV3 IS NEW PPV(D2REC); -- ERROR: D2REC HAS 2 
                                          -- DISCRIMINANTS.
          PACKAGE PPV4 IS NEW PPV(RECB);  -- ERROR: RECB HAS 
                                          -- BOOLEAN BASE.
          PACKAGE PPV5 IS NEW PPV(RECND); -- ERROR: RECND HAS ND BASE.
          PACKAGE PPV6 IS NEW PPV(CREC);  -- ERROR: CREC IS CONSTRAINED.

          PACKAGE PLP1 IS NEW PLP(REC);   -- OK.
          PACKAGE PLP2 IS NEW PLP(DREC);  -- OK.
          PACKAGE PLP3 IS NEW PLP(RECB);  -- ERROR: RECB HAS
                                          -- BOOLEAN BASE.
          PACKAGE PLP4 IS NEW PLP(RECND); -- ERROR: RECND HAS ND BASE.
          PACKAGE PLP5 IS NEW PLP(D2REC); -- ERROR: D2REC HAS
                                          -- 2 DISCRIMINANTS.
          PACKAGE PLP6 IS NEW PLP(CREC);  -- ERROR: CREC IS CONSTRAINED.

          PACKAGE GPV1 IS NEW GPV(D2REC); -- OK.
          PACKAGE GPV2 IS NEW GPV(REC);   -- ERROR: WRONG NUMBER OF
                                          -- DISCRIMINANTS.
          PACKAGE GPV3 IS NEW GPV(RECB);  -- ERROR: WRONG NUMBER OF
                                          -- DISCRIMINANTS.
          PACKAGE GPV4 IS NEW GPV(SREC);  -- ERROR: SREC IS CONSTRAINED.
     
          PACKAGE GLP1 IS NEW GLP(D2REC); -- OK.
          PACKAGE GLP2 IS NEW GLP(REC);   -- ERROR: WRONG NUMBER OF
                                          -- DISCRIMINANTS.
          PACKAGE GLP3 IS NEW GLP(RECB);  -- ERROR: WRONG NUMBER OF
                                          -- DISCRIMINANTS.
          PACKAGE GLP4 IS NEW GLP(SREC);  -- ERROR: SREC IS CONSTRAINED.

     END PACK;
BEGIN
     NULL;
END BC3202C;
