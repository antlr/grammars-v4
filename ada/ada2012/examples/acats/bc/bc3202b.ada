-- BC3202B.ADA

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

-- CHECK WHEN THE DISCRIMINANT TYPE IS A GENERIC FORMAL TYPE.

-- SPS 7/9/82
-- JRL 11/30/95  Removed Ada95-incompatible cases (corresponding discriminant
--               subtypes must statically match).
-- PWN 03/28/96  Restored checks in Ada95 legal format.

PROCEDURE BC3202B IS
     TYPE NI IS NEW INTEGER;
     SUBTYPE NATURAL IS INTEGER RANGE 1..INTEGER'LAST;

     TYPE REC (C : INTEGER) IS RECORD NULL; END RECORD;
     TYPE DREC (D : INTEGER :=3) IS RECORD NULL; END RECORD;
     SUBTYPE CREC IS REC (C => 3);
     TYPE NREC (D : NATURAL) IS RECORD NULL; END RECORD;
     TYPE NDREC IS RECORD NULL; END RECORD;
     TYPE D2REC (A : INTEGER; B : BOOLEAN) IS RECORD NULL;
          END RECORD;
     SUBTYPE SREC IS D2REC (A => 3, B => TRUE);
     TYPE RECB (D : BOOLEAN) IS RECORD NULL; END RECORD;
     TYPE RECNI (D : NI) IS RECORD NULL; END RECORD;

     GENERIC
          TYPE DESC IS (<>);
          TYPE PV (D : DESC) IS PRIVATE;
          TYPE LP (D : DESC) IS LIMITED PRIVATE;
     PACKAGE PP IS END PP;

     GENERIC
          TYPE D1 IS (<>);
          TYPE D2 IS (<>);
          TYPE PV (A : D1; B : D2) IS PRIVATE;
          TYPE LP (A : D1; B : D2) IS LIMITED PRIVATE;
     PACKAGE GP IS END GP;

     PACKAGE PP1 IS NEW PP(INTEGER, REC, REC);   -- OK.
     PACKAGE PP2 IS NEW PP(INTEGER, DREC, DREC); -- OK.
     PACKAGE PP3 IS NEW PP(INTEGER, NREC, NREC); -- ERROR: NREC MUST STATICLY
                                                 -- MATCH.
     PACKAGE PP4 IS NEW PP(INTEGER, REC, NDREC); -- ERROR: NDREC NO 
                                                 -- DISCRIMINANT.
     PACKAGE PP5 IS NEW PP(INTEGER, REC, D2REC); -- ERROR: D2REC 2 
                                                 -- DISCRIMINANTS.
     PACKAGE PP6 IS NEW PP(INTEGER, REC, RECB);  -- ERROR: BOOLEAN BASE.
     PACKAGE PP7 IS NEW PP(INTEGER, REC, RECNI); -- ERROR: NI BASE.
     PACKAGE PP8 IS NEW PP(INTEGER, REC, CREC);  -- ERROR: CREC IS 
                                                 -- CONSTRAINED.
     PACKAGE PP9 IS NEW PP(INTEGER, RECB, REC);  -- ERROR: RECB HAS 
                                                 -- BOOLEAN BASE.
     PACKAGE PP10 IS NEW PP(INTEGER, RECNI, REC);-- ERROR: NI BASE.
     PACKAGE PP11 IS NEW PP(INTEGER, NDREC, REC);-- ERROR: 
                                                 -- NO DISCRIMINANT.
     PACKAGE PP12 IS NEW PP(INTEGER, D2REC, REC);-- ERROR: 
                                                 -- 2 DISCRIMINANTS.
     PACKAGE PP13 IS NEW PP(INTEGER, CREC, REC); -- ERROR: CREC IS 
                                                 -- CONSTRAINED.

     PACKAGE GP1 IS NEW GP(INTEGER, BOOLEAN,
                           D2REC,  D2REC);       -- OK.
     PACKAGE GP2 IS NEW GP(INTEGER, BOOLEAN, 
                           D2REC,  REC);         -- ERROR: WRONG NUMBER 
                                                 -- OF DISCRIMINANTS.
     PACKAGE GP3 IS NEW GP(INTEGER, BOOLEAN, 
                           D2REC,  RECB);        -- ERROR: WRONG NUMBER 
                                                 -- OF DISCRIMINANTS.
     PACKAGE GP4 IS NEW GP(INTEGER, BOOLEAN, 
                           D2REC,  SREC);        -- ERROR: SREC IS 
                                                 -- CONSTRAINED.

     PACKAGE GP5 IS NEW GP(INTEGER, BOOLEAN, 
                           REC,  D2REC);         -- ERROR: WRONG NUMBER 
                                                 -- OF DISCRIMINANTS.
     PACKAGE GP6 IS NEW GP(INTEGER, BOOLEAN, 
                           RECB,  D2REC);        -- ERROR: WRONG NUMBER 
                                                 -- OF DISCRIMINANTS.
     PACKAGE GP7 IS NEW GP(INTEGER, BOOLEAN, 
                           SREC,  D2REC);        -- ERROR: SREC IS 
                                                 -- CONSTRAINED.

BEGIN
     NULL;
END BC3202B;
